-- |
-- Module      : Test.DejaFu.SCT.Internal
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Internal types and functions for dynamic partial-order
-- reduction. This module is NOT considered to form part of the public
-- interface of this library.
module Test.DejaFu.SCT.Internal where

import           Control.DeepSeq      (NFData(..))
import           Control.Exception    (MaskingState(..))
import           Data.Char            (ord)
import qualified Data.Foldable        as F
import           Data.Function        (on)
import           Data.List            (intercalate, nubBy, partition, sortOn)
import           Data.List.NonEmpty   (NonEmpty(..), toList)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (catMaybes, fromJust, isJust, isNothing,
                                       listToMaybe)
import           Data.Sequence        (Seq, (|>))
import qualified Data.Sequence        as Sq
import           Data.Set             (Set)
import qualified Data.Set             as S
import           System.Random        (RandomGen, randomR)

import           Test.DejaFu.Common
import           Test.DejaFu.Schedule (Decision(..), Scheduler, decisionOf,
                                       tidOf)

-------------------------------------------------------------------------------
-- * Dynamic partial-order reduction

-- | DPOR execution is represented as a tree of states, characterised
-- by the decisions that lead to that state.
data DPOR = DPOR
  { dporRunnable :: Set ThreadId
  -- ^ What threads are runnable at this step.
  , dporTodo     :: Map ThreadId Bool
  -- ^ Follow-on decisions still to make, and whether that decision
  -- was added conservatively due to the bound.
  , dporDone     :: Map ThreadId DPOR
  -- ^ Follow-on decisions that have been made.
  , dporSleep    :: Map ThreadId ThreadAction
  -- ^ Transitions to ignore (in this node and children) until a
  -- dependent transition happens.
  , dporTaken    :: Map ThreadId ThreadAction
  -- ^ Transitions which have been taken, excluding
  -- conservatively-added ones. This is used in implementing sleep
  -- sets.
  , dporAction   :: Maybe ThreadAction
  -- ^ What happened at this step. This will be 'Nothing' at the root,
  -- 'Just' everywhere else.
  } deriving (Eq, Show)

instance NFData DPOR where
  rnf dpor = rnf ( dporRunnable dpor
                 , dporTodo     dpor
                 , dporDone     dpor
                 , dporSleep    dpor
                 , dporTaken    dpor
                 , dporAction   dpor
                 )

-- | One step of the execution, including information for backtracking
-- purposes. This backtracking information is used to generate new
-- schedules.
data BacktrackStep = BacktrackStep
  { bcktThreadid   :: ThreadId
  -- ^ The thread running at this step
  , bcktDecision   :: Decision
  -- ^ What was decided at this step.
  , bcktAction     :: ThreadAction
  -- ^ What happened at this step.
  , bcktRunnable   :: Map ThreadId Lookahead
  -- ^ The threads runnable at this step
  , bcktBacktracks :: Map ThreadId Bool
  -- ^ The list of alternative threads to run, and whether those
  -- alternatives were added conservatively due to the bound.
  , bcktState      :: DepState
  -- ^ Some domain-specific state at this point.
  } deriving (Eq, Show)

instance NFData BacktrackStep where
  rnf bs = rnf ( bcktThreadid   bs
               , bcktDecision   bs
               , bcktAction     bs
               , bcktRunnable   bs
               , bcktBacktracks bs
               , bcktState      bs
               )

-- | Initial DPOR state, given an initial thread ID. This initial
-- thread should exist and be runnable at the start of execution.
initialState :: DPOR
initialState = DPOR
  { dporRunnable = S.singleton initialThread
  , dporTodo     = M.singleton initialThread False
  , dporDone     = M.empty
  , dporSleep    = M.empty
  , dporTaken    = M.empty
  , dporAction   = Nothing
  }

-- | Produce a new schedule prefix from a @DPOR@ tree. If there are no new
-- prefixes remaining, return 'Nothing'. Also returns whether the
-- decision was added conservatively, and the sleep set at the point
-- where divergence happens.
--
-- A schedule prefix is a possibly empty sequence of decisions that
-- have already been made, terminated by a single decision from the
-- to-do set. The intent is to put the system into a new state when
-- executed with this initial sequence of scheduling decisions.
findSchedulePrefix
  :: (ThreadId -> Bool)
  -- ^ Some partitioning function, applied to the to-do decisions. If
  -- there is an identifier which passes the test, it will be used,
  -- rather than any which fail it. This allows a very basic way of
  -- domain-specific prioritisation between otherwise equal choices,
  -- which may be useful in some cases.
  -> DPOR
  -> Maybe ([ThreadId], Bool, Map ThreadId ThreadAction)
findSchedulePrefix predicate = listToMaybe . go where
  go dpor =
    let prefixes = here dpor : map go' (M.toList $ dporDone dpor)
    in case concatPartition (\(t:_,_,_) -> predicate t) prefixes of
         ([], choices) -> choices
         (choices, _)  -> choices

  go' (tid, dpor) = (\(ts,c,slp) -> (tid:ts,c,slp)) <$> go dpor

  -- Prefix traces terminating with a to-do decision at this point.
  here dpor = [([t], c, sleeps dpor) | (t, c) <- M.toList $ dporTodo dpor]

  -- The new sleep set is the union of the sleep set of the node we're
  -- branching from, plus all the decisions we've already explored.
  sleeps dpor = dporSleep dpor `M.union` dporTaken dpor

-- | Add a new trace to the tree, creating a new subtree branching off
-- at the point where the \"to-do\" decision was made.
incorporateTrace
  :: MemType
  -- ^ Memory model
  -> Bool
  -- ^ Whether the \"to-do\" point which was used to create this new
  -- execution was conservative or not.
  -> Trace
  -- ^ The execution trace: the decision made, the runnable threads,
  -- and the action performed.
  -> DPOR
  -> DPOR
incorporateTrace memtype conservative trace dpor0 = grow initialDepState (initialDPORThread dpor0) trace dpor0 where
  grow state tid trc@((d, _, a):rest) dpor =
    let tid'   = tidOf tid d
        state' = updateDepState state tid' a
    in case M.lookup tid' (dporDone dpor) of
         Just dpor' ->
           let done = M.insert tid' (grow state' tid' rest dpor') (dporDone dpor)
           in dpor { dporDone = done }
         Nothing ->
           let taken = M.insert tid' a (dporTaken dpor)
               sleep = dporSleep dpor `M.union` dporTaken dpor
               done  = M.insert tid' (subtree state' tid' sleep trc) (dporDone dpor)
           in dpor { dporTaken = if conservative then dporTaken dpor else taken
                   , dporTodo  = M.delete tid' (dporTodo dpor)
                   , dporDone  = done
                   }
  grow _ _ [] _ = err "incorporateTrace" "trace exhausted without reading a to-do point!"

  -- Construct a new subtree corresponding to a trace suffix.
  subtree state tid sleep ((_, _, a):rest) =
    let state' = updateDepState state tid a
        sleep' = M.filterWithKey (\t a' -> not $ dependent memtype state' tid a t a') sleep
    in DPOR
        { dporRunnable = S.fromList $ case rest of
            ((_, runnable, _):_) -> map fst runnable
            [] -> []
        , dporTodo     = M.empty
        , dporDone     = M.fromList $ case rest of
          ((d', _, _):_) ->
            let tid' = tidOf tid d'
            in  [(tid', subtree state' tid' sleep' rest)]
          [] -> []
        , dporSleep = sleep'
        , dporTaken = case rest of
          ((d', _, a'):_) -> M.singleton (tidOf tid d') a'
          [] -> M.empty
        , dporAction = Just a
        }
  subtree _ _ _ [] = err "incorporateTrace" "subtree suffix empty!"

-- | Produce a list of new backtracking points from an execution
-- trace. These are then used to inform new \"to-do\" points in the
-- @DPOR@ tree.
--
-- Two traces are passed in to this function: the first is generated
-- from the special DPOR scheduler, the other from the execution of
-- the concurrent program.
--
-- If the trace ends with any threads other than the initial one still
-- runnable, a dependency is imposed between this final action and
-- everything else.
findBacktrackSteps
  :: MemType
  -- ^ Memory model.
  -> BacktrackFunc
  -- ^ Backtracking function. Given a list of backtracking points, and
  -- a thread to backtrack to at a specific point in that list, add
  -- the new backtracking points. There will be at least one: this
  -- chosen one, but the function may add others.
  -> Bool
  -- ^ Whether the computation was aborted due to no decisions being
  -- in-bounds.
  -> Seq (NonEmpty (ThreadId, Lookahead), [ThreadId])
  -- ^ A sequence of threads at each step: the nonempty list of
  -- runnable threads (with lookahead values), and the list of threads
  -- still to try. The reason for the two separate lists is because
  -- the threads chosen to try will be dependent on the specific
  -- domain.
  -> Trace
  -- ^ The execution trace.
  -> [BacktrackStep]
findBacktrackSteps memtype backtrack boundKill = go initialDepState S.empty initialThread [] . F.toList where
  -- Walk through the traces one step at a time, building up a list of
  -- new backtracking points.
  go state allThreads tid bs ((e,i):is) ((d,_,a):ts) =
    let tid' = tidOf tid d
        state' = updateDepState state tid' a
        this = BacktrackStep
          { bcktThreadid   = tid'
          , bcktDecision   = d
          , bcktAction     = a
          , bcktRunnable   = M.fromList . toList $ e
          , bcktBacktracks = M.fromList $ map (\i' -> (i', False)) i
          , bcktState      = state'
          }
        bs' = doBacktrack killsEarly allThreads' (toList e) (bs++[this])
        runnable = S.fromList (M.keys $ bcktRunnable this)
        allThreads' = allThreads `S.union` runnable
        killsEarly = null ts && boundKill
    in go state' allThreads' tid' bs' is ts
  go _ _ _ bs _ _ = bs

  -- Find the prior actions dependent with this one and add
  -- backtracking points.
  doBacktrack killsEarly allThreads enabledThreads bs =
    let tagged = reverse $ zip [0..] bs
        idxs   = [ (head is, False, u)
                 | (u, n) <- enabledThreads
                 , v <- S.toList allThreads
                 , u /= v
                 , let is = idxs' u n v tagged
                 , not $ null is]

        idxs' u n v = catMaybes . go' True where
          {-# INLINE go' #-}
          go' final ((i,b):rest)
            -- Don't cross subconcurrency boundaries
            | isSubC final b = []
            -- If this is the final action in the trace and the
            -- execution was killed due to nothing being within bounds
            -- (@killsEarly == True@) assume worst-case dependency.
            | bcktThreadid b == v && (killsEarly || isDependent b) = Just i : go' False rest
            | otherwise = go' False rest
          go' _ [] = []

          {-# INLINE isSubC #-}
          isSubC final b = case bcktAction b of
            Stop -> not final && bcktThreadid b == initialThread
            Subconcurrency -> bcktThreadid b == initialThread
            _ -> False

          {-# INLINE isDependent #-}
          isDependent b = dependent' memtype (bcktState b) (bcktThreadid b) (bcktAction b) u n
    in backtrack bs idxs

-- | Add new backtracking points, if they have not already been
-- visited, fit into the bound, and aren't in the sleep set.
incorporateBacktrackSteps
  :: ([(Decision, ThreadAction)] -> (Decision, Lookahead) -> Bool)
  -- ^ Bound function: returns true if that schedule prefix terminated
  -- with the lookahead decision fits within the bound.
  -> [BacktrackStep]
  -- ^ Backtracking steps identified by 'findBacktrackSteps'.
  -> DPOR
  -> DPOR
incorporateBacktrackSteps bv = go Nothing [] where
  go priorTid pref (b:bs) bpor =
    let bpor' = doBacktrack priorTid pref b bpor
        tid   = bcktThreadid b
        pref' = pref ++ [(bcktDecision b, bcktAction b)]
        child = go (Just tid) pref' bs . fromJust $ M.lookup tid (dporDone bpor)
    in bpor' { dporDone = M.insert tid child $ dporDone bpor' }
  go _ _ [] bpor = bpor

  doBacktrack priorTid pref b bpor =
    let todo' = [ x
                | x@(t,c) <- M.toList $ bcktBacktracks b
                , let decision  = decisionOf priorTid (dporRunnable bpor) t
                , let lahead = fromJust . M.lookup t $ bcktRunnable b
                , bv pref (decision, lahead)
                , t `notElem` M.keys (dporDone bpor)
                , c || M.notMember t (dporSleep bpor)
                ]
    in bpor { dporTodo = dporTodo bpor `M.union` M.fromList todo' }

-------------------------------------------------------------------------------
-- * DPOR scheduler

-- | The scheduler state
data DPORSchedState = DPORSchedState
  { schedSleep     :: Map ThreadId ThreadAction
  -- ^ The sleep set: decisions not to make until something dependent
  -- with them happens.
  , schedPrefix    :: [ThreadId]
  -- ^ Decisions still to make
  , schedBPoints   :: Seq (NonEmpty (ThreadId, Lookahead), [ThreadId])
  -- ^ Which threads are runnable at each step, and the alternative
  -- decisions still to make.
  , schedIgnore    :: Bool
  -- ^ Whether to ignore this execution or not: @True@ if the
  -- execution is aborted due to all possible decisions being in the
  -- sleep set, as then everything in this execution is covered by
  -- another.
  , schedBoundKill :: Bool
  -- ^ Whether the execution was terminated due to all decisions being
  -- out of bounds.
  , schedDepState  :: DepState
  -- ^ State used by the dependency function to determine when to
  -- remove decisions from the sleep set.
  } deriving (Eq, Show)

instance NFData DPORSchedState where
  rnf s = rnf ( schedSleep     s
              , schedPrefix    s
              , schedBPoints   s
              , schedIgnore    s
              , schedBoundKill s
              , schedDepState  s
              )

-- | Initial DPOR scheduler state for a given prefix
initialDPORSchedState :: Map ThreadId ThreadAction
  -- ^ The initial sleep set.
  -> [ThreadId]
  -- ^ The schedule prefix.
  -> DPORSchedState
initialDPORSchedState sleep prefix = DPORSchedState
  { schedSleep     = sleep
  , schedPrefix    = prefix
  , schedBPoints   = Sq.empty
  , schedIgnore    = False
  , schedBoundKill = False
  , schedDepState  = initialDepState
  }

-- | A bounding function takes the scheduling decisions so far and a
-- decision chosen to come next, and returns if that decision is
-- within the bound.
type BoundFunc
  = [(Decision, ThreadAction)] -> (Decision, Lookahead) -> Bool

-- | A backtracking step is a point in the execution where another
-- decision needs to be made, in order to explore interesting new
-- schedules. A backtracking /function/ takes the steps identified so
-- far and a list of points and thread at that point to backtrack
-- to. More points be added to compensate for the effects of the
-- bounding function. For example, under pre-emption bounding a
-- conservative backtracking point is added at the prior context
-- switch. The bool is whether the point is conservative. Conservative
-- points are always explored, whereas non-conservative ones might be
-- skipped based on future information.
--
-- In general, a backtracking function should identify one or more
-- backtracking points, and then use @backtrackAt@ to do the actual
-- work.
type BacktrackFunc
  = [BacktrackStep] -> [(Int, Bool, ThreadId)] -> [BacktrackStep]

-- | Add a backtracking point. If the thread isn't runnable, add all
-- runnable threads. If the backtracking point is already present,
-- don't re-add it UNLESS this would make it conservative.
backtrackAt
  :: (ThreadId -> BacktrackStep -> Bool)
  -- ^ If this returns @True@, backtrack to all runnable threads,
  -- rather than just the given thread.
  -> BacktrackFunc
backtrackAt toAll bs0 = backtrackAt' . nubBy ((==) `on` fst') . sortOn fst' where
  fst' (x,_,_) = x

  backtrackAt' ((i,c,t):is) = go i bs0 i c t is
  backtrackAt' [] = bs0

  go i0 (b:bs) 0 c tid is
    -- If the backtracking point is already present, don't re-add it,
    -- UNLESS this would force it to backtrack (it's conservative)
    -- where before it might not.
    | not (toAll tid b) && tid `M.member` bcktRunnable b =
      let val = M.lookup tid $ bcktBacktracks b
          b' = if isNothing val || (val == Just False && c)
            then b { bcktBacktracks = backtrackTo tid c b }
            else b
      in b' : case is of
        ((i',c',t'):is') -> go i' bs (i'-i0-1) c' t' is'
        [] -> bs
    -- Otherwise just backtrack to everything runnable.
    | otherwise =
      let b' = b { bcktBacktracks = backtrackAll c b }
      in b' : case is of
        ((i',c',t'):is') -> go i' bs (i'-i0-1) c' t' is'
        [] -> bs
  go i0 (b:bs) i c tid is = b : go i0 bs (i-1) c tid is
  go _ [] _ _ _ _ = err "backtrackAt" "ran out of schedule whilst backtracking!"

  -- Backtrack to a single thread
  backtrackTo tid c = M.insert tid c . bcktBacktracks

  -- Backtrack to all runnable threads
  backtrackAll c = M.map (const c) . bcktRunnable

-- | DPOR scheduler: takes a list of decisions, and maintains a trace
-- including the runnable threads, and the alternative choices allowed
-- by the bound-specific initialise function.
--
-- After the initial decisions are exhausted, this prefers choosing
-- the prior thread if it's (1) still runnable and (2) hasn't just
-- yielded. Furthermore, threads which /will/ yield are ignored in
-- preference of those which will not.
dporSched
  :: MemType
  -- ^ Memory model.
  -> BoundFunc
  -- ^ Bound function: returns true if that schedule prefix terminated
  -- with the lookahead decision fits within the bound.
  -> Scheduler DPORSchedState
dporSched memtype inBound trc prior threads s = schedule where
  -- Pick a thread to run.
  schedule = case schedPrefix s of
    -- If there is a decision available, make it
    (d:ds) -> (Just d, (nextState []) { schedPrefix = ds })

    -- Otherwise query the initialise function for a list of possible
    -- choices, filter out anything in the sleep set, and make one of
    -- them arbitrarily (recording the others).
    [] ->
      let choices  = restrictToBound initialise
          checkDep t a = case prior of
            Just (tid, act) -> dependent memtype (schedDepState s) tid act t a
            Nothing -> False
          ssleep'  = M.filterWithKey (\t a -> not $ checkDep t a) $ schedSleep s
          choices' = filter (`notElem` M.keys ssleep') choices
          signore' = not (null choices) && all (`elem` M.keys ssleep') choices
          sbkill'  = not (null initialise) && null choices
      in case choices' of
            (nextTid:rest) -> (Just nextTid, (nextState rest) { schedSleep = ssleep' })
            [] -> (Nothing, (nextState []) { schedIgnore = signore', schedBoundKill = sbkill' })

  -- The next scheduler state
  nextState rest = s
    { schedBPoints  = schedBPoints s |> (threads, rest)
    , schedDepState = nextDepState
    }
  nextDepState = let ds = schedDepState s in maybe ds (uncurry $ updateDepState ds) prior

  -- Pick a new thread to run, not considering bounds. Choose the
  -- current thread if available and it hasn't just yielded, otherwise
  -- add all runnable threads.
  initialise = tryDaemons . yieldsToEnd $ case prior of
    Just (tid, act)
      | not (didYield act) && tid `elem` tids -> [tid]
    _ -> tids'

  -- If one of the chosen actions will kill the computation, and there
  -- are daemon threads, try them instead.
  --
  -- This is necessary if the killing action is NOT dependent with
  -- every other action, according to the dependency function. This
  -- is, strictly speaking, wrong; an action that kills another thread
  -- is definitely dependent with everything in that thread. HOWEVER,
  -- implementing it that way leads to an explosion of schedules
  -- tried. Really, all that needs to happen is for the
  -- thread-that-would-be-killed to be executed fully ONCE, and then
  -- the normal dependency mechanism will identify any other
  -- backtracking points that should be tried. This is achieved by
  -- adding every thread that would be killed to the to-do list.
  -- Furthermore, these threads MUST be ahead of the killing thread,
  -- or the killing thread will end up in the sleep set and so the
  -- killing action not performed. This is, again, because of the lack
  -- of the dependency messing things up in the name of performance.
  --
  -- See commits a056f54 and 8554ce9, and my 4th June comment in issue
  -- #52.
  tryDaemons ts
    | any doesKill ts = case partition doesKill tids' of
        (kills, nokills) -> nokills ++ kills
    | otherwise = ts
  doesKill t = killsDaemons t (action t)

  -- Restrict the possible decisions to those in the bound.
  restrictToBound = filter (\t -> inBound trc (decision t, action t))

  -- Move the threads which will immediately yield to the end of the list
  yieldsToEnd ts = case partition (willYield . action) ts of
    (yields, noyields) -> noyields ++ yields

  -- Get the decision that will lead to a thread being scheduled.
  decision = decisionOf (fst <$> prior) (S.fromList tids')

  -- Get the action of a thread
  action t = fromJust $ lookup t threads'

  -- The runnable thread IDs
  tids = fst <$> threads

  -- The runnable threads as a normal list.
  threads' = toList threads
  tids'    = toList tids

-------------------------------------------------------------------------------
-- Weighted random scheduler

-- | The scheduler state
data RandSchedState g = RandSchedState
  { schedWeights :: Map ThreadId Int
  -- ^ The thread weights: used in determining which to run.
  , schedGen     :: g
  -- ^ The random number generator.
  } deriving (Eq, Show)

instance NFData g => NFData (RandSchedState g) where
  rnf s = rnf ( schedWeights s
              , schedGen     s
              )

-- | Initial weighted random scheduler state.
initialRandSchedState :: g -> RandSchedState g
initialRandSchedState = RandSchedState M.empty

-- | Weighted random scheduler: assigns to each new thread a weight,
-- and makes a weighted random choice out of the runnable threads at
-- every step.
randSched :: RandomGen g => Scheduler (RandSchedState g)
randSched _ _ threads s = (pick choice enabled, RandSchedState weights' g'') where
  -- Select a thread
  pick idx ((x, f):xs)
    | idx < f = Just x
    | otherwise = pick (idx - f) xs
  pick _ [] = Nothing
  (choice, g'') = randomR (0, sum (map snd enabled) - 1) g'
  enabled = M.toList $ M.filterWithKey (\tid _ -> tid `elem` tids) weights'

  -- The weights, with any new threads added.
  weights' = schedWeights s `M.union` M.fromList newWeights
  (newWeights, g') = foldr assignWeight ([], schedGen s) $ filter (`M.notMember` schedWeights s) tids
  assignWeight tid ~(ws, g0) =
    let (w, g) = randomR (1, 50) g0
    in ((tid, w):ws, g)

  -- The runnable threads.
  tids = map fst (toList threads)

-------------------------------------------------------------------------------
-- Dependency function

-- | Check if an action is dependent on another.
dependent :: MemType -> DepState -> ThreadId -> ThreadAction -> ThreadId -> ThreadAction -> Bool
-- This is basically the same as 'dependent'', but can make use of the
-- additional information in a 'ThreadAction' to make different
-- decisions in a few cases:
--
--  - @SetNumCapabilities@ and @GetNumCapabilities@ are NOT dependent
--    IF the value read is the same as the value written. 'dependent''
--    can not see the value read (as it hasn't happened yet!), and so
--    is more pessimistic here.
--
--  - When masked interruptible, a thread can only be interrupted when
--    actually blocked. 'dependent'' has to assume that all
--    potentially-blocking operations can block, and so is more
--    pessimistic in this case.
--
--  - The @isBlock@ / @isBarrier@ case in 'dependent'' is NOT a sound
--    optimisation when dealing with a 'ThreadAction' that has been
--    converted to a 'Lookahead'. I'm not entirely sure why, which
--    makes me question whether the \"optimisation\" is sound as it
--    is.
--
--  - Dependency of STM transactions can be /greatly/ improved here,
--    as the 'Lookahead' does not know which @TVar@s will be touched,
--    and so has to assume all transactions are dependent.
dependent _ _ _ (SetNumCapabilities a) _ (GetNumCapabilities b) = a /= b
dependent _ ds _ (ThrowTo t) t2 a = t == t2 && canInterrupt ds t2 a
dependent memtype ds t1 a1 t2 a2 = case rewind a2 of
  Just l2
    | isSTM a1 && isSTM a2
      -> not . S.null $ tvarsOf a1 `S.intersection` tvarsOf a2
    | not (isBlock a1 && isBarrier (simplifyLookahead l2)) ->
      dependent' memtype ds t1 a1 t2 l2
  _ -> dependentActions memtype ds (simplifyAction a1) (simplifyAction a2)

  where
    isSTM (STM _ _) = True
    isSTM (BlockedSTM _) = True
    isSTM _ = False

-- | Variant of 'dependent' to handle 'Lookahead'.
--
-- Termination of the initial thread is handled specially in the DPOR
-- implementation.
dependent' :: MemType -> DepState -> ThreadId -> ThreadAction -> ThreadId -> Lookahead -> Bool
dependent' memtype ds t1 a1 t2 l2 = case (a1, l2) of
  -- Worst-case assumption: all IO is dependent.
  (LiftIO, WillLiftIO) -> True

  -- Throwing an exception is only dependent with actions in that
  -- thread and if the actions can be interrupted. We can also
  -- slightly improve on that by not considering interrupting the
  -- normal termination of a thread: it doesn't make a difference.
  (ThrowTo t, WillStop) | t == t2 -> False
  (Stop, WillThrowTo t) | t == t1 -> False
  (ThrowTo t, _)     -> t == t2 && canInterruptL ds t2 l2
  (_, WillThrowTo t) -> t == t1 && canInterrupt  ds t1 a1

  -- Another worst-case: assume all STM is dependent.
  (STM _ _, WillSTM) -> True

  -- This is a bit pessimistic: Set/Get are only dependent if the
  -- value set is not the same as the value that will be got, but we
  -- can't know that here. 'dependent' optimises this case.
  (GetNumCapabilities a, WillSetNumCapabilities b) -> a /= b
  (SetNumCapabilities _, WillGetNumCapabilities)   -> True
  (SetNumCapabilities a, WillSetNumCapabilities b) -> a /= b

  -- Don't impose a dependency if the other thread will immediately
  -- block already. This is safe because a context switch will occur
  -- anyway so there's no point pre-empting the action UNLESS the
  -- pre-emption would possibly allow for a different relaxed memory
  -- stage.
  _ | isBlock a1 && isBarrier (simplifyLookahead l2) -> False
    | otherwise -> dependentActions memtype ds (simplifyAction a1) (simplifyLookahead l2)

-- | Check if two 'ActionType's are dependent. Note that this is not
-- sufficient to know if two 'ThreadAction's are dependent, without
-- being so great an over-approximation as to be useless!
dependentActions :: MemType -> DepState -> ActionType -> ActionType -> Bool
dependentActions memtype ds a1 a2 = case (a1, a2) of
  -- Unsynchronised reads and writes are always dependent, even under
  -- a relaxed memory model, as an unsynchronised write gives rise to
  -- a commit, which synchronises.
  (UnsynchronisedRead          r1, _) | same crefOf && a2 /= PartiallySynchronisedCommit r1 -> a2 /= UnsynchronisedRead r1
  (UnsynchronisedWrite         r1, _) | same crefOf && a2 /= PartiallySynchronisedCommit r1 -> True
  (PartiallySynchronisedWrite  r1, _) | same crefOf && a2 /= PartiallySynchronisedCommit r1 -> True
  (PartiallySynchronisedModify r1, _) | same crefOf && a2 /= PartiallySynchronisedCommit r1 -> True
  (SynchronisedModify          r1, _) | same crefOf && a2 /= PartiallySynchronisedCommit r1 -> True

  -- Unsynchronised writes and synchronisation where the buffer is not
  -- empty.
  --
  -- See [RMMVerification], lemma 5.25.
  (UnsynchronisedWrite r1, _) | same crefOf && isCommit a2 r1 && isBuffered ds r1 -> False
  (_, UnsynchronisedWrite r2) | same crefOf && isCommit a1 r2 && isBuffered ds r2 -> False

  -- Unsynchronised reads where a memory barrier would flush a
  -- buffered write
  (UnsynchronisedRead r1, _) | isBarrier a2 -> isBuffered ds r1 && memtype /= SequentialConsistency
  (_, UnsynchronisedRead r2) | isBarrier a1 -> isBuffered ds r2 && memtype /= SequentialConsistency

  (_, _)
    -- Two actions on the same CRef where at least one is synchronised
    | same crefOf && (synchronises a1 (fromJust $ crefOf a1) || synchronises a2 (fromJust $ crefOf a2)) -> True
    -- Two actions on the same MVar
    | same mvarOf -> True

  _ -> False

  where
    same :: Eq a => (ActionType -> Maybe a) -> Bool
    same f = isJust (f a1) && f a1 == f a2

-------------------------------------------------------------------------------
-- Dependency function state

data DepState = DepState
  { depCRState :: Map CRefId Bool
  -- ^ Keep track of which @CRef@s have buffered writes.
  , depMaskState :: Map ThreadId MaskingState
  -- ^ Keep track of thread masking states. If a thread isn't present,
  -- the masking state is assumed to be @Unmasked@. This nicely
  -- provides compatibility with dpor-0.1, where the thread IDs are
  -- not available.
  } deriving (Eq, Show)

instance NFData DepState where
  rnf depstate = rnf ( depCRState depstate
                     , [(t, m `seq` ()) | (t, m) <- M.toList (depMaskState depstate)]
                     )

-- | Initial dependency state.
initialDepState :: DepState
initialDepState = DepState M.empty M.empty

-- | Update the 'CRef' buffer state with the action that has just
-- happened.
updateDepState :: DepState -> ThreadId -> ThreadAction -> DepState
updateDepState depstate tid act = DepState
  { depCRState   = updateCRState       act $ depCRState   depstate
  , depMaskState = updateMaskState tid act $ depMaskState depstate
  }

-- | Update the 'CRef' buffer state with the action that has just
-- happened.
updateCRState :: ThreadAction -> Map CRefId Bool -> Map CRefId Bool
updateCRState (CommitCRef _ r) = M.delete r
updateCRState (WriteCRef    r) = M.insert r True
updateCRState ta
  | isBarrier $ simplifyAction ta = const M.empty
  | otherwise = id

-- | Update the thread masking state with the action that has just
-- happened.
updateMaskState :: ThreadId -> ThreadAction -> Map ThreadId MaskingState -> Map ThreadId MaskingState
updateMaskState tid (Fork tid2) = \masks -> case M.lookup tid masks of
  -- A thread inherits the masking state of its parent.
  Just ms -> M.insert tid2 ms masks
  Nothing -> masks
updateMaskState tid (SetMasking   _ ms) = M.insert tid ms
updateMaskState tid (ResetMasking _ ms) = M.insert tid ms
updateMaskState _ _ = id

-- | Check if a 'CRef' has a buffered write pending.
isBuffered :: DepState -> CRefId -> Bool
isBuffered depstate r = M.findWithDefault False r (depCRState depstate)

-- | Check if an exception can interrupt a thread (action).
canInterrupt :: DepState -> ThreadId -> ThreadAction -> Bool
canInterrupt depstate tid act
  -- If masked interruptible, blocked actions can be interrupted.
  | isMaskedInterruptible depstate tid = case act of
    BlockedPutMVar  _ -> True
    BlockedReadMVar _ -> True
    BlockedTakeMVar _ -> True
    BlockedSTM      _ -> True
    BlockedThrowTo  _ -> True
    _ -> False
  -- If masked uninterruptible, nothing can be.
  | isMaskedUninterruptible depstate tid = False
  -- If no mask, anything can be.
  | otherwise = True

-- | Check if an exception can interrupt a thread (lookahead).
canInterruptL :: DepState -> ThreadId -> Lookahead -> Bool
canInterruptL depstate tid lh
  -- If masked interruptible, actions which can block may be
  -- interrupted.
  | isMaskedInterruptible depstate tid = case lh of
    WillPutMVar  _ -> True
    WillReadMVar _ -> True
    WillTakeMVar _ -> True
    WillSTM        -> True
    WillThrowTo  _ -> True
    _ -> False
  -- If masked uninterruptible, nothing can be.
  | isMaskedUninterruptible depstate tid = False
  -- If no mask, anything can be.
  | otherwise = True

-- | Check if a thread is masked interruptible.
isMaskedInterruptible :: DepState -> ThreadId -> Bool
isMaskedInterruptible depstate tid =
  M.lookup tid (depMaskState depstate) == Just MaskedInterruptible

-- | Check if a thread is masked uninterruptible.
isMaskedUninterruptible :: DepState -> ThreadId -> Bool
isMaskedUninterruptible depstate tid =
  M.lookup tid (depMaskState depstate) == Just MaskedUninterruptible

-------------------------------------------------------------------------------
-- * Utilities

-- The initial thread of a DPOR tree.
initialDPORThread :: DPOR -> ThreadId
initialDPORThread = S.elemAt 0 . dporRunnable

-- | Check if a thread yielded.
didYield :: ThreadAction -> Bool
didYield Yield = True
didYield _ = False

-- | Check if a thread will yield.
willYield :: Lookahead -> Bool
willYield WillYield = True
willYield _ = False

-- | Check if an action will kill daemon threads.
killsDaemons :: ThreadId -> Lookahead -> Bool
killsDaemons t WillStop = t == initialThread
killsDaemons _ _ = False

-- | Render a 'DPOR' value as a graph in GraphViz \"dot\" format.
toDot :: (ThreadId -> String)
  -- ^ Show a @tid@ - this should produce a string suitable for
  -- use as a node identifier.
  -> (ThreadAction -> String)
  -- ^ Show a @action@.
  -> DPOR
  -> String
toDot = toDotFiltered (\_ _ -> True)

-- | Render a 'DPOR' value as a graph in GraphViz \"dot\" format, with
-- a function to determine if a subtree should be included or not.
toDotFiltered :: (ThreadId -> DPOR -> Bool)
  -- ^ Subtree predicate.
  -> (ThreadId -> String)
  -> (ThreadAction -> String)
  -> DPOR
  -> String
toDotFiltered check showTid showAct = digraph . go "L" where
  digraph str = "digraph {\n" ++ str ++ "\n}"

  go l b = unlines $ node l b : edges l b

  -- Display a labelled node.
  node n b = n ++ " [label=\"" ++ label b ++ "\"]"

  -- Display the edges.
  edges l b = [ edge l l' i ++ go l' b'
              | (i, b') <- M.toList (dporDone b)
              , check i b'
              , let l' = l ++ tidId i
              ]

  -- A node label, summary of the DPOR state at that node.
  label b = showLst id
    [ maybe "Nothing" (("Just " ++) . showAct) $ dporAction b
    , "Run:" ++ showLst showTid (S.toList $ dporRunnable b)
    , "Tod:" ++ showLst showTid (M.keys   $ dporTodo     b)
    , "Slp:" ++ showLst (\(t,a) -> "(" ++ showTid t ++ ", " ++ showAct a ++ ")")
        (M.toList $ dporSleep b)
    ]

  -- Display a labelled edge
  edge n1 n2 l = n1 ++ " -> " ++ n2 ++ " [label=\"" ++ showTid l ++ "\"]\n"

  -- Show a list of values
  showLst showf xs = "[" ++ intercalate ", " (map showf xs) ++ "]"

  -- Generate a graphviz-friendly identifier from a tid.
  tidId = concatMap (show . ord) . showTid

-- | Internal errors.
err :: String -> String -> a
err func msg = error (func ++ ": (internal error) " ++ msg)

-- | A combination of 'partition' and 'concat'.
concatPartition :: (a -> Bool) -> [[a]] -> ([a], [a])
{-# INLINE concatPartition #-}
-- note: `foldr (flip (foldr select))` is slow, as is `foldl (foldl
-- select))`, and `foldl'` variants. The sweet spot seems to be `foldl
-- (foldr select)` for some reason I don't really understand.
concatPartition p = foldl (foldr select) ([], []) where
  -- Lazy pattern matching, got this trick from the 'partition'
  -- implementation. This reduces allocation fairly significantly; I
  -- do not know why.
  select a ~(ts, fs)
    | p a       = (a:ts, fs)
    | otherwise = (ts, a:fs)

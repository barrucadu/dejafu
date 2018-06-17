{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Test.DejaFu.SCT.Internal.DPOR
-- Copyright   : (c) 2015--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : DeriveAnyClass, DeriveGeneric, FlexibleContexts, ViewPatterns
--
-- Internal types and functions for SCT via dynamic partial-order
-- reduction.  This module is NOT considered to form part of the
-- public interface of this library.
module Test.DejaFu.SCT.Internal.DPOR where

import           Control.Applicative  ((<|>))
import           Control.DeepSeq      (NFData(..))
import           Control.Exception    (MaskingState(..))
import qualified Data.Foldable        as F
import           Data.Function        (on)
import           Data.List            (nubBy, partition, sortOn)
import           Data.List.NonEmpty   (toList)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (isJust, isNothing, listToMaybe,
                                       maybeToList)
import           Data.Sequence        (Seq, (|>))
import qualified Data.Sequence        as Sq
import           Data.Set             (Set)
import qualified Data.Set             as S
import           GHC.Generics         (Generic)
import           GHC.Stack            (HasCallStack)

import           Test.DejaFu.Internal
import           Test.DejaFu.Schedule (Scheduler(..))
import           Test.DejaFu.Types
import           Test.DejaFu.Utils    (decisionOf, tidOf)

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
  , dporNext     :: Maybe (ThreadId, DPOR)
  -- ^ The next decision made. Executions are explored in a
  -- depth-first fashion, so this changes as old subtrees are
  -- exhausted and new ones explored.
  , dporDone     :: Set ThreadId
  -- ^ All transitions which have been taken from this point,
  -- including conservatively-added ones.
  , dporSleep    :: Map ThreadId ThreadAction
  -- ^ Transitions to ignore (in this node and children) until a
  -- dependent transition happens.
  , dporTaken    :: Map ThreadId ThreadAction
  -- ^ Transitions which have been taken, excluding
  -- conservatively-added ones. This is used in implementing sleep
  -- sets.
  } deriving (Eq, Show, Generic, NFData)

-- | Check the DPOR data invariants and raise an error if any are
-- broken.
--
-- This is a reasonable thing to do, because if the state is corrupted
-- then nothing sensible can happen anyway.
validateDPOR :: HasCallStack => DPOR -> DPOR
validateDPOR dpor
    | not (todo `S.isSubsetOf` runnable) = fatal "thread exists in todo set but not runnable set"
    | not (done `S.isSubsetOf` runnable) = fatal "thread exists in done set but not runnable set"
    | not (taken `S.isSubsetOf` done) = fatal "thread exists in taken set but not done set"
    | not (todo `disjoint` done) = fatal "thread exists in both taken set and done set"
    | not (maybe True (`S.member` done) next) = fatal "taken thread does not exist in done set"
    | otherwise = dpor
  where
    done = dporDone dpor
    next = fst <$> dporNext dpor
    runnable = dporRunnable dpor
    taken = S.fromList (M.keys (dporTaken dpor))
    todo = S.fromList (M.keys (dporTodo dpor))

    disjoint s1 s2 = S.null (S.intersection s1 s2)

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
  } deriving (Eq, Show, Generic, NFData)

-- | Initial DPOR state, given an initial thread ID. This initial
-- thread should exist and be runnable at the start of execution.
--
-- The main thread must be in the list of initially runnable threads.
initialState :: [ThreadId] -> DPOR
initialState threads
  | initialThread `elem` threads = DPOR
    { dporRunnable = S.fromList threads
    , dporTodo     = M.singleton initialThread False
    , dporNext     = Nothing
    , dporDone     = S.empty
    , dporSleep    = M.empty
    , dporTaken    = M.empty
    }
  | otherwise = fatal "initialState" "Initial thread is not in initially runnable set"

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
  :: DPOR
  -> Maybe ([ThreadId], Bool, Map ThreadId ThreadAction)
findSchedulePrefix dpor = case dporNext dpor of
    Just (tid, child) -> go tid child <|> here
    Nothing -> here
  where
    go tid child = (\(ts,c,slp) -> (tid:ts,c,slp)) <$> findSchedulePrefix child

    -- Prefix traces terminating with a to-do decision at this point.
    here =
      let todos = [([t], c, sleeps) | (t, c) <- M.toList $ dporTodo dpor]
          (best, worst) = partition (\([t],_,_) -> t >= initialThread) todos
      in listToMaybe best <|> listToMaybe worst

    -- The new sleep set is the union of the sleep set of the node
    -- we're branching from, plus all the decisions we've already
    -- explored.
    sleeps = dporSleep dpor `M.union` dporTaken dpor

-- | Add a new trace to the stack.  This won't work if to-dos aren't explored depth-first.
incorporateTrace :: HasCallStack
  => Bool
  -- ^ True if all IO is thread-safe.
  -> MemType
  -> Bool
  -- ^ Whether the \"to-do\" point which was used to create this new
  -- execution was conservative or not.
  -> Trace
  -- ^ The execution trace: the decision made, the runnable threads,
  -- and the action performed.
  -> DPOR
  -> DPOR
incorporateTrace safeIO memtype conservative trace dpor0 = grow initialDepState (initialDPORThread dpor0) trace dpor0 where
  grow state tid trc@((d, _, a):rest) dpor =
    let tid'   = tidOf tid d
        state' = updateDepState memtype state tid' a
    in case dporNext dpor of
         Just (t, child)
           | t == tid' ->
             validateDPOR $ dpor { dporNext = Just (tid', grow state' tid' rest child) }
           | hasTodos child -> fatal "replacing child with todos!"
         _ -> validateDPOR $
           let taken = M.insert tid' a (dporTaken dpor)
               sleep = dporSleep dpor `M.union` dporTaken dpor
           in dpor { dporTaken = if conservative then dporTaken dpor else taken
                   , dporTodo  = M.delete tid' (dporTodo dpor)
                   , dporNext  = Just (tid', subtree state' tid' sleep trc)
                   , dporDone  = S.insert tid' (dporDone dpor)
                   }
  grow _ _ [] _ = fatal "trace exhausted without reading a to-do point!"

  -- check if there are to-do points in a tree
  hasTodos dpor = not (M.null (dporTodo dpor)) || (case dporNext dpor of Just (_, dpor') -> hasTodos dpor'; _ -> False)

  -- Construct a new subtree corresponding to a trace suffix.
  subtree state tid sleep ((_, _, a):rest) = validateDPOR $
    let state' = updateDepState memtype state tid a
        sleep' = M.filterWithKey (\t a' -> not $ dependent safeIO state' tid a t a') sleep
    in DPOR
        { dporRunnable = S.fromList $ case rest of
            ((d', runnable, _):_) -> tidOf tid d' : map fst runnable
            [] -> []
        , dporTodo = M.empty
        , dporNext = case rest of
          ((d', _, _):_) ->
            let tid' = tidOf tid d'
            in  Just (tid', subtree state' tid' sleep' rest)
          [] -> Nothing
        , dporDone = case rest of
            ((d', _, _):_) -> S.singleton (tidOf tid d')
            [] -> S.empty
        , dporSleep = sleep'
        , dporTaken = case rest of
          ((d', _, a'):_) -> M.singleton (tidOf tid d') a'
          [] -> M.empty
        }
  subtree _ _ _ [] = fatal "subtree suffix empty!"

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
  :: Bool
  -- ^ True if all IO is thread-safe
  -> MemType
  -> BacktrackFunc
  -- ^ Backtracking function. Given a list of backtracking points, and
  -- a thread to backtrack to at a specific point in that list, add
  -- the new backtracking points. There will be at least one: this
  -- chosen one, but the function may add others.
  -> Bool
  -- ^ Whether the computation was aborted due to no decisions being
  -- in-bounds.
  -> Seq ([(ThreadId, Lookahead)], [ThreadId])
  -- ^ A sequence of threads at each step: the list of runnable
  -- in-bound threads (with lookahead values), and the list of threads
  -- still to try. The reason for the two separate lists is because
  -- the threads chosen to try will be dependent on the specific
  -- domain.
  -> Trace
  -- ^ The execution trace.
  -> [BacktrackStep]
findBacktrackSteps safeIO memtype backtrack boundKill = go initialDepState S.empty initialThread [] . F.toList where
  -- Walk through the traces one step at a time, building up a list of
  -- new backtracking points.
  go state allThreads tid bs ((e,i):is) ((d,_,a):ts) =
    let tid' = tidOf tid d
        state' = updateDepState memtype state tid' a
        this = BacktrackStep
          { bcktThreadid   = tid'
          , bcktDecision   = d
          , bcktAction     = a
          , bcktRunnable   = M.fromList e
          , bcktBacktracks = M.fromList $ map (\i' -> (i', False)) i
          , bcktState      = state
          }
        bs' = doBacktrack killsEarly allThreads' e (bs++[this])
        runnable = S.fromList (M.keys $ bcktRunnable this)
        allThreads' = allThreads `S.union` runnable
        killsEarly = null ts && boundKill
    in go state' allThreads' tid' bs' is ts
  go _ _ _ bs _ _ = bs

  -- Find the prior actions dependent with this one and add
  -- backtracking points.
  doBacktrack killsEarly allThreads enabledThreads bs =
    let tagged = reverse $ zip [0..] bs
        idxs   = [ (i, False, u)
                 | (u, n) <- enabledThreads
                 , v <- S.toList allThreads
                 , u /= v
                 , i <- maybeToList (findIndex u n v tagged)]

        findIndex u n v = go' True where
          {-# INLINE go' #-}
          go' final ((i,b):rest)
            -- Don't cross subconcurrency boundaries
            | isSubC final b = Nothing
            -- If this is the final action in the trace and the
            -- execution was killed due to nothing being within bounds
            -- (@killsEarly == True@) assume worst-case dependency.
            | bcktThreadid b == v && (killsEarly || isDependent b) = Just i
            | otherwise = go' False rest
          go' _ [] = Nothing

          {-# INLINE isSubC #-}
          isSubC final b = case bcktAction b of
            Stop -> not final && bcktThreadid b == initialThread
            Subconcurrency -> bcktThreadid b == initialThread
            _ -> False

          {-# INLINE isDependent #-}
          isDependent b
            -- Don't impose a dependency if the other thread will
            -- immediately block already. This is safe because a
            -- context switch will occur anyway so there's no point
            -- pre-empting the action UNLESS the pre-emption would
            -- possibly allow for a different relaxed memory stage.
            | isBlock (bcktAction b) && isBarrier (simplifyLookahead n) = False
            | otherwise = dependent' safeIO (bcktState b) (bcktThreadid b) (bcktAction b) u n
    in backtrack bs idxs

-- | Add new backtracking points, if they have not already been
-- visited and aren't in the sleep set.
incorporateBacktrackSteps :: HasCallStack
  => [BacktrackStep] -> DPOR -> DPOR
incorporateBacktrackSteps (b:bs) dpor = validateDPOR dpor' where
  tid = bcktThreadid b

  dpor' = dpor
    { dporTodo = dporTodo dpor `M.union` M.fromList todo
    , dporNext = Just (tid, child)
    }

  todo =
    [ x
    | x@(t,c) <- M.toList $ bcktBacktracks b
    , Just t /= (fst <$> dporNext dpor)
    , S.notMember t (dporDone dpor)
    , c || M.notMember t (dporSleep dpor)
    ]

  child = case dporNext dpor of
    Just (t, d)
      | t /= tid -> fatal "incorporating wrong trace!"
      | otherwise -> incorporateBacktrackSteps bs d
    Nothing -> fatal "child is missing!"
incorporateBacktrackSteps [] dpor = dpor

-------------------------------------------------------------------------------
-- * DPOR scheduler

-- | The scheduler state
data DPORSchedState k = DPORSchedState
  { schedSleep     :: Map ThreadId ThreadAction
  -- ^ The sleep set: decisions not to make until something dependent
  -- with them happens.
  , schedPrefix    :: [ThreadId]
  -- ^ Decisions still to make
  , schedBPoints   :: Seq ([(ThreadId, Lookahead)], [ThreadId])
  -- ^ Which threads are runnable and in-bound at each step, and the
  -- alternative decisions still to make.
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
  , schedBState    :: Maybe k
  -- ^ State used by the incremental bounding function.
  } deriving (Eq, Show, Generic, NFData)

-- | Initial DPOR scheduler state for a given prefix
initialDPORSchedState :: Map ThreadId ThreadAction
  -- ^ The initial sleep set.
  -> [ThreadId]
  -- ^ The schedule prefix.
  -> DPORSchedState k
initialDPORSchedState sleep prefix = DPORSchedState
  { schedSleep     = sleep
  , schedPrefix    = prefix
  , schedBPoints   = Sq.empty
  , schedIgnore    = False
  , schedBoundKill = False
  , schedDepState  = initialDepState
  , schedBState    = Nothing
  }

-- | An incremental bounding function is a stateful function that
-- takes the last and next decisions, and returns a new state only if
-- the next decision is within the bound.
type IncrementalBoundFunc k
  = Maybe k -> Maybe (ThreadId, ThreadAction) -> (Decision, Lookahead) -> Maybe k

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
backtrackAt :: HasCallStack
  => (ThreadId -> BacktrackStep -> Bool)
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
  go _ [] _ _ _ _ = fatal "ran out of schedule whilst backtracking!"

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
dporSched :: HasCallStack
  => Bool
  -- ^ True if all IO is thread safe.
  -> MemType
  -> IncrementalBoundFunc k
  -- ^ Bound function: returns true if that schedule prefix terminated
  -- with the lookahead decision fits within the bound.
  -> Scheduler (DPORSchedState k)
dporSched safeIO memtype boundf = Scheduler $ \prior threads s ->
  let
    -- The next scheduler state
    nextState rest = s
      { schedBPoints  = schedBPoints s |> (restrictToBound fst threads', rest)
      , schedDepState = nextDepState
      }
    nextDepState = let ds = schedDepState s in maybe ds (uncurry $ updateDepState memtype ds) prior

    -- Pick a new thread to run, not considering bounds. Choose the
    -- current thread if available and it hasn't just yielded,
    -- otherwise add all runnable threads.
    initialise = tryDaemons . yieldsToEnd $ case prior of
      Just (tid, act)
        | not (didYield act) && tid `elem` tids && isInBound tid -> [tid]
      _ -> tids

    -- If one of the chosen actions will kill the computation, and
    -- there are daemon threads, try them instead.
    --
    -- This is necessary if the killing action is NOT dependent with
    -- every other action, according to the dependency function. This
    -- is, strictly speaking, wrong; an action that kills another
    -- thread is definitely dependent with everything in that
    -- thread. HOWEVER, implementing it that way leads to an explosion
    -- of schedules tried. Really, all that needs to happen is for the
    -- thread-that-would-be-killed to be executed fully ONCE, and then
    -- the normal dependency mechanism will identify any other
    -- backtracking points that should be tried. This is achieved by
    -- adding every thread that would be killed to the to-do list.
    -- Furthermore, these threads MUST be ahead of the killing thread,
    -- or the killing thread will end up in the sleep set and so the
    -- killing action not performed. This is, again, because of the
    -- lack of the dependency messing things up in the name of
    -- performance.
    --
    -- See commits a056f54 and 8554ce9, and my 4th June comment in
    -- issue #52.
    tryDaemons ts
      | any doesKill ts = case partition doesKill tids of
          (kills, nokills) -> nokills ++ kills
      | otherwise = ts
    doesKill t = killsDaemons t (action t)

    -- Restrict the possible decisions to those in the bound.
    restrictToBound f = filter (isInBound . f)
    isInBound t = isJust $ boundf (schedBState s) prior (decision t, action t)

    -- Move the threads which will immediately yield to the end of the list
    yieldsToEnd ts = case partition (willYield . action) ts of
      (yields, noyields) -> noyields ++ yields

    -- Get the decision that will lead to a thread being scheduled.
    decision = decisionOf (fst <$> prior) (S.fromList tids)

    -- Get the action of a thread
    action t = efromJust (lookup t threads')

    -- The runnable thread IDs
    tids = map fst threads'

    -- The runnable threads as a normal list.
    threads' = toList threads
  in case schedPrefix s of
    -- If there is a decision available, make it
    (t:ts) ->
      let bstate' = boundf (schedBState s) prior (decision t, action t)
      in (Just t, (nextState []) { schedPrefix = ts, schedBState = bstate' })

    -- Otherwise query the initialise function for a list of possible
    -- choices, filter out anything in the sleep set, and make one of
    -- them arbitrarily (recording the others).
    [] ->
      let choices  = restrictToBound id initialise
          checkDep t a = case prior of
            Just (tid, act) -> dependent safeIO (schedDepState s) tid act t a
            Nothing -> False
          ssleep'  = M.filterWithKey (\t a -> not $ checkDep t a) $ schedSleep s
          choices' = filter (`notElem` M.keys ssleep') choices
          signore' = not (null choices) && all (`elem` M.keys ssleep') choices
          sbkill'  = not (null initialise) && null choices
      in case choices' of
        (nextTid:rest) ->
          let bstate' = boundf (schedBState s) prior (decision nextTid, action nextTid)
          in (Just nextTid, (nextState rest) { schedSleep = ssleep', schedBState = bstate' })
        [] ->
          (Nothing, (nextState []) { schedIgnore = signore', schedBoundKill = sbkill', schedBState = Nothing })

-------------------------------------------------------------------------------
-- * Dependency function

-- | Check if two actions commute.
--
-- This implements a stronger check that @not (dependent ...)@, as it
-- handles some cases which 'dependent' doesn't need to care about.
--
-- This should not be used to re-order traces which contain
-- subconcurrency.
independent :: Bool -> DepState -> ThreadId -> ThreadAction -> ThreadId -> ThreadAction -> Bool
independent safeIO ds t1 a1 t2 a2
    | t1 == t2 = False
    | check t1 a1 t2 a2 = False
    | check t2 a2 t1 a1 = False
    | otherwise = not (dependent safeIO ds t1 a1 t2 a2)
  where
    -- @dontCheck@ must be the first thing in the computation.
    check _ (DontCheck _) _ _ = True
    -- can't re-order any action of a thread with the fork which
    -- created it.
    check _ (Fork t) tid _ | t == tid = True
    check _ (ForkOS t) tid _ | t == tid = True
    -- because we can't easily tell if this will terminate the other
    -- thread, we just can't re-order asynchronous exceptions at all
    -- :(
    --
    -- See #191 / #190
    check _ (ThrowTo t _) tid _ | t == tid = True
    check _ (BlockedThrowTo t) tid _ | t == tid = True
    -- can't re-order an unsynchronised write with something which synchronises that CRef.
    check _ (simplifyAction -> UnsynchronisedWrite r) _ (simplifyAction -> a) | synchronises a r = True
    check _ _ _ _ = False

-- | Check if an action is dependent on another.
--
-- This is basically the same as 'dependent'', but can make use of the
-- additional information in a 'ThreadAction' to make better decisions
-- in a few cases.
dependent :: Bool -> DepState -> ThreadId -> ThreadAction -> ThreadId -> ThreadAction -> Bool
dependent safeIO ds t1 a1 t2 a2 = case (a1, a2) of
  -- When masked interruptible, a thread can only be interrupted when
  -- actually blocked. 'dependent'' has to assume that all
  -- potentially-blocking operations can block, and so is more
  -- pessimistic in this case.
  (ThrowTo t _, ThrowTo u _)
    | t == t2 && u == t1 -> canInterrupt ds t1 a1 || canInterrupt ds t2 a2
  (ThrowTo t _, _) | t == t2 -> canInterrupt ds t2 a2 && a2 /= Stop
  (_, ThrowTo t _) | t == t1 -> canInterrupt ds t1 a1 && a1 /= Stop

  -- Dependency of STM transactions can be /greatly/ improved here, as
  -- the 'Lookahead' does not know which @TVar@s will be touched, and
  -- so has to assume all transactions are dependent.
  (STM _ _, STM _ _)           -> checkSTM
  (STM _ _, BlockedSTM _)      -> checkSTM
  (BlockedSTM _, STM _ _)      -> checkSTM
  (BlockedSTM _, BlockedSTM _) -> checkSTM

  _ -> dependent' safeIO ds t1 a1 t2 (rewind a2)
    && dependent' safeIO ds t2 a2 t1 (rewind a1)

  where
    -- STM actions A and B are dependent if A wrote to anything B
    -- touched, or vice versa.
    checkSTM = checkSTM' a1 a2 || checkSTM' a2 a1
    checkSTM' a b = not . S.null $ tvarsWritten a `S.intersection` tvarsOf b

-- | Variant of 'dependent' to handle 'Lookahead'.
--
-- Termination of the initial thread is handled specially in the DPOR
-- implementation.
dependent' :: Bool -> DepState -> ThreadId -> ThreadAction -> ThreadId -> Lookahead -> Bool
dependent' safeIO ds t1 a1 t2 l2 = case (a1, l2) of
  -- Worst-case assumption: all IO is dependent.
  (LiftIO, WillLiftIO) -> not safeIO

  -- Throwing an exception is only dependent with actions in that
  -- thread and if the actions can be interrupted. We can also
  -- slightly improve on that by not considering interrupting the
  -- normal termination of a thread: it doesn't make a difference.
  (ThrowTo t _, WillThrowTo u)
    | t == t2 && u == t1 -> canInterrupt ds t1 a1 || canInterruptL ds t2 l2
  (ThrowTo t _, _)   | t == t2 -> canInterruptL ds t2 l2 && l2 /= WillStop
  (_, WillThrowTo t) | t == t1 -> canInterrupt  ds t1 a1 && a1 /= Stop

  -- Another worst-case: assume all STM is dependent.
  (STM _ _, WillSTM) -> True
  (BlockedSTM _, WillSTM) -> True

  -- the number of capabilities is essentially a global shared
  -- variable
  (GetNumCapabilities _, WillSetNumCapabilities _) -> True
  (SetNumCapabilities _, WillGetNumCapabilities)   -> True
  (SetNumCapabilities _, WillSetNumCapabilities _) -> True

  _ -> dependentActions ds (simplifyAction a1) (simplifyLookahead l2)

-- | Check if two 'ActionType's are dependent. Note that this is not
-- sufficient to know if two 'ThreadAction's are dependent, without
-- being so great an over-approximation as to be useless!
dependentActions :: DepState -> ActionType -> ActionType -> Bool
dependentActions ds a1 a2 = case (a1, a2) of
  (UnsynchronisedRead _, UnsynchronisedRead _) -> False

  -- Unsynchronised writes and synchronisation where the buffer is not
  -- empty.
  --
  -- See [RMMVerification], lemma 5.25.
  (UnsynchronisedWrite r1, PartiallySynchronisedCommit r2) | r1 == r2 && isBuffered ds r1 -> False
  (PartiallySynchronisedCommit r1, UnsynchronisedWrite r2) | r1 == r2 && isBuffered ds r1 -> False

  -- Unsynchronised reads where a memory barrier would flush a
  -- buffered write
  (UnsynchronisedRead r1, _) | isBarrier a2 && isBuffered ds r1 -> True
  (_, UnsynchronisedRead r2) | isBarrier a1 && isBuffered ds r2 -> True

  -- Commits and memory barriers must be dependent, as memory barriers
  -- (currently) flush in a consistent order.  Alternative orders need
  -- to be explored as well.  Perhaps a better implementation of
  -- memory barriers would just block every non-commit thread while
  -- any buffer is nonempty.
  (PartiallySynchronisedCommit r1, _) | synchronises a2 r1 -> True
  (_, PartiallySynchronisedCommit r2) | synchronises a1 r2 -> True

  -- Two @MVar@ puts are dependent if they're to the same empty
  -- @MVar@, and two takes are dependent if they're to the same full
  -- @MVar@.
  (SynchronisedWrite v1, SynchronisedWrite v2) | v1 == v2 -> not (isFull ds v1)
  (SynchronisedRead  v1, SynchronisedRead  v2) | v1 == v2 -> isFull ds v1
  (SynchronisedWrite v1, SynchronisedRead  v2) | v1 == v2 -> True
  (SynchronisedRead  v1, SynchronisedWrite v2) | v1 == v2 -> True

  (_, _) -> maybe False (\r -> Just r == crefOf a2) (crefOf a1)

-------------------------------------------------------------------------------
-- ** Dependency function state

data DepState = DepState
  { depCRState :: Map CRefId Bool
  -- ^ Keep track of which @CRef@s have buffered writes.
  , depMVState :: Set MVarId
  -- ^ Keep track of which @MVar@s are full.
  , depMaskState :: Map ThreadId MaskingState
  -- ^ Keep track of thread masking states. If a thread isn't present,
  -- the masking state is assumed to be @Unmasked@. This nicely
  -- provides compatibility with dpor-0.1, where the thread IDs are
  -- not available.
  } deriving (Eq, Show)

instance NFData DepState where
  rnf depstate = rnf ( depCRState depstate
                     , depMVState depstate
                     , [(t, m `seq` ()) | (t, m) <- M.toList (depMaskState depstate)]
                     )

-- | Initial dependency state.
initialDepState :: DepState
initialDepState = DepState M.empty S.empty M.empty

-- | Update the dependency state with the action that has just
-- happened.
updateDepState :: MemType -> DepState -> ThreadId -> ThreadAction -> DepState
updateDepState memtype depstate tid act = DepState
  { depCRState   = updateCRState memtype act $ depCRState   depstate
  , depMVState   = updateMVState         act $ depMVState   depstate
  , depMaskState = updateMaskState tid   act $ depMaskState depstate
  }

-- | Update the @CRef@ buffer state with the action that has just
-- happened.
updateCRState :: MemType -> ThreadAction -> Map CRefId Bool -> Map CRefId Bool
updateCRState SequentialConsistency _ = const M.empty
updateCRState _ (CommitCRef _ r) = M.delete r
updateCRState _ (WriteCRef    r) = M.insert r True
updateCRState _ ta
  | isBarrier $ simplifyAction ta = const M.empty
  | otherwise = id

-- | Update the @MVar@ full/empty state with the action that has just
-- happened.
updateMVState :: ThreadAction -> Set MVarId -> Set MVarId
updateMVState (PutMVar mvid _) = S.insert mvid
updateMVState (TryPutMVar mvid True _) = S.insert mvid
updateMVState (TakeMVar mvid _) = S.delete mvid
updateMVState (TryTakeMVar mvid True _) = S.delete mvid
updateMVState _ = id

-- | Update the thread masking state with the action that has just
-- happened.
updateMaskState :: ThreadId -> ThreadAction -> Map ThreadId MaskingState -> Map ThreadId MaskingState
updateMaskState tid (Fork tid2) = \masks -> case M.lookup tid masks of
  -- A thread inherits the masking state of its parent.
  Just ms -> M.insert tid2 ms masks
  Nothing -> masks
updateMaskState tid (SetMasking   _ ms) = M.insert tid ms
updateMaskState tid (ResetMasking _ ms) = M.insert tid ms
updateMaskState tid (Throw True) = M.delete tid
updateMaskState _ (ThrowTo tid True) = M.delete tid
updateMaskState tid Stop = M.delete tid
updateMaskState _ _ = id

-- | Check if a @CRef@ has a buffered write pending.
isBuffered :: DepState -> CRefId -> Bool
isBuffered depstate r = M.findWithDefault False r (depCRState depstate)

-- | Check if an @MVar@ is full.
isFull :: DepState -> MVarId -> Bool
isFull depstate v = S.member v (depMVState depstate)

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
didYield (ThreadDelay _) = True
didYield _ = False

-- | Check if a thread will yield.
willYield :: Lookahead -> Bool
willYield WillYield = True
willYield (WillThreadDelay _) = True
willYield _ = False

-- | Check if an action will kill daemon threads.
killsDaemons :: ThreadId -> Lookahead -> Bool
killsDaemons t WillStop = t == initialThread
killsDaemons _ _ = False

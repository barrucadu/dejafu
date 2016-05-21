-- |
-- Module      : Test.DPOR.Internal
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Internal types and functions for dynamic partial-order
-- reduction. This module is NOT considered to form part of the public
-- interface of this library.
module Test.DPOR.Internal where

import Control.DeepSeq (NFData(..), force)
import Data.Char (ord)
import Data.List (foldl', intercalate, partition, sortBy)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Ord (Down(..), comparing)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as Sq

import Test.DPOR.Schedule (Decision(..), Scheduler, decisionOf, tidOf)

-------------------------------------------------------------------------------
-- * Dynamic partial-order reduction

-- | DPOR execution is represented as a tree of states, characterised
-- by the decisions that lead to that state.
data DPOR tid action = DPOR
  { dporRunnable :: Set tid
  -- ^ What threads are runnable at this step.
  , dporTodo     :: Map tid Bool
  -- ^ Follow-on decisions still to make, and whether that decision
  -- was added conservatively due to the bound.
  , dporDone     :: Map tid (DPOR tid action)
  -- ^ Follow-on decisions that have been made.
  , dporSleep    :: Map tid action
  -- ^ Transitions to ignore (in this node and children) until a
  -- dependent transition happens.
  , dporTaken    :: Map tid action
  -- ^ Transitions which have been taken, excluding
  -- conservatively-added ones. This is used in implementing sleep
  -- sets.
  , dporAction   :: Maybe action
  -- ^ What happened at this step. This will be 'Nothing' at the root,
  -- 'Just' everywhere else.
  }

instance (NFData tid, NFData action) => NFData (DPOR tid action) where
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
data BacktrackStep tid action lookahead state = BacktrackStep
  { bcktThreadid   :: tid
  -- ^ The thread running at this step
  , bcktDecision   :: (Decision tid, action)
  -- ^ What happened at this step.
  , bcktRunnable   :: Map tid lookahead
  -- ^ The threads runnable at this step
  , bcktBacktracks :: Map tid Bool
  -- ^ The list of alternative threads to run, and whether those
  -- alternatives were added conservatively due to the bound.
  , bcktState      :: state
  -- ^ Some domain-specific state at this point.
  } deriving Show

instance ( NFData tid
         , NFData action
         , NFData lookahead
         , NFData state
         ) => NFData (BacktrackStep tid action lookahead state) where
  rnf b = rnf ( bcktThreadid   b
              , bcktDecision   b
              , bcktRunnable   b
              , bcktBacktracks b
              , bcktState      b
              )

-- | Initial DPOR state, given an initial thread ID. This initial
-- thread should exist and be runnable at the start of execution.
initialState :: Ord tid => tid -> DPOR tid action
initialState initialThread = DPOR
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
--
-- This returns the longest prefix, on the assumption that this will
-- lead to lots of backtracking points being identified before
-- higher-up decisions are reconsidered, so enlarging the sleep sets.
findSchedulePrefix :: Ord tid
  => (tid -> Bool)
  -- ^ Some partitioning function, applied to the to-do decisions. If
  -- there is an identifier which passes the test, it will be used,
  -- rather than any which fail it. This allows a very basic way of
  -- domain-specific prioritisation between otherwise equal choices,
  -- which may be useful in some cases.
  -> DPOR tid action
  -> Maybe ([tid], Bool, Map tid action)
findSchedulePrefix predicate dpor0 = go (initialDPORThread dpor0) dpor0 where
  go tid dpor =
        -- All the possible prefix traces from this point, with
        -- updated DPOR subtrees if taken from the done list.
    let prefixes = mapMaybe go' (M.toList $ dporDone dpor) ++ here dpor
        -- Sort by number of preemptions, in descending order.
        cmp = Down . preEmps tid dpor . (\(a,_,_) -> a)
        sorted = sortBy (comparing cmp) prefixes

    in if null prefixes
       then Nothing
       else case partition (\(t:_,_,_) -> predicate t) sorted of
              (choice:_, _)  -> Just choice
              ([], choice:_) -> Just choice
              ([], []) -> err "findSchedulePrefix" "empty prefix list!" 

  go' (tid, dpor) = (\(ts,c,slp) -> (tid:ts,c,slp)) <$> go tid dpor

  -- Prefix traces terminating with a to-do decision at this point.
  here dpor = [([t], c, sleeps dpor) | (t, c) <- M.toList $ dporTodo dpor]

  -- The new sleep set is the union of the sleep set of the node we're
  -- branching from, plus all the decisions we've already explored.
  sleeps dpor = dporSleep dpor `M.union` dporTaken dpor

  -- The number of pre-emptive context switches
  preEmps tid dpor (t:ts) =
    let rest = preEmps t (fromJust . M.lookup t $ dporDone dpor) ts
    in  if tid `S.member` dporRunnable dpor then 1 + rest else rest
  preEmps _ _ [] = 0::Int

-- | One of the outputs of the runner is a @Trace@, which is a log of
-- decisions made, all the runnable threads and what they would do,
-- and the action a thread took in its step.
type Trace tid action lookahead
  = [(Decision tid, [(tid, NonEmpty lookahead)], action)]

-- | Add a new trace to the tree, creating a new subtree branching off
-- at the point where the \"to-do\" decision was made.
incorporateTrace :: Ord tid
  => state
  -- ^ Initial state
  -> (state -> action -> state)
  -- ^ State step function
  -> (state -> (tid, action) -> (tid, action) -> Bool)
  -- ^ Dependency function
  -> Bool
  -- ^ Whether the \"to-do\" point which was used to create this new
  -- execution was conservative or not.
  -> Trace tid action lookahead
  -- ^ The execution trace: the decision made, the runnable threads,
  -- and the action performed.
  -> DPOR tid action
  -> DPOR tid action
incorporateTrace stinit ststep dependency conservative trace dpor0 = grow stinit (initialDPORThread dpor0) trace dpor0 where
  grow state tid trc@((d, _, a):rest) dpor =
    let tid'   = tidOf tid d
        state' = ststep state a
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
  grow _ _ [] dpor = dpor

  -- Construct a new subtree corresponding to a trace suffix.
  subtree state tid sleep ((_, _, a):rest) =
    let state' = ststep state a
        sleep' = M.filterWithKey (\t a' -> not $ dependency state' (tid, a) (t,a')) sleep
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
findBacktrackSteps :: Ord tid
  => s
  -- ^ Initial state.
  -> (s -> action -> s)
  -- ^ State step function.
  -> (s -> (tid, action) -> (tid, lookahead) -> Bool)
  -- ^ Dependency function.
  -> ([BacktrackStep tid action lookahead s] -> Int -> tid -> [BacktrackStep tid action lookahead s])
  -- ^ Backtracking function. Given a list of backtracking points, and
  -- a thread to backtrack to at a specific point in that list, add
  -- the new backtracking points. There will be at least one: this
  -- chosen one, but the function may add others.
  -> Seq (NonEmpty (tid, lookahead), [tid])
  -- ^ A sequence of threads at each step: the nonempty list of
  -- runnable threads (with lookahead values), and the list of threads
  -- still to try. The reason for the two separate lists is because
  -- the threads chosen to try will be dependent on the specific
  -- domain.
  -> Trace tid action lookahead
  -- ^ The execution trace.
  -> [BacktrackStep tid action lookahead s]
findBacktrackSteps _ _ _ _ bcktrck
  | Sq.null bcktrck = const []
findBacktrackSteps stinit ststep dependency backtrack bcktrck = go stinit S.empty initialThread [] (Sq.viewl bcktrck) where
  -- Get the initial thread ID
  initialThread = case Sq.viewl bcktrck of
    (((tid, _):|_, _):<_) -> tid
    _ -> err "findBacktrack" "impossible case reached!"

  -- Walk through the traces one step at a time, building up a list of
  -- new backtracking points.
  go state allThreads tid bs ((e,i):<is) ((d,_,a):ts) =
    let tid' = tidOf tid d
        state' = ststep state a
        this = BacktrackStep
          { bcktThreadid   = tid'
          , bcktDecision   = (d, a)
          , bcktRunnable   = M.fromList . toList $ e
          , bcktBacktracks = M.fromList $ map (\i' -> (i', False)) i
          , bcktState      = state'
          }
        bs' = doBacktrack killsEarly allThreads' (toList e) (bs++[this])
        runnable = S.fromList (M.keys $ bcktRunnable this)
        allThreads' = allThreads `S.union` runnable
        killsEarly = null ts && any (/=initialThread) runnable
    in go state' allThreads' tid' bs' (Sq.viewl is) ts
  go _ _ _ bs _ _ = bs

  -- Find the prior actions dependent with this one and add
  -- backtracking points.
  doBacktrack killsEarly allThreads enabledThreads bs =
    let tagged = reverse $ zip [0..] bs
        idxs   = [ (head is, u)
                 | (u, n) <- enabledThreads
                 , v <- S.toList allThreads
                 , u /= v
                 , let is = idxs' u n v tagged
                 , not $ null is]

        idxs' u n v = mapMaybe go' where
          go' (i, b)
            | bcktThreadid b == v && (killsEarly || isDependent b) = Just i
            | otherwise = Nothing

          isDependent b = dependency (bcktState b) (bcktThreadid b, snd $ bcktDecision b) (u, n)
    in foldl' (\b (i, u) -> backtrack b i u) bs idxs

-- | Add new backtracking points, if they have not already been
-- visited, fit into the bound, and aren't in the sleep set.
incorporateBacktrackSteps :: Ord tid
  => ([(Decision tid, action)] -> (Decision tid, lookahead) -> Bool)
  -- ^ Bound function: returns true if that schedule prefix terminated
  -- with the lookahead decision fits within the bound.
  -> [BacktrackStep tid action lookahead s]
  -- ^ Backtracking steps identified by 'findBacktrackSteps'.
  -> DPOR tid action
  -> DPOR tid action
incorporateBacktrackSteps bv = go Nothing [] where
  go priorTid pref (b:bs) bpor =
    let bpor' = doBacktrack priorTid pref b bpor
        tid   = bcktThreadid b
        pref' = pref ++ [bcktDecision b]
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

-- | A @Scheduler@ where the state is a @SchedState@.
type DPORScheduler tid action lookahead s
  = Scheduler tid action lookahead (SchedState tid action lookahead s)

-- | The scheduler state
data SchedState tid action lookahead s = SchedState
  { schedSleep   :: Map tid action
  -- ^ The sleep set: decisions not to make until something dependent
  -- with them happens.
  , schedPrefix  :: [tid]
  -- ^ Decisions still to make
  , schedBPoints :: Seq (NonEmpty (tid, lookahead), [tid])
  -- ^ Which threads are runnable at each step, and the alternative
  -- decisions still to make.
  , schedIgnore  :: Bool
  -- ^ Whether to ignore this execution or not: @True@ if the
  -- execution is aborted due to all possible decisions being in the
  -- sleep set, as then everything in this execution is covered by
  -- another.
  , schedDepState :: s
  -- ^ State used by the dependency function to determine when to
  -- remove decisions from the sleep set.
  } deriving Show

instance ( NFData tid
         , NFData action
         , NFData lookahead
         , NFData s
         ) => NFData (SchedState tid action lookahead s) where
  rnf s = rnf ( schedSleep    s
              , schedPrefix   s
              , schedBPoints  s
              , schedIgnore   s
              , schedDepState s
              )

-- | Initial scheduler state for a given prefix
initialSchedState :: s
  -- ^ The initial dependency function state.
  -> Map tid action
  -- ^ The initial sleep set.
  -> [tid]
  -- ^ The schedule prefix.
  -> SchedState tid action lookahead s
initialSchedState s sleep prefix = SchedState
  { schedSleep    = sleep
  , schedPrefix   = prefix
  , schedBPoints  = Sq.empty
  , schedIgnore   = False
  , schedDepState = s
  }

-- | A bounding function takes the scheduling decisions so far and a
-- decision chosen to come next, and returns if that decision is
-- within the bound.
type BoundFunc tid action lookahead
  = [(Decision tid, action)] -> (Decision tid, lookahead) -> Bool

-- | A backtracking step is a point in the execution where another
-- decision needs to be made, in order to explore interesting new
-- schedules. A backtracking /function/ takes the steps identified so
-- far and a point and a thread to backtrack to, and inserts at least
-- that backtracking point. More may be added to compensate for the
-- effects of the bounding function. For example, under pre-emption
-- bounding a conservative backtracking point is added at the prior
-- context switch.
--
-- In general, a backtracking function should identify one or more
-- backtracking points, and then use @backtrackAt@ to do the actual
-- work.
type BacktrackFunc tid action lookahead s
  = [BacktrackStep tid action lookahead s] -> Int -> tid
  -> [BacktrackStep tid action lookahead s]

-- | DPOR scheduler: takes a list of decisions, and maintains a trace
-- including the runnable threads, and the alternative choices allowed
-- by the bound-specific initialise function.
--
-- After the initial decisions are exhausted, this prefers choosing
-- the prior thread if it's (1) still runnable and (2) hasn't just
-- yielded. Furthermore, threads which /will/ yield are ignored in
-- preference of those which will not.
--
-- This forces full evaluation of the result every step, to avoid any
-- possible space leaks.
dporSched :: (Ord tid, NFData tid, NFData action, NFData lookahead, NFData s)
  => (action -> Bool)
  -- ^ Determine if a thread yielded.
  -> (lookahead -> Bool)
  -- ^ Determine if a thread will yield.
  -> (s -> (tid, action) -> (tid, action) -> Bool)
  -- ^ Dependency function.
  -> (s -> action -> s)
  -- ^ Dependency function's state step function.
  -> BoundFunc tid action lookahead
  -- ^ Bound function: returns true if that schedule prefix terminated
  -- with the lookahead decision fits within the bound.
  -> DPORScheduler tid action lookahead s
dporSched didYield willYield dependency ststep inBound trc prior threads s = force schedule where
  -- Pick a thread to run.
  schedule = case schedPrefix s of
    -- If there is a decision available, make it
    (d:ds) -> (Just d, (nextState []) { schedPrefix = ds })

    -- Otherwise query the initialise function for a list of possible
    -- choices, filter out anything in the sleep set, and make one of
    -- them arbitrarily (recording the others).
    [] ->
      let choices  = initialise
          checkDep t a = case prior of
            Just (tid, act) -> dependency (schedDepState s) (tid, act) (t, a)
            Nothing -> False
          ssleep'  = M.filterWithKey (\t a -> not $ checkDep t a) $ schedSleep s
          choices' = filter (`notElem` M.keys ssleep') choices
          signore' = not (null choices) && all (`elem` M.keys ssleep') choices
      in case choices' of
            (nextTid:rest) -> (Just nextTid, (nextState rest) { schedSleep = ssleep' })
            [] -> (Nothing, (nextState []) { schedIgnore = signore' })

  -- The next scheduler state
  nextState rest = s
    { schedBPoints  = schedBPoints s |> (threads, rest)
    , schedDepState = case prior of
        Just (_, act) -> ststep (schedDepState s) act
        Nothing -> schedDepState s
    }

  -- Pick a new thread to run, which does not exceed the bound. Choose
  -- the current thread if available and it hasn't just yielded,
  -- otherwise add all runnable threads.
  initialise = restrictToBound . yieldsToEnd $ case prior of
    Just (tid, act)
      | didYield act -> map fst (toList threads)
      | any (\(t, _) -> t == tid) threads -> [tid]
    _ -> map fst (toList threads)

  -- Restrict the possible decisions to those in the bound.
  restrictToBound = fst . partition (\t -> inBound trc (decision t, action t))

  -- Move the threads which will immediately yield to the end of the list
  yieldsToEnd ts = case partition (willYield . action) ts of
    (yields, noyields) -> noyields ++ yields

  -- Get the decision that will lead to a thread being scheduled.
  decision = decisionOf (fst <$> prior) (S.fromList $ map fst threads')

  -- Get the action of a thread
  action t = fromJust $ lookup t threads'

  -- The runnable threads as a normal list.
  threads' = toList threads

-------------------------------------------------------------------------------
-- * Utilities

-- The initial thread of a DPOR tree.
initialDPORThread :: DPOR tid action -> tid
initialDPORThread = S.elemAt 0 . dporRunnable

-- | Render a 'DPOR' value as a graph in GraphViz \"dot\" format.
toDot :: (tid -> String)
  -- ^ Show a @tid@ - this should produce a string suitable for
  -- use as a node identifier.
  -> (action -> String)
  -- ^ Show a @action@.
  -> DPOR tid action
  -> String
toDot = toDotFiltered (\_ _ -> True)

-- | Render a 'DPOR' value as a graph in GraphViz \"dot\" format, with
-- a function to determine if a subtree should be included or not.
toDotFiltered :: (tid -> DPOR tid action -> Bool)
  -- ^ Subtree predicate.
  -> (tid    -> String)
  -> (action -> String)
  -> DPOR tid action
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

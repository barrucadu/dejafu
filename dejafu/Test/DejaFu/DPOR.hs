-- | Dynamic partial-order reduction.
module Test.DejaFu.DPOR
  ( -- * Scheduling decisions
    Decision(..)
  , tidOf
  , decisionOf
  , activeTid

  -- * Dynamic partial-order reduction
  , DPOR(..)
  , BacktrackStep(..)
  , initialState
  , findSchedulePrefix
  , incorporateTrace
  , findBacktrackSteps
  , incorporateBacktrackSteps

  -- * Utilities
  , initialDPORThread
  , toDot
  , toDotFiltered
  ) where

import Control.DeepSeq (NFData(..))
import Data.Char (ord)
import Data.List (foldl', intercalate, partition, sortBy)
import Data.List.Extra (NonEmpty(..), toList)
import Data.Ord (Down(..), comparing)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Sequence (Seq, ViewL(..))

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq

-------------------------------------------------------------------------------
-- Scheduling decisions

-- | Scheduling decisions are based on the state of the running
-- program, and so we can capture some of that state in recording what
-- specific decision we made.
data Decision thread_id =
    Start thread_id
  -- ^ Start a new thread, because the last was blocked (or it's the
  -- start of computation).
  | Continue
  -- ^ Continue running the last thread for another step.
  | SwitchTo thread_id
  -- ^ Pre-empt the running thread, and switch to another.
  deriving (Eq, Show)

instance NFData thread_id => NFData (Decision thread_id) where
  rnf (Start    tid) = rnf tid
  rnf (SwitchTo tid) = rnf tid
  rnf d = d `seq` ()

-- | Get the resultant thread identifier of a 'Decision', with a default case
-- for 'Continue'.
tidOf
  :: t
  -- ^ The @Continue@ case.
  -> Decision t
  -- ^ The decision.
  -> t
tidOf _ (Start t)    = t
tidOf _ (SwitchTo t) = t
tidOf tid _          = tid

-- | Get the 'Decision' that would have resulted in this thread identifier,
-- given a prior thread (if any) and list of runnable threads.
decisionOf :: (Eq thread_id, Foldable f)
  => Maybe thread_id
  -- ^ The prior thread.
  -> f thread_id
  -- ^ The runnable threads.
  -> thread_id
  -- ^ The current thread.
  -> Decision thread_id
decisionOf Nothing _ chosen = Start chosen
decisionOf (Just prior) runnable chosen
  | prior == chosen = Continue
  | prior `elem` runnable = SwitchTo chosen
  | otherwise = Start chosen

-- | Get the tid of the currently active thread after executing a
-- series of decisions. The list MUST begin with a 'Start', if it
-- doesn't an error will be thrown.
activeTid ::
    [Decision thread_id]
  -- ^ The sequence of decisions that have been made.
  -> thread_id
activeTid (Start tid:ds) = foldl' tidOf tid ds
activeTid _ = error "activeTid: first decision MUST be a 'Start'."

-------------------------------------------------------------------------------
-- Dynamic partial-order reduction

-- | DPOR execution is represented as a tree of states, characterised
-- by the decisions that lead to that state.
data DPOR thread_id thread_action = DPOR
  { dporRunnable :: Set thread_id
  -- ^ What threads are runnable at this step.
  , dporTodo     :: Map thread_id Bool
  -- ^ Follow-on decisions still to make, and whether that decision
  -- was added conservatively due to the bound.
  , dporDone     :: Map thread_id (DPOR thread_id thread_action)
  -- ^ Follow-on decisions that have been made.
  , dporSleep    :: Map thread_id thread_action
  -- ^ Transitions to ignore (in this node and children) until a
  -- dependent transition happens.
  , dporTaken    :: Map thread_id thread_action
  -- ^ Transitions which have been taken, excluding
  -- conservatively-added ones. This is used in implementing sleep
  -- sets.
  , dporAction   :: Maybe thread_action
  -- ^ What happened at this step. This will be 'Nothing' at the root,
  -- 'Just' everywhere else.
  }

instance ( NFData thread_id
         , NFData thread_action
         ) => NFData (DPOR thread_id thread_action) where
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
data BacktrackStep thread_id thread_action lookahead state = BacktrackStep
  { bcktThreadid   :: thread_id
  -- ^ The thread running at this step
  , bcktDecision   :: (Decision thread_id, thread_action)
  -- ^ What happened at this step.
  , bcktRunnable   :: Map thread_id lookahead
  -- ^ The threads runnable at this step
  , bcktBacktracks :: Map thread_id Bool
  -- ^ The list of alternative threads to run, and whether those
  -- alternatives were added conservatively due to the bound.
  , bcktState      :: state
  -- ^ Some domain-specific state at this point.
  } deriving Show

instance ( NFData thread_id
         , NFData thread_action
         , NFData lookahead
         , NFData state
         ) => NFData (BacktrackStep thread_id thread_action lookahead state) where
  rnf b = rnf ( bcktThreadid   b
              , bcktDecision   b
              , bcktRunnable   b
              , bcktBacktracks b
              , bcktState      b
              )

-- | Initial DPOR state, given an initial thread ID. This initial
-- thread should exist and be runnable at the start of execution.
initialState :: Ord thread_id => thread_id -> DPOR thread_id thread_action
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
findSchedulePrefix :: Ord thread_id
  => (thread_id -> Bool)
  -- ^ Some partitioning function, applied to the to-do decisions. If
  -- there is an identifier which passes the test, it will be used,
  -- rather than any which fail it. This allows a very basic way of
  -- domain-specific prioritisation between otherwise equal choices,
  -- which may be useful in some cases.
  -> DPOR thread_id thread_action
  -> Maybe ([thread_id], Bool, Map thread_id thread_action)
findSchedulePrefix predicate dporRoot = go (initialDPORThread dporRoot) dporRoot where
  go tid dpor =
        -- All the possible prefix traces from this point, with
        -- updated DPOR subtrees if taken from the done list.
    let prefixes = mapMaybe go' (M.toList $ dporDone dpor) ++ [([t], c, sleeps dpor) | (t, c) <- M.toList $ dporTodo dpor]
        -- Sort by number of preemptions, in descending order.
        cmp = Down . preEmps tid dpor . (\(a,_,_) -> a)

    in if null prefixes
       then Nothing
       else case partition (\(t:_,_,_) -> predicate t) $ sortBy (comparing cmp) prefixes of
              (choice:_, _)  -> Just choice
              ([], choice:_) -> Just choice
              ([], []) -> error "findSchedulePrefix: (internal error) empty prefix list!" 

  go' (tid, dpor) = (\(ts,c,slp) -> (tid:ts,c,slp)) <$> go tid dpor

  -- The new sleep set is the union of the sleep set of the node we're
  -- branching from, plus all the decisions we've already explored.
  sleeps dpor = dporSleep dpor `M.union` dporTaken dpor

  -- The number of pre-emptive context switches
  preEmps tid dpor (t:ts) =
    let rest = preEmps t (fromJust . M.lookup t $ dporDone dpor) ts
    in  if tid `S.member` dporRunnable dpor then 1 + rest else rest
  preEmps _ _ [] = 0::Int

-- | Add a new trace to the tree, creating a new subtree branching off
-- at the point where the \"to-do\" decision was made.
incorporateTrace :: Ord thread_id
  => state
  -- ^ Initial state
  -> (state -> thread_action -> state)
  -- ^ State step function
  -> (state -> (thread_id, thread_action) -> (thread_id, thread_action) -> Bool)
  -- ^ Dependency function
  -> Bool
  -- ^ Whether the \"to-do\" point which was used to create this new
  -- execution was conservative or not.
  -> [(Decision thread_id, [thread_id], thread_action)]
  -- ^ The execution trace: the decision made, the runnable threads,
  -- and the action performed.
  -> DPOR thread_id thread_action
  -> DPOR thread_id thread_action
incorporateTrace stinit ststep dependency conservative trace dporRoot = grow stinit (initialDPORThread dporRoot) trace dporRoot where
  grow state tid trc@((d, _, a):rest) dpor =
    let tid'   = tidOf tid d
        state' = ststep state a
    in  case M.lookup tid' $ dporDone dpor of
          Just dpor' -> dpor { dporDone  = M.insert tid' (grow state' tid' rest dpor') $ dporDone dpor }
          Nothing    -> dpor { dporTaken = if conservative then dporTaken dpor else M.insert tid' a $ dporTaken dpor
                            , dporTodo  = M.delete tid' $ dporTodo dpor
                            , dporDone  = M.insert tid' (subtree state' tid' (dporSleep dpor `M.union` dporTaken dpor) trc) $ dporDone dpor }
  grow _ _ [] dpor = dpor

  -- Construct a new subtree corresponding to a trace suffix.
  subtree state tid sleep ((_, _, a):rest) =
    let state' = ststep state a
        sleep' = M.filterWithKey (\t a' -> not $ dependency state' (tid, a) (t,a')) sleep
    in DPOR
        { dporRunnable = S.fromList $ case rest of
            ((_, runnable, _):_) -> runnable
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
  subtree _ _ _ [] = error "incorporateTrace: (internal error) subtree suffix empty!"

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
findBacktrackSteps :: Ord thread_id
  => state
  -- ^ Initial state.
  -> (state -> thread_action -> state)
  -- ^ State step function.
  -> (state -> (thread_id, thread_action) -> (thread_id, lookahead) -> Bool)
  -- ^ Dependency function.
  -> ([BacktrackStep thread_id thread_action lookahead state] -> Int -> thread_id -> [BacktrackStep thread_id thread_action lookahead state])
  -- ^ Backtracking function. Given a list of backtracking points, and
  -- a thread to backtrack to at a specific point in that list, add
  -- the new backtracking points. There will be at least one: this
  -- chosen one, but the function may add others.
  -> Seq (NonEmpty (thread_id, lookahead), [thread_id])
  -- ^ A sequence of threads at each step: the nonempty list of
  -- runnable threads (with lookahead values), and the list of threads
  -- still to try. The reason for the two separate lists is because
  -- the threads chosen to try will be dependent on the specific
  -- domain.
  -> [(Decision thread_id, thread_action)]
  -- ^ The execution trace.
  -> [BacktrackStep thread_id thread_action lookahead state]
findBacktrackSteps stinit ststep dependency backtrack bcktrck = go stinit S.empty initialThread [] (Sq.viewl bcktrck) where
  -- Get the initial thread ID
  initialThread = case Sq.viewl bcktrck of
    (((tid, _):|_, _):<_) -> tid
    _ -> error "findBacktrack: empty backtracking sequence."

  -- Walk through the traces one step at a time, building up a list of
  -- new backtracking points.
  go state allThreads tid bs ((e,i):<is) ((d,a):ts) =
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
incorporateBacktrackSteps :: Ord thread_id
  => ([(Decision thread_id, thread_action)] -> (Decision thread_id, lookahead) -> Bool)
  -- ^ Bound function: returns true if that schedule prefix terminated
  -- with the lookahead decision fits within the bound.
  -> [BacktrackStep thread_id thread_action lookahead state]
  -- ^ Backtracking steps identified by 'findBacktrackSteps'.
  -> DPOR thread_id thread_action
  -> DPOR thread_id thread_action
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
-- Utilities

-- The initial thread of a DPOR tree.
initialDPORThread :: DPOR thread_id thread_action -> thread_id
initialDPORThread = S.elemAt 0 . dporRunnable

-- | Render a 'DPOR' value as a graph in GraphViz \"dot\" format.
toDot
  :: (thread_id -> String)
  -- ^ Show a @thread_id@ - this should produce a string suitable for
  -- use as a node identifier.
  -> (thread_action -> String)
  -- ^ Show a @thread_action@.
  -> DPOR thread_id thread_action
  -> String
toDot = toDotFiltered (\_ _ -> True)

-- | Render a 'DPOR' value as a graph in GraphViz \"dot\" format, with
-- a function to determine if a subtree should be included or not.
toDotFiltered
  :: (thread_id -> DPOR thread_id thread_action -> Bool)
  -- ^ Subtree predicate.
  -> (thread_id     -> String)
  -> (thread_action -> String)
  -> DPOR thread_id thread_action
  -> String
toDotFiltered check showTid showAct dpor = "digraph {\n" ++ go "L" dpor ++ "\n}" where
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
  edge n1 n2 l = n1 ++ "-> " ++ n2 ++ " [label=\"" ++ showTid l ++ "\"]\n"

  -- Show a list of values
  showLst showf xs = "[" ++ intercalate ", " (map showf xs) ++ "]"

  -- Generate a graphviz-friendly identifier from a thread_id.
  tidId = concatMap (show . ord) . showTid

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Systematic testing of concurrent computations through dynamic
-- partial-order reduction and schedule bounding.
module Test.DPOR
  ( -- * Bounded dynamic partial-order reduction

  -- | We can characterise the state of a concurrent computation by
  -- considering the ordering of dependent events. This is a partial
  -- order: independent events can be performed in any order without
  -- affecting the result, and so are /not/ ordered.
  --
  -- Partial-order reduction is a technique for computing these
  -- partial orders, and only testing one total order for each partial
  -- order. This cuts down the amount of work to be done
  -- significantly. /Bounded/ partial-order reduction is a further
  -- optimisation, which only considers schedules within some bound.
  --
  -- This module provides a generic function for DPOR, parameterised
  -- by the actual (domain-specific) dependency function to use.
  --
  -- See /Bounded partial-order reduction/, K. Coons, M. Musuvathi,
  -- K. McKinley for more details.

    dpor
  , simpleDPOR
  , DPOR(..)

  -- ** Backtracking

  , BacktrackFunc
  , BacktrackStep(..)
  , backtrackAt

  -- ** Bounding

  , BoundFunc
  , (&+&)
  , trueBound

  -- *** Preemption

  -- | DPOR with preemption bounding. This adds conservative
  -- backtracking points at the prior context switch whenever a
  -- non-conervative backtracking point is added, as alternative
  -- decisions can influence the reachability of different states.
  --
  -- See the BPOR paper for more details.

  , PreemptionBound(..)
  , defaultPreemptionBound
  , preempBound
  , preempBacktrack
  , preempCount

  -- *** Fair

  -- | DPOR using fair bounding. This bounds the maximum difference
  -- between the number of yield operations different threads have
  -- performed.
  --
  -- See the DPOR paper for more details.

  , FairBound(..)
  , defaultFairBound
  , fairBound
  , fairBacktrack
  , yieldCount
  , maxYieldCountDiff

  -- *** Length

  -- | BPOR using length bounding. This bounds the maximum length (in
  -- terms of primitive actions) of an execution.

  , LengthBound(..)
  , defaultLengthBound
  , lenBound
  , lenBacktrack

  -- * Scheduling & execution traces

  -- | The partial-order reduction is driven by incorporating
  -- information gained from trial executions of the concurrent
  -- program.

  , DPORScheduler
  , SchedState
  , Trace

  , module Test.DPOR.Schedule
  ) where

import Control.DeepSeq (NFData)
import Data.List (nub)
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as M

import Test.DPOR.Internal
import Test.DPOR.Schedule

-------------------------------------------------------------------------------
-- Bounded dynamic partial-order reduction

-- | Dynamic partial-order reduction.
--
-- This takes a lot of functional parameters because it's so generic,
-- but most are fairly simple.
--
-- Some state may be maintained when determining backtracking points,
-- which can then inform the dependency functions. This state is not
-- preserved between different schedules, and built up from scratch
-- each time.
--
-- The dependency functions must be consistent: if we can convert
-- between @action@ and @lookahead@, and supply some sensible default
-- state, then (1) == true implies that (2) is. In practice, (1) is
-- the most specific and (2) will be more pessimistic (due to,
-- typically, less information being available when merely looking
-- ahead).
dpor :: ( Ord    tid
       , NFData tid
       , NFData action
       , NFData lookahead
       , NFData s
       , Monad  m
       )
  => (action    -> Bool)
  -- ^ Determine if a thread yielded.
  -> (lookahead -> Bool)
  -- ^ Determine if a thread will yield.
  -> s
  -- ^ The initial state for backtracking.
  -> (s -> (tid, action) -> s)
  -- ^ The backtracking state step function.
  -> (s -> (tid, action) -> (tid, action)    -> Bool)
  -- ^ The dependency (1) function.
  -> (s -> (tid, action) -> (tid, lookahead) -> Bool)
  -- ^ The dependency (2) function.
  -> tid
  -- ^ The initial thread.
  -> (tid -> Bool)
  -- ^ The thread partitioning function: when choosing what to
  -- execute, prefer threads which return true.
  -> BoundFunc tid action lookahead
  -- ^ The bounding function.
  -> BacktrackFunc tid action lookahead s
  -- ^ The backtracking function. Note that, for some bounding
  -- functions, this will need to add conservative backtracking
  -- points.
  -> (DPOR tid action -> DPOR tid action)
  -- ^ Some post-processing to do after adding the new to-do points.
  -> (DPORScheduler tid action lookahead s
    -> SchedState tid action lookahead s
    -> m (a, SchedState tid action lookahead s, Trace tid action lookahead))
  -- ^ The runner: given the scheduler and state, execute the
  -- computation under that scheduler.
  -> m [(a, Trace tid action lookahead)]
dpor didYield
     willYield
     stinit
     ststep
     dependency1
     dependency2
     initialTid
     predicate
     inBound
     backtrack
     transform
     run
  = go (initialState initialTid)

  where
    -- Repeatedly run the computation gathering all the results and
    -- traces into a list until there are no schedules remaining to
    -- try.
    go dp = case nextPrefix dp of
      Just (prefix, conservative, sleep) -> do
        (res, s, trace) <- run scheduler
                              (initialSchedState stinit sleep prefix)

        let bpoints = findBacktracks s trace
        let newDPOR = addTrace conservative trace dp

        if schedIgnore s
        then go newDPOR
        else ((res, trace):) <$> go (transform $ addBacktracks bpoints newDPOR)

      Nothing -> pure []

    -- Find the next schedule prefix.
    nextPrefix = findSchedulePrefix predicate

    -- The DPOR scheduler.
    scheduler = dporSched didYield willYield dependency1 ststep inBound

    -- Find the new backtracking steps.
    findBacktracks = findBacktrackSteps stinit ststep dependency2 backtrack .
                     schedBPoints

    -- Incorporate a trace into the DPOR tree.
    addTrace = incorporateTrace stinit ststep dependency1

    -- Incorporate the new backtracking steps into the DPOR tree.
    addBacktracks = incorporateBacktrackSteps inBound

-- | A much simplified DPOR function: no state, no preference between
-- threads, and no post-processing between iterations.
simpleDPOR :: ( Ord    tid
             , NFData tid
             , NFData action
             , NFData lookahead
             , Monad  m
             )
  => (action    -> Bool)
  -- ^ Determine if a thread yielded.
  -> (lookahead -> Bool)
  -- ^ Determine if a thread will yield.
  -> ((tid, action) -> (tid, action)    -> Bool)
  -- ^ The dependency (1) function.
  -> ((tid, action) -> (tid, lookahead) -> Bool)
  -- ^ The dependency (2) function.
  -> tid
  -- ^ The initial thread.
  -> BoundFunc tid action lookahead
  -- ^ The bounding function.
  -> BacktrackFunc tid action lookahead ()
  -- ^ The backtracking function. Note that, for some bounding
  -- functions, this will need to add conservative backtracking
  -- points.
  -> (DPORScheduler tid action lookahead ()
    -> SchedState tid action lookahead ()
    -> m (a, SchedState tid action lookahead (), Trace tid action lookahead))
  -- ^ The runner: given the scheduler and state, execute the
  -- computation under that scheduler.
  -> m [(a, Trace tid action lookahead)]
simpleDPOR didYield
           willYield
           dependency1
           dependency2
           initialTid
           inBound
           backtrack
  = dpor didYield
         willYield
         ()
         (\_ _ -> ())
         (const dependency1)
         (const dependency2)
         initialTid
         (const True)
         inBound
         backtrack
         id

-- | Add a backtracking point. If the thread isn't runnable, add all
-- runnable threads. If the backtracking point is already present,
-- don't re-add it UNLESS this would make it conservative.
backtrackAt :: Ord tid
  => (BacktrackStep tid action lookahead s -> Bool)
  -- ^ If this returns @True@, backtrack to all runnable threads,
  -- rather than just the given thread.
  -> Bool
  -- ^ Is this backtracking point conservative? Conservative points
  -- are always explored, whereas non-conservative ones might be
  -- skipped based on future information.
  -> BacktrackFunc tid action lookahead s
backtrackAt toAll conservative bs i tid = go bs i where
  go bx@(b:rest) 0
    -- If the backtracking point is already present, don't re-add it,
    -- UNLESS this would force it to backtrack (it's conservative)
    -- where before it might not.
    | not (toAll b) && tid `M.member` bcktRunnable b =
      let val = M.lookup tid $ bcktBacktracks b
      in if isNothing val || (val == Just False && conservative)
         then b { bcktBacktracks = backtrackTo b } : rest
         else bx

    -- Otherwise just backtrack to everything runnable.
    | otherwise = b { bcktBacktracks = backtrackAll b } : rest

  go (b:rest) n = b : go rest (n-1)
  go [] _ = error "backtrackAt: Ran out of schedule whilst backtracking!"

  -- Backtrack to a single thread
  backtrackTo = M.insert tid conservative . bcktBacktracks

  -- Backtrack to all runnable threads
  backtrackAll = M.map (const conservative) . bcktRunnable

-------------------------------------------------------------------------------
-- Bounds

-- | Combine two bounds into a larger bound, where both must be
-- satisfied.
(&+&) :: BoundFunc tid action lookahead
      -> BoundFunc tid action lookahead
      -> BoundFunc tid action lookahead
(&+&) b1 b2 ts dl = b1 ts dl && b2 ts dl

-- | The \"true\" bound, which allows everything.
trueBound :: BoundFunc tid action lookahead
trueBound _ _ = True

-------------------------------------------------------------------------------
-- Preemption bounding

newtype PreemptionBound = PreemptionBound Int
  deriving (NFData, Enum, Eq, Ord, Num, Real, Integral, Read, Show)

-- | A sensible default preemption bound: 2.
--
-- See /Concurrency Testing Using Schedule Bounding: an Empirical Study/,
-- P. Thomson, A. F. Donaldson, A. Betts for justification.
defaultPreemptionBound :: PreemptionBound
defaultPreemptionBound = 2

-- | Preemption bound function
preempBound :: (action -> Bool)
  -- ^ Determine if a thread yielded.
  -> PreemptionBound
  -> BoundFunc tid action lookahead
preempBound didYield (PreemptionBound pb) ts dl =
  preempCount didYield ts dl <= pb

-- | Add a backtrack point, and also conservatively add one prior to
-- the most recent transition before that point. This may result in
-- the same state being reached multiple times, but is needed because
-- of the artificial dependency imposed by the bound.
preempBacktrack :: Ord tid
  => (action -> Bool)
  -- ^ If this is true of the action at a preemptive context switch,
  -- do NOT use that point for the conservative point, try earlier.
  -> BacktrackFunc tid action lookahead s
preempBacktrack ignore bs i tid =
  maybe id (\j' b -> backtrack True b j' tid) j $ backtrack False bs i tid

  where
    -- Index of the conservative point
    j = goJ . reverse . pairs $ zip [0..i-1] bs where
      goJ (((_,b1), (j',b2)):rest)
        | bcktThreadid b1 /= bcktThreadid b2
          && not (ignore . snd $ bcktDecision b1)
          && not (ignore . snd $ bcktDecision b2) = Just j'
        | otherwise = goJ rest
      goJ [] = Nothing

    -- List of adjacent pairs
    {-# INLINE pairs #-}
    pairs = zip <*> tail

    -- Add a backtracking point.
    backtrack = backtrackAt $ const False

-- | Count the number of preemptions in a schedule prefix.
preempCount :: (action -> Bool)
  -- ^ Determine if a thread yielded.
  -> [(Decision tid, action)]
  -- ^ The schedule prefix.
  -> (Decision tid, lookahead)
  -- ^ The to-do point.
  -> Int
preempCount didYield ts (d, _) = go Nothing ts where
  go p ((d', a):rest) = preempC p d' + go (Just a) rest
  go p [] = preempC p d

  preempC (Just act) (SwitchTo _) | didYield act = 0
  preempC _ (SwitchTo _) = 1
  preempC _ _ = 0

-------------------------------------------------------------------------------
-- Fair bounding

newtype FairBound = FairBound Int
  deriving (NFData, Enum, Eq, Ord, Num, Real, Integral, Read, Show)

-- | A sensible default fair bound: 5.
--
-- This comes from playing around myself, but there is probably a
-- better default.
defaultFairBound :: FairBound
defaultFairBound = 5

-- | Fair bound function
fairBound :: Eq tid
  => (action -> Bool)
  -- ^ Determine if a thread yielded.
  -> (lookahead -> Bool)
  -- ^ Determine if a thread will yield.
  -> (action -> [tid])
  -- ^ The new threads an action causes to come into existence.
  -> FairBound -> BoundFunc tid action lookahead
fairBound didYield willYield forkTids (FairBound fb) ts dl =
  maxYieldCountDiff didYield willYield forkTids ts dl <= fb

-- | Add a backtrack point. If the thread isn't runnable, or performs
-- a release operation, add all runnable threads.
fairBacktrack :: Ord tid
  => (lookahead -> Bool)
  -- ^ Determine if an action is a release operation: if it could
  -- cause other threads to become runnable.
  -> BacktrackFunc tid action lookahead s
fairBacktrack willRelease bs i t = backtrackAt check False bs i t where
  -- True if a release operation is performed.
  check b = Just True == (willRelease <$> M.lookup t (bcktRunnable b))

-- | Count the number of yields by a thread in a schedule prefix.
yieldCount :: Eq tid
  => (action -> Bool)
  -- ^ Determine if a thread yielded.
  -> (lookahead -> Bool)
  -- ^ Determine if a thread will yield.
  -> tid
  -- ^ The thread to count yields for.
  -> [(Decision tid, action)] -> (Decision tid, lookahead) -> Int
yieldCount didYield willYield tid ts (ld, l) = go initialThread ts where
  go t ((Start    t', act):rest) = go' t t' act rest
  go t ((SwitchTo t', act):rest) = go' t t' act rest
  go t ((Continue,    act):rest) = go' t t  act rest
  go t []
    | t == tid && willYield l = 1
    | otherwise = 0

  go' t t' act rest
    | t == tid && didYield act = 1 + go t' rest
    | otherwise = go t' rest

  -- The initial thread ID
  initialThread = case (ts, ld) of
    ((Start t, _):_, _) -> t
    ([], Start t)  -> t
    _ -> error "yieldCount: unknown initial thread."

-- | Get the maximum difference between the yield counts of all
-- threads in this schedule prefix.
maxYieldCountDiff :: Eq tid
  => (action -> Bool)
  -- ^ Determine if a thread yielded.
  -> (lookahead -> Bool)
  -- ^ Determine if a thread will yield.
  -> (action -> [tid])
  -- ^ The new threads an action causes to come into existence.
  -> [(Decision tid, action)] -> (Decision tid, lookahead) -> Int
maxYieldCountDiff didYield willYield forkTids ts dl = maximum yieldCountDiffs
  where
    yieldsBy tid = yieldCount didYield willYield tid ts dl
    yieldCounts = [yieldsBy tid | tid <- nub $ allTids ts]
    yieldCountDiffs = [y1 - y2 | y1 <- yieldCounts, y2 <- yieldCounts]

    -- All the threads created during the lifetime of the system.
    allTids ((_, act):rest) =
      let tids' = forkTids act
      in if null tids' then allTids rest else tids' ++ allTids rest
    allTids [] = [initialThread]

    -- The initial thread ID
    initialThread = case (ts, dl) of
      ((Start t, _):_, _) -> t
      ([], (Start t, _))  -> t
      _ -> error "maxYieldCountDiff: unknown initial thread."

-------------------------------------------------------------------------------
-- Length bounding

newtype LengthBound = LengthBound Int
  deriving (NFData, Enum, Eq, Ord, Num, Real, Integral, Read, Show)

-- | A sensible default length bound: 250.
--
-- Based on the assumption that anything which executes for much
-- longer (or even this long) will take ages to test.
defaultLengthBound :: LengthBound
defaultLengthBound = 250

-- | Length bound function
lenBound :: LengthBound -> BoundFunc tid action lookahead
lenBound (LengthBound lb) ts _ = length ts < lb

-- | Add a backtrack point. If the thread isn't runnable, add all
-- runnable threads.
lenBacktrack :: Ord tid => BacktrackFunc tid action lookahead s
lenBacktrack = backtrackAt (const False) False

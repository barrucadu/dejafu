{-# LANGUAGE Rank2Types  #-}

-- | A runner for concurrent monads to systematically detect
-- concurrency errors such as data races and deadlocks: internal definitions.
module Test.DejaFu.SCT.Internal where

import Control.Monad.Loops (unfoldrM)
import Data.List (unfoldr)
import Test.DejaFu.Deterministic
import Test.DejaFu.Deterministic.IO (ConcIO, runConcIO)

-- * SCT Runners

-- | Run a concurrent program under a given scheduler a number of
-- times, collecting the results and the trace that gave rise to them.
--
-- The initial state for each run is the final state of the prior run,
-- so it is important that the scheduler actually maintain some
-- internal state, or all the results will be identical.
runSCT :: Scheduler s -- ^ The scheduler
       -> s -- ^ The scheduler's initial state
       -> Int -- ^ The number of executions to perform
       -> (forall t. Conc t a) -- ^ The computation to test
       -> [(Maybe a, Trace)]
runSCT sched s n = runSCT' sched (s, n) term step where
  term (_,  g) = g == 0
  step (s', g) _ = (s', g - 1)

-- | Variant of 'runSCT' for computations which do 'IO'.
runSCTIO :: Scheduler s -> s -> Int -> (forall t. ConcIO t a) -> IO [(Maybe a, Trace)]
runSCTIO sched s n = runSCTIO' sched (s, n) term step where
  term (_,  g) = g == 0
  step (s', g) _ = (s', g - 1)

-- | Run a concurrent program under a given scheduler, where the SCT
-- runner itself maintains some internal state, and has a function to
-- produce a new scheduler state for each run, and decide termination
-- based on the internal state.
--
-- Note: the state step function takes the state returned by the
-- scheduler, not the initial state!
runSCT' :: Scheduler s -- ^ The scheduler
        -> (s, g) -- ^ The scheduler's and runner's initial states
        -> ((s, g) -> Bool) -- ^ Termination decider
        -> ((s, g) -> Trace -> (s, g)) -- ^ State step function
        -> (forall t. Conc t a) -- ^ The computation to test
        -> [(Maybe a, Trace)]
runSCT' sched initial term step c = unfoldr go initial where
  go sg@(s, g)
    | term sg   = Nothing
    | otherwise = res `seq` Just ((res, trace), sg')

    where
      (res, s', trace) = runConc sched s c

      sg' = step (s', g) trace

-- | Variant of 'runSCT'' for computations which do 'IO'.
runSCTIO' :: Scheduler s -> (s, g) -> ((s, g) -> Bool) -> ((s, g) -> Trace -> (s, g)) -> (forall t. ConcIO t a) -> IO [(Maybe a, Trace)]
runSCTIO' sched initial term step c = unfoldrM go initial where
  go sg@(s, g)
    | term sg   = return Nothing
    | otherwise = do
      (res, s', trace) <- runConcIO sched s c

      let sg' = step (s', g) trace

      res `seq` return (Just ((res, trace), sg'))

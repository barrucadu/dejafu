{-# LANGUAGE RankNTypes  #-}

-- | A runner for concurrent monads to systematically detect
-- concurrency errors such as data races and deadlocks: internal definitions.
module Control.Monad.Conc.SCT.Internal where

import Control.Monad.Conc.Fixed

import qualified Control.Monad.Conc.Fixed.IO as CIO

-- * Types

-- | An @SCTScheduler@ is like a regular 'Scheduler', except it builds
-- a trace of scheduling decisions made.
--
-- Note that the 'SchedTrace' is built in *reverse*, this is more
-- efficient than appending to the list every time.
type SCTScheduler s = Scheduler (s, SchedTrace)

-- | A @SchedTrace@ is just a list of all the decisions that were made,
-- with the alternative decisions that could have been made at each
-- step.
type SchedTrace = [(Decision, [Decision])]

-- | A @SCTTrace@ is a combined 'SchedTrace' and 'Trace'.
type SCTTrace = [(Decision, [Decision], ThreadAction)]

-- | Scheduling decisions are based on the state of the running
-- program, and so we can capture some of that state in recording what
-- specific decision we made.
data Decision =
    Start ThreadId
  -- ^ Start a new thread, because the last was blocked (or it's the
  -- initial thread).
  | Continue
  -- ^ Continue running the last thread for another step.
  | SwitchTo ThreadId
  -- ^ Pre-empt the running thread, and switch to another.
  deriving (Eq, Ord, Show)

-- * SCT Runners

-- | Run a concurrent program under a given scheduler a number of
-- times, collecting the results and the scheduling that gave rise to
-- them.
--
-- The initial state for each run is the final state of the last run,
-- so it is important that the scheduler actually maintain some
-- internal state, or all the results will be identical.
runSCT :: SCTScheduler s -> s -> Int -> (forall t. Conc t a) -> [(Maybe a, SCTTrace)]
runSCT sched s n = runSCT' sched s n term step where
  term _  g = g == 0
  step s' g _ = (s', g - 1)

-- | A varant of 'runSCT' for concurrent programs that do 'IO'.
--
-- Warning! The IO will be executed lots of times, in lots of
-- interleavings! Be very confident that nothing in a 'liftIO' can
-- block on the action of another thread, or you risk deadlocking this
-- function!
runSCTIO :: SCTScheduler s -> s -> Int -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
runSCTIO sched s n = runSCTIO' sched s n term step where
  term _  g = g == 0
  step s' g _ = (s', g - 1)

-- | Run a concurrent program under a given scheduler, where the SCT
-- runner itself maintains some internal state, and has a function to
-- produce a new scheduler state for each run, and decide termination
-- based on the internal state.
--
-- Note: the state step function takes the state returned by the
-- scheduler, not the initial state!
runSCT' :: SCTScheduler s -- ^ The scheduler
        -> s -- ^ The scheduler's initial satte
        -> g -- ^ The runner's initial state
        -> (s -> g -> Bool) -- ^ Termination decider
        -> (s -> g -> SCTTrace -> (s, g)) -- ^ State step function
        -> (forall t. Conc t a) -- ^ Conc program
        -> [(Maybe a, SCTTrace)]
runSCT' sched s g term step c
  | term s g = []
  | otherwise = (res, trace) : rest where

  (res, (s', strace), ttrace) = runConc' sched (s, [(Start 0, [])]) c

  trace = reverse $ scttrace strace ttrace

  (s'', g') = step s' g trace

  rest = runSCT' sched s'' g' term step c

-- | A variant of runSCT' for concurrent programs that do IO.
--
-- Warning! The IO will be executed lots of times, in lots of
-- interleavings! Be very confident that nothing in a 'liftIO' can
-- block on the action of another thread, or you risk deadlocking this
-- function!
runSCTIO' :: SCTScheduler s -> s -> g -> (s -> g -> Bool) -> (s -> g -> SCTTrace -> (s, g)) -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
runSCTIO' sched s g term step c
  | term s g = return []
  | otherwise = do
    (res, (s', strace), ttrace) <- CIO.runConc' sched (s, [(Start 0, [])]) c

    let trace = reverse $ scttrace strace ttrace
    let (s'', g') = step s' g trace

    rest <- runSCTIO' sched s'' g' term step c

    return $ (res, trace) : rest

-- * Utils (Internal)

-- | Zip a list of 'SchedTrace's and a 'Trace' together into an
-- 'SCTTrace'.
scttrace :: SchedTrace -> Trace -> SCTTrace
scttrace = zipWith $ \(d, alts) (_, act) -> (d, alts, act)

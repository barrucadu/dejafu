{-# LANGUAGE Rank2Types  #-}

-- | A runner for concurrent monads to systematically detect
-- concurrency errors such as data races and deadlocks: internal definitions.
module Control.Monad.Conc.SCT.Internal where

import Control.DeepSeq (NFData(..))
import Control.Monad (liftM)
import Control.Monad.Conc.Fixed
import Data.List (unfoldr)

import qualified Control.Monad.Conc.Fixed.IO as CIO

-- * Types

-- | An @SCTScheduler@ is like a regular 'Scheduler', except it builds
-- a trace of scheduling decisions made.
--
-- If implementing your own scheduler, note that the 'SchedTrace' is
-- built in reverse, as this is more efficient than appending to the
-- list every time, also note that the scheduler is not called before
-- starting the first thread, as there is only one possible decision.
type SCTScheduler s = Scheduler (s, SchedTrace)

-- | A @SchedTrace@ is just a list of all the decisions that were made,
-- with the alternative decisions that could have been made at each
-- step.
type SchedTrace = [(Decision, [Decision])]

-- | An @SCTTrace@ is a combined 'SchedTrace' and 'Trace'.
type SCTTrace = [(Decision, [Decision], ThreadAction)]

-- | Scheduling decisions are based on the state of the running
-- program, and so we can capture some of that state in recording what
-- specific decision we made.
data Decision =
    Start ThreadId
  -- ^ Start a new thread, because the last was blocked (or it's the
  -- start of computation).
  | Continue
  -- ^ Continue running the last thread for another step.
  | SwitchTo ThreadId
  -- ^ Pre-empt the running thread, and switch to another.
  deriving (Eq, Show)

instance NFData Decision where
  rnf (Start tid) = rnf tid
  rnf  Continue = ()
  rnf (SwitchTo tid) = rnf tid

-- * SCT Runners

-- | Run a concurrent program under a given scheduler a number of
-- times, collecting the results and the scheduling that gave rise to
-- them.
--
-- The initial state for each run is the final state of the last run,
-- so it is important that the scheduler actually maintain some
-- internal state, or all the results will be identical.
runSCT :: SCTScheduler s -> s -> Int -> (forall t. Conc t a) -> [(Maybe a, SCTTrace)]
runSCT sched s n = runSCT' sched (s, n) term step where
  term (_,  g) = g == 0
  step (s', g) _ = (s', g - 1)

-- | A varant of 'runSCT' for concurrent programs that do 'IO'.
--
-- Warning! The IO will be executed lots of times, in lots of
-- interleavings! Be very confident that nothing in a 'liftIO' can
-- block on the action of another thread, or you risk deadlocking this
-- function!
runSCTIO :: SCTScheduler s -> s -> Int -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
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
runSCT' :: SCTScheduler s -- ^ The scheduler
        -> (s, g) -- ^ The scheduler's and runner's initial states
        -> ((s, g) -> Bool) -- ^ Termination decider
        -> ((s, g) -> SCTTrace -> (s, g)) -- ^ State step function
        -> (forall t. Conc t a) -- ^ Conc program
        -> [(Maybe a, SCTTrace)]
runSCT' sched initial term step c = unfoldr go initial where
  go sg@(s, g)
    | term sg   = Nothing
    | otherwise = res `seq` Just ((res, trace), sg')

    where
      (res, (s', strace), ttrace) = runConc' sched (s, initialTrace) c

      trace = reverse $ scttrace strace ttrace

      sg' = step (s', g) trace

-- | A variant of runSCT' for concurrent programs that do IO.
--
-- Warning! The IO will be executed lots of times, in lots of
-- interleavings! Be very confident that nothing in a 'liftIO' can
-- block on the action of another thread, or you risk deadlocking this
-- function!
runSCTIO' :: SCTScheduler s -> (s, g) -> ((s, g) -> Bool) -> ((s, g) -> SCTTrace -> (s, g)) -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
runSCTIO' sched initial term step c = unfoldrM go initial where
  go sg@(s, g)
    | term sg   = return Nothing
    | otherwise = do
      (res, (s', strace), ttrace) <- CIO.runConc' sched (s, initialTrace) c

      let trace = reverse $ scttrace strace ttrace
      let sg' = step (s', g) trace

      res `seq` return (Just ((res, trace), sg'))

-- | Like 'unfoldr', but monadic.
unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f b = f b >>= maybe (return []) (\(a, b') -> (a:) `liftM` unfoldrM f b')

-- * Utils (Internal)

-- | Zip a list of 'SchedTrace's and a 'Trace' together into an
-- 'SCTTrace'.
scttrace :: SchedTrace -> Trace -> SCTTrace
scttrace = zipWith $ \(d, alts) (_, act) -> (d, alts, act)

-- | The initial trace of a @Conc@ computation.
initialTrace :: SchedTrace
initialTrace = [(Start 0, [])]

{-# LANGUAGE RankNTypes  #-}

-- | A runner for concurrent monads to systematically detect
-- concurrency errors such as data races and deadlocks.
--
-- As an example, consider this program, which has two locks and a
-- shared variable. Two threads are spawned, which claim the locks,
-- update the shared variable, and release the locks. The main thread
-- waits for them both to terminate, and returns the final result.
--
-- > bad :: ConcCVar cvar m => m Int
-- > bad = do
-- >   a <- newEmptyCVar
-- >   b <- newEmptyCVar
-- > 
-- >   c <- newCVar 0
-- > 
-- >   j1 <- spawn $ lock a >> lock b >> modifyCVar_ c (return . succ) >> unlock b >> unlock a
-- >   j2 <- spawn $ lock b >> lock a >> modifyCVar_ c (return . pred) >> unlock a >> unlock b
-- > 
-- >   takeCVar j1
-- >   takeCVar j2
-- > 
-- >   takeCVar c
--
-- The correct result is 0, as it starts out as 0 and is incremented
-- and decremented by threads 1 and 2, respectively. However, note the
-- order of acquisition of the locks in the two threads. If thread 2
-- pre-empts thread 1 between the acquisition of the locks (or if
-- thread 1 pre-empts thread 2), a deadlock situation will arise, as
-- thread 1 will have lock `a` and be waiting on `b`, and thread 2
-- will have `b` and be waiting on `a`.

module Control.Monad.Conc.SCT
 ( -- *Systematic Concurrency Testing
   SCTScheduler
 , SchedTrace
 , SCTTrace
 , Decision(..)
 , runSCT
 , runSCTIO
 , runSCT'
 , runSCTIO'

 -- * Schedulers
 , sctRandom
 , sctRandomNP

 -- * Utilities
 , toSCT
 , showTrace
 ) where

import Control.Monad.Conc.Fixed
import System.Random (RandomGen)

import qualified Control.Monad.Conc.Fixed.IO as CIO

-- | An @SCTScheduler@ is like a regular 'Scheduler', except it builds
-- a trace of scheduling decisions made.
type SCTScheduler s = Scheduler (s, SchedTrace)

-- | A @SchedTrace@ is just a list of all the decisions that were made,
-- with the alternative decisions that could have been made at each
-- step
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
  deriving (Eq, Show)

-- | Run a concurrent program under a given scheduler a number of
-- times, collecting the results and the scheduling that gave rise to
-- them.
--
-- The initial state for each run is the final state of the last run,
-- so it is important that the scheduler actually maintain some
-- internal state, or all the results will be identical.
runSCT :: SCTScheduler s -> s -> Int -> (forall t. Conc t a) -> [(Maybe a, SCTTrace)]
runSCT sched s n = runSCT' sched s n term step where
  term (_, g) = g == 0
  step (s, g) = (s, g - 1)

-- | A varant of 'runSCT' for concurrent programs that do 'IO'.
--
-- Warning! The IO will be executed lots of times, in lots of
-- interleavings! Be very confident that nothing in a 'liftIO' can
-- block on the action of another thread, or you risk deadlocking this
-- function!
runSCTIO :: SCTScheduler s -> s -> Int -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
runSCTIO sched s n = runSCTIO' sched s n term step where
  term (_, g) = g == 0
  step (s, g) = (s, g - 1)

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
        -> ((s, g) -> Bool) -- ^ Termination decider
        -> ((s, g) -> (s, g)) -- ^ State step function
        -> (forall t. Conc t a) -- ^ Conc program
        -> [(Maybe a, SCTTrace)]
runSCT' sched s g term step c
  | term (s, g) = []
  | otherwise = (res, scttrace strace ttrace) : rest where

  (res, (s', strace), ttrace) = runConc' sched (s, [(Start 0, [])]) c

  (s'', g') = step (s', g)

  rest = runSCT' sched s'' g' term step c

-- | A variant of runSCT' for concurrent programs that do IO.
--
-- Warning! The IO will be executed lots of times, in lots of
-- interleavings! Be very confident that nothing in a 'liftIO' can
-- block on the action of another thread, or you risk deadlocking this
-- function!
runSCTIO' :: SCTScheduler s -> s -> g -> ((s, g) -> Bool) -> ((s, g) -> (s, g)) -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
runSCTIO' sched s g term step c
  | term (s, g) = return []
  | otherwise = do
    (res, (s', strace), ttrace) <- CIO.runConc' sched (s, [(Start 0, [])]) c

    let (s'', g') = step (s', g)

    rest <- runSCTIO' sched s'' g' term step c

    return $ (res, scttrace strace ttrace) : rest

-- | Zip a list of 'SchedTrace's and a 'Trace' together into an
-- 'SCTTrace'.
scttrace :: SchedTrace -> Trace -> SCTTrace
scttrace = zipWith $ \(d, alts) (_, act) -> (d, alts, act)

-- | A simple pre-emptive random scheduler.
sctRandom :: RandomGen g => SCTScheduler g
sctRandom = toSCT randomSched

-- | A random scheduler with no pre-emption.
sctRandomNP :: RandomGen g => SCTScheduler g
sctRandomNP = toSCT randomSchedNP

-- | Convert a 'Scheduler' to an 'SCTScheduler' by recording the
-- trace.
toSCT :: Scheduler s -> SCTScheduler s
toSCT sched (s, trace) prior threads = (tid, (s', trace ++ [(decision, alters)])) where
  (tid, s') = sched s prior threads

  decision | tid == prior         = Continue
           | prior `elem` threads = SwitchTo tid
           | otherwise            = Start tid

  alters | tid == prior         = map SwitchTo $ filter (/=prior) threads
         | prior `elem` threads = Continue : map SwitchTo (filter (\t -> t /= prior && t /= tid) threads)
         | otherwise            = map Start $ filter (/=tid) threads

-- | Pretty-print a scheduler trace.
showTrace :: SchedTrace -> String
showTrace = trace "" 0 . map fst where
    trace prefix num (Start tid:ds)    = thread prefix num ++ trace ("S" ++ show tid) 1 ds
    trace prefix num (SwitchTo tid:ds) = thread prefix num ++ trace ("P" ++ show tid) 1 ds
    trace prefix num (Continue:ds)     = trace prefix (num + 1) ds
    trace prefix num []                = thread prefix num

    thread prefix num = prefix ++ replicate num '-'

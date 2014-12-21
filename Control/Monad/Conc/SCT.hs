{-# LANGUAGE RankNTypes  #-}

-- | A runner for concurrent monads to systematically detect
-- concurrency errors such as data races and deadlocks.
--
-- As an example, consider this program, which has two locks and a
-- shared variable. Two threads are spawned, which claim the locks,
-- update the shared variable, and release the locks. The main thread
-- waits for them both to terminate, and returns the final result.
--
-- > bad :: ConcCVar (cvar t) (m t) => m t Int
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
 , Decision(..)
 , runSCT
 , sctRandom
 , showTrace
 ) where

import Control.Monad.Conc.Fixed
import System.Random (RandomGen, randomR)

-- | An @SCTScheduler@ is like a regular 'Scheduler', except it builds
-- a log of scheduling decisions made.
type SCTScheduler s = Scheduler (s, [Decision])

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
runSCT :: SCTScheduler s -> s -> Int -> (forall t. Conc t a) -> IO [(Maybe a, [Decision])]
runSCT sched s runs c = runSCT' s runs where
  runSCT' _ 0 = return []
  runSCT' s n = do
    (res, (s', log)) <- runConc' sched (s, [Start 0]) c
    rest <- runSCT' s' $ n - 1
    return $ (res, log) : rest

-- | A simple pre-emptive random scheduler.
sctRandom :: RandomGen g => SCTScheduler g
sctRandom (g, log) last threads = (tid, (g', log ++ [decision])) where
  (choice, g') = randomR (0, length threads - 1) g
  tid = threads !! choice
  decision | tid == last         = Continue
           | last `elem` threads = SwitchTo tid
           | otherwise           = Start tid

-- | Pretty-print a scheduler trace.
showTrace :: [Decision] -> String
showTrace = trace "" 0 where
    trace log num (Start tid:ds)    = thread log num ++ trace ("S" ++ show tid) 1 ds
    trace log num (Continue:ds)     = trace log (num + 1) ds
    trace log num (SwitchTo tid:ds) = thread log num ++ trace ("P" ++ show tid) 1 ds
    trace log num []                = thread log num

    thread "" _    = ""
    thread log num = log ++ replicate num '-'

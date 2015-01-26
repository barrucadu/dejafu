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
-- thread 1 will have lock @a@ and be waiting on @b@, and thread 2
-- will have @b@ and be waiting on @a@.

module Control.Monad.Conc.SCT
 ( -- * Types
   SCTScheduler
 , SchedTrace
 , SCTTrace
 , Decision(..)

-- * SCT Runners
 , runSCT
 , runSCTIO
 , runSCT'
 , runSCTIO'

 -- * Random Schedulers
 , sctRandom
 , sctRandomNP

 -- * Schedule Bounding
 -- | Schedule bounding is a means of cutting down the search space of
 -- schedules, by taking advantage of some intrinsic properties of
 -- schedules: such as the number of pre-emptions (pre-emption
 -- bounding), or the number of deviations from a deterministic
 -- scheduler (delay bounding); and then exploring all schedules
 -- within the bound.
 , sctBounded
 , sctBoundedIO
 , sctPreBound
 , sctPreBoundIO
 , sctDelayBound
 , sctDelayBoundIO

 -- * Utilities
 , makeSCT
 , preEmpCount
 , showTrace
 ) where

import Control.Monad.Conc.Fixed
import Control.Monad.Conc.SCT.Internal
import Control.Monad.Conc.SCT.Bounding
import System.Random (RandomGen)

-- * Random Schedulers

-- | A simple pre-emptive random scheduler.
sctRandom :: RandomGen g => SCTScheduler g
sctRandom = makeSCT randomSched

-- | A random scheduler with no pre-emption.
sctRandomNP :: RandomGen g => SCTScheduler g
sctRandomNP = makeSCT randomSchedNP

-- * Utils

-- | Check the pre-emption count of some scheduling decisions.
preEmpCount :: [Decision] -> Int
preEmpCount (SwitchTo _:ss) = 1 + preEmpCount ss
preEmpCount (_:ss) = preEmpCount ss
preEmpCount [] = 0

-- | Pretty-print a scheduler trace.
showTrace :: SchedTrace -> String
showTrace = trace "" 0 . map fst where
  trace prefix num (Start tid:ds)    = thread prefix num ++ trace ("S" ++ show tid) 1 ds
  trace prefix num (SwitchTo tid:ds) = thread prefix num ++ trace ("P" ++ show tid) 1 ds
  trace prefix num (Continue:ds)     = trace prefix (num + 1) ds
  trace prefix num []                = thread prefix num

  thread prefix num = prefix ++ replicate num '-'

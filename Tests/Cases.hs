module Tests.Cases where

import Control.Monad.Conc.Class
import Control.Monad.Conc.CVar
import Tests.Utils

-- | List of all tests
testCases :: [Test]
testCases =
  [ Test "Simple 2-Deadlock" $ testNot "No deadlocks found!" $ testDeadlockFree 100 simple2Deadlock
  ]

-- | Should deadlock on a minority of schedules.
simple2Deadlock :: ConcCVar cvar m => m Int
simple2Deadlock = do
  a <- newEmptyCVar
  b <- newEmptyCVar

  c <- newCVar 0

  j1 <- spawn $ lock a >> lock b >> modifyCVar_ c (return . succ) >> unlock b >> unlock a
  j2 <- spawn $ lock b >> lock a >> modifyCVar_ c (return . pred) >> unlock a >> unlock b

  takeCVar j1
  takeCVar j2

  takeCVar c

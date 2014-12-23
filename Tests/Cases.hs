module Tests.Cases where

import Control.Monad (replicateM)
import Control.Monad.Conc.Class
import Control.Monad.Conc.CVar
import Tests.Utils

-- | List of all tests
testCases :: [Test]
testCases =
  [ Test "Simple 2-Deadlock" $ testNot "No deadlocks found!" $ testDeadlockFree 100 simple2Deadlock
  , Test "2 Philosophers"    $ testNot "No deadlocks found!" $ testDeadlockFree 100 $ philosophers 2
  , Test "3 Philosophers"    $ testNot "No deadlocks found!" $ testDeadlockFree 100 $ philosophers 3
  --Random scheduling isn't good enough for these, without increasing
  --the runs.
  --, Test "4 Philosophers"    $ testNot "No deadlocks found!" $ testDeadlockFree 100 $ philosophers 4
  --, Test "5 Philosophers"    $ testNot "No deadlocks found!" $ testDeadlockFree 100 $ philosophers 5
  --, Test "100 Philosophers"  $ testNot "No deadlocks found!" $ testDeadlockFree 100 $ philosophers 100
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

-- | Dining philosophers problem, result is irrelevent, we just want
-- deadlocks.
philosophers :: ConcCVar cvar m => Int -> m ()
philosophers n = do
  forks <- replicateM n newEmptyCVar
  let phils = map (\(i,p) -> p i forks) $ zip [0..] $ replicate n philosopher
  cvars <- mapM spawn phils
  mapM_ takeCVar cvars

  where
    philosopher ident forks = do
      let leftId  = ident
      let rightId = (ident + 1) `mod` length forks
      lock $ forks !! leftId
      lock $ forks !! rightId
      -- In the traditional approach, we'd wait for a random time
      -- here, but we want the only source of (important)
      -- nondeterminism to come from the scheduler, which it does, as
      -- pre-emption is effectively a delay.
      unlock $ forks !! leftId
      unlock $ forks !! rightId

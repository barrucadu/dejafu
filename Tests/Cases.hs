-- | Tests sourced from <https://github.com/sctbenchmarks>.
module Tests.Cases where

import Control.Monad (replicateM)
import Control.Monad.Conc.Class
import Control.Monad.Conc.CVar
import Tests.Utils

-- | List of all tests
testCases :: [Test]
testCases =
  [ Test "Simple 2-Deadlock" $ testNot "No deadlocks found!" $ testDeadlockFree 1   simple2Deadlock
  , Test "2 Philosophers"    $ testNot "No deadlocks found!" $ testDeadlockFree 1 $ philosophers 2
  , Test "3 Philosophers"    $ testNot "No deadlocks found!" $ testDeadlockFree 1 $ philosophers 3
  , Test "4 Philosophers"    $ testNot "No deadlocks found!" $ testDeadlockFree 1 $ philosophers 4
  , Test "5 Philosophers"    $ testNot "No deadlocks found!" $ testDeadlockFree 1 $ philosophers 5
  --, Test "100 Philosophers"  $ testNot "No deadlocks found!" $ testDeadlockFree 0 $ philosophers 100
  , Test "Threshold Value"   $ testNot "All values equal!"   $ testAlwaysSame   1   thresholdValue
  , Test "Forgotten Unlock"  $                                 testDeadlocks    1   forgottenUnlock
  , Test "Simple 2-Race"     $ testNot "All values equal!"   $ testAlwaysSame   1   simple2Race
  , Test "Racey Stack"       $ testNot "All values equal!"   $ testAlwaysSame   1   raceyStack
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

-- | Checks if a value has been increased above a threshold, data
-- racey.
thresholdValue :: ConcCVar cvar m => m Bool
thresholdValue = do
  l <- newEmptyCVar
  x <- newCVar 0

  fork $ lock l >> modifyCVar_ x (return . (+1)) >> unlock l
  fork $ lock l >> modifyCVar_ x (return . (+2)) >> unlock l
  res <- spawn $ lock l >> readCVar x >>= \x' -> unlock l >> return (x' > 3)

  takeCVar res

-- | A lock taken but never released.
forgottenUnlock :: ConcCVar cvar m => m ()
forgottenUnlock = do
  l <- newEmptyCVar
  m <- newEmptyCVar

  let lockl = lock l >> unlock l >> lock l >> lock m >> unlock m >> lock m >> unlock m

  j1 <- spawn lockl
  j2 <- spawn lockl

  takeCVar j1
  takeCVar j2

-- | Very simple data race between two threads.
simple2Race :: ConcCVar cvar m => m Int
simple2Race = do
  x <- newEmptyCVar

  fork $ putCVar x 0
  fork $ putCVar x 1

  readCVar x

-- | Race on popping from a stack.
raceyStack :: ConcCVar cvar m => m (Maybe Int)
raceyStack = do
  s <- newCVar []

  fork $ t1 s [1..10]
  j <- spawn $ t2 s 10 0

  takeCVar j

  where
    push s a = modifyCVar_ s $ return . (a:)
    pop s = do
      val <- takeCVar s
      case val of
        [] -> putCVar s [] >> return Nothing
        (x:xs) -> putCVar s xs >> return (Just x)

    t1 s (x:xs) = push s x >> t1 s xs
    t1 _ []     = return ()

    t2 _ 0 total = return $ Just total
    t2 s n total = do
      val <- pop s
      case val of
        Just x  -> t2 s (n-1) (total+x)
        Nothing -> return Nothing
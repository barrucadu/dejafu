{-# LANGUAGE ImpredicativeTypes #-}

-- | Tests sourced from <https://github.com/sctbenchmarks>.
module Tests.Cases where

import Control.Concurrent.CVar
import Control.Exception (ArithException(..), ArrayException)
import Control.Monad (liftM, replicateM, void)
import Control.Monad.Conc.Class
import Control.Monad.STM.Class

-- | Should deadlock on a minority of schedules.
simple2Deadlock :: MonadConc m => m Int
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
philosophers :: MonadConc m => Int -> m ()
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
thresholdValue :: MonadConc m => m Bool
thresholdValue = do
  l <- newEmptyCVar
  x <- newCVar (0::Int)

  void . fork $ lock l >> modifyCVar_ x (return . (+1)) >> unlock l
  void . fork $ lock l >> modifyCVar_ x (return . (+2)) >> unlock l
  res <- spawn $ lock l >> readCVar x >>= \x' -> unlock l >> return (x' >= 3)

  takeCVar res

-- | A lock taken but never released.
forgottenUnlock :: MonadConc m => m ()
forgottenUnlock = do
  l <- newEmptyCVar
  m <- newEmptyCVar

  let lockl = lock l >> unlock l >> lock l >> lock m >> unlock m >> lock m >> unlock m

  j1 <- spawn lockl
  j2 <- spawn lockl

  takeCVar j1
  takeCVar j2

-- | Very simple data race between two threads.
simple2Race :: MonadConc m => m Int
simple2Race = do
  x <- newEmptyCVar

  void . fork $ putCVar x 0
  void . fork $ putCVar x 1

  readCVar x

-- | Race on popping from a stack.
raceyStack :: MonadConc m => m (Maybe Int)
raceyStack = do
  s <- newCVar []

  void . fork $ t1 s [1..10]
  j <- spawn $ t2 s (10::Int) 0

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

-- | Cause a deadlock sometimes by killing a thread.
threadKill :: MonadConc m => m ()
threadKill = do
  x <- newEmptyCVar
  tid <- fork $ putCVar x ()
  killThread tid
  readCVar x

-- | Never deadlock by masking a thread.
threadKillMask :: MonadConc m => m ()
threadKillMask = do
  x <- newEmptyCVar
  y <- newEmptyCVar
  tid <- fork . mask . const $ putCVar x () >> putCVar y ()
  readCVar x
  killThread tid
  readCVar y

-- | Test nested exception handlers.
excNest :: MonadConc m => m Int
excNest =
  Control.Monad.Conc.Class.catch
    (Control.Monad.Conc.Class.catch
      (throw Overflow)
      (\e -> return . const 1 $ (e :: ArrayException)))
    (\e -> return . const 2 $ (e :: ArithException))

-- | Test unmasking exceptions
threadKillUmask :: MonadConc m => m ()
threadKillUmask = do
  x <- newEmptyCVar
  y <- newEmptyCVar
  tid <- fork . mask $ \umask -> putCVar x () >> umask (putCVar y ())
  readCVar x
  killThread tid
  readCVar y

-- | Test atomicity of STM.
stmAtomic :: MonadConc m => m Int
stmAtomic = do
  x <- atomically $ newCTVar (0::Int)
  void . fork . atomically $ writeCTVar x 1 >> writeCTVar x 2
  atomically $ readCTVar x

-- | Test STM retry
stmRetry :: MonadConc m => m Bool
stmRetry = do
  x <- atomically $ newCTVar (0::Int)
  void . fork . atomically $ writeCTVar x 1 >> retry
  (==0) `liftM` atomically (readCTVar x)

-- | Test STM orElse
stmOrElse :: MonadConc m => m Bool
stmOrElse = do
  x <- atomically $ newCTVar (0::Int)
  atomically $ (writeCTVar x 1 >> retry) `orElse` writeCTVar x 2
  (==2) `liftM` atomically (readCTVar x)

-- | Test STM exceptions
stmExc :: MonadConc m => m Bool
stmExc = do
  x <- atomically $ newCTVar (0::Int)
  void . atomically $ writeCTVar x 1 >> throwSTM Overflow
  (==0) `liftM` atomically (readCTVar x)

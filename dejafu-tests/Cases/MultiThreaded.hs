{-# LANGUAGE CPP #-}

module Cases.MultiThreaded where

import Control.Monad (void)
import Test.DejaFu (Failure(..), gives, gives')
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (test)
import Test.HUnit.DejaFu (testDejafu)

import Control.Concurrent.Classy
import Control.Monad.STM.Class

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

tests :: [Test]
tests =
  [ testGroup "Threading" . hUnitTestToTests $ test
    [ testDejafu threadId1    "child thread ID"  $ gives' [True]
    , testDejafu threadId2    "parent thread ID" $ gives' [True]
    , testDejafu threadNoWait "no wait" $ gives' [Nothing, Just ()]
    ]

  , testGroup "MVar" . hUnitTestToTests $ test
    [ testDejafu cvarLock "deadlock" $ gives [Left Deadlock, Right 0]
    , testDejafu cvarRace "race"     $ gives' [0,1]
    ]

  , testGroup "CRef" . hUnitTestToTests $ test
    [ testDejafu crefRace "race" $ gives' [0,1]
    ]

  , testGroup "STM" . hUnitTestToTests $ test
    [ testDejafu stmAtomic "atomicity" $ gives' [0,2]
    ]

  , testGroup "Killing Threads" . hUnitTestToTests $ test
    [ testDejafu threadKill      "no masking" $ gives  [Left Deadlock, Right ()]
    , testDejafu threadKillMask  "masked"     $ gives' [()]
    , testDejafu threadKillUmask "unmasked"   $ gives  [Left Deadlock, Right ()]
    ]
  ]

--------------------------------------------------------------------------------
-- Threading

-- | Fork reports the good @ThreadId@.
threadId1 :: MonadConc m => m Bool
threadId1 = do
  var <- newEmptyMVar

  tid <- fork $ myThreadId >>= putMVar var

  (tid ==) <$> readMVar var

-- | A child and parent thread have different @ThreadId@s.
threadId2 :: MonadConc m => m Bool
threadId2 = do
  tid <- spawn myThreadId

  (/=) <$> myThreadId <*> readMVar tid

-- | A parent thread doesn't wait for child threads before
-- terminating.
threadNoWait :: MonadConc m => m (Maybe ())
threadNoWait = do
  x <- newCRef Nothing

  void . fork . writeCRef x $ Just ()

  readCRef x

--------------------------------------------------------------------------------
-- @MVar@s

-- | Deadlocks sometimes due to order of acquision of locks.
cvarLock :: MonadConc m => m Int
cvarLock = do
  a <- newEmptyMVar
  b <- newEmptyMVar

  c <- newMVar 0

  let lock m = putMVar m ()
  let unlock = takeMVar

  j1 <- spawn $ lock a >> lock b >> modifyMVar_ c (return . succ) >> unlock b >> unlock a
  j2 <- spawn $ lock b >> lock a >> modifyMVar_ c (return . pred) >> unlock a >> unlock b

  takeMVar j1
  takeMVar j2

  takeMVar c

-- | When racing two @putMVar@s, one of them will win.
cvarRace :: MonadConc m => m Int
cvarRace = do
  x <- newEmptyMVar

  void . fork $ putMVar x 0
  void . fork $ putMVar x 1

  readMVar x

--------------------------------------------------------------------------------
-- @CRef@s
--
-- TODO: Tests on CAS operations

-- | When racing two @writeCRef@s, one of them will win.
crefRace :: MonadConc m => m Int
crefRace = do
  x <- newCRef (0::Int)

  j1 <- spawn $ writeCRef x 0
  j2 <- spawn $ writeCRef x 1

  takeMVar j1
  takeMVar j2

  readCRef x

--------------------------------------------------------------------------------
-- STM

-- | Transactions are atomic.
stmAtomic :: MonadConc m => m Int
stmAtomic = do
  x <- atomically $ newTVar (0::Int)
  void . fork . atomically $ writeTVar x 1 >> writeTVar x 2
  atomically $ readTVar x

--------------------------------------------------------------------------------
-- Exceptions

-- | Cause a deadlock sometimes by killing a thread.
threadKill :: MonadConc m => m ()
threadKill = do
  x <- newEmptyMVar
  tid <- fork $ putMVar x ()
  killThread tid
  readMVar x

-- | Never deadlock by masking a thread.
threadKillMask :: MonadConc m => m ()
threadKillMask = do
  x <- newEmptyMVar
  y <- newEmptyMVar
  tid <- fork $ mask $ \_ -> putMVar x () >> putMVar y ()
  readMVar x
  killThread tid
  readMVar y

-- | Sometimes deadlock by killing a thread.
threadKillUmask :: MonadConc m => m ()
threadKillUmask = do
  x <- newEmptyMVar
  y <- newEmptyMVar
  tid <- fork $ mask $ \umask -> putMVar x () >> umask (putMVar y ())
  readMVar x
  killThread tid
  readMVar y

module Cases.MultiThreaded where

import Control.Exception (ArithException(..))
import Control.Monad (void, unless)
import Test.DejaFu (Failure(..), gives, gives')
import Test.Framework (Test)

import Control.Concurrent.Classy hiding (newQSemN, signalQSemN, waitQSemN)
import Test.DejaFu.Conc (ConcT, subconcurrency)

import Common
import QSemN

tests :: [Test]
tests =
    [ testGroup "Threading"
      [ T "child thread ID"  threadId1    $ gives' [True]
      , T "parent thread ID" threadId2    $ gives' [True]
      , T "no wait"          threadNoWait $ gives' [Nothing, Just ()]
      ]
    , testGroup "MVar"
      [ T "deadlock" cvarLock $ gives  [Left Deadlock, Right 0]
      , T "race"     cvarRace $ gives' [0,1]
      ]
    , testGroup "CRef"
      [ T "race"            crefRace        $ gives' [0,1]
      , T "cas modify"      crefCASModify   $ gives' [0,1]
      , T "cas race"        crefCASRace     $ gives' [(True, 2), (False, 2)]
      , T "cas race (redo)" crefCASRaceRedo $ gives' [(True, 1), (True, 2)]
      , T "cas tickets"     crefCASTickets  $ gives' [(True, False, 1), (False, True, 2)]
      ]
    , testGroup "STM"
      [ T "atomicity"   stmAtomic     $ gives' [0,2]
      , T "left retry"  stmLeftRetry  $ gives' [()]
      , T "right retry" stmRightRetry $ gives' [()]
      , T "issue 55"    stmIssue55    $ gives' [True]
      , T "issue 111"   stmIssue111   $ gives' [1]
      ]
    , testGroup "Killing Threads"
      [ T "no masking" threadKill      $ gives  [Left Deadlock, Right ()]
      , T "masked"     threadKillMask  $ gives' [()]
      , T "masked (uninterruptible)" threadKillUninterruptibleMask $ gives [Left Deadlock]
      , T "unmasked"   threadKillUmask $ gives  [Left Deadlock, Right ()]
      , T "throw to main (uncaught)" threadKillToMain1 $ gives  [Left UncaughtException]
      , T "throw to main (caught)"   threadKillToMain2 $ gives' [()]
      ]
    , testGroup "Daemons"
      [ T "schedule daemon" schedDaemon $ gives' [0,1]
      ]
    , testGroup "Subconcurrency"
      [ T "deadlock1" scDeadlock1 $ gives' [Left Deadlock, Right ()]
      , T "deadlock2" scDeadlock2 $ gives' [(Left Deadlock, ()), (Right (), ())]
      , T "success"   scSuccess   $ gives' [Right ()]
      , T "illegal"   scIllegal   $ gives  [Left IllegalSubconcurrency]
      , T "issue 71"  scIssue71   $ gives' [()]
      , T "issue 81"  scIssue81   $ gives' [(Right (),0)]
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

-- | When racing two @writeCRef@s, one of them will win.
crefRace :: MonadConc m => m Int
crefRace = do
  x <- newCRef (0::Int)

  j1 <- spawn $ writeCRef x 0
  j2 <- spawn $ writeCRef x 1

  takeMVar j1
  takeMVar j2

  readCRef x

-- | Modify CAS works.
crefCASModify :: MonadConc m => m Int
crefCASModify = do
  x <- newCRef (0::Int)
  fork $ modifyCRefCAS x (\_ -> (1, ()))
  readCRef x

-- | CAS with two threads is racey.
crefCASRace :: MonadConc m => m (Bool, Int)
crefCASRace = do
  x <- newCRef (0::Int)
  t <- readForCAS x
  j <- spawn $ casCRef x t 1
  writeCRef x 2
  b <- fst <$> readMVar j
  v <- readCRef x
  pure (b, v)

-- | Failed CAS can use the new ticket to succeed.
crefCASRaceRedo :: MonadConc m => m (Bool, Int)
crefCASRaceRedo = do
  x <- newCRef (0::Int)
  t <- readForCAS x
  v <- newEmptyMVar
  j <- spawn $ do
    o@(f, t') <- casCRef x t 1
    takeMVar v
    if f then pure o else casCRef x t' 1
  writeCRef x 2
  putMVar v ()
  b <- fst <$> readMVar j
  v <- readCRef x
  pure (b, v)

-- | A ticket is only good for one CAS.
crefCASTickets :: MonadConc m => m (Bool, Bool, Int)
crefCASTickets = do
  x  <- newCRef (0::Int)
  t  <- readForCAS x
  j1 <- spawn $ casCRef x t 1
  j2 <- spawn $ casCRef x t 2
  b1 <- fst <$> readMVar j1
  b2 <- fst <$> readMVar j2
  v  <- readCRef x
  pure (b1, b2, v)

--------------------------------------------------------------------------------
-- STM

-- | Transactions are atomic.
stmAtomic :: MonadConc m => m Int
stmAtomic = do
  x <- atomically $ newTVar (0::Int)
  void . fork . atomically $ writeTVar x 1 >> writeTVar x 2
  atomically $ readTVar x

-- | 'retry' is the left identity of 'orElse'.
stmLeftRetry :: MonadConc m => m ()
stmLeftRetry = do
  x <- atomically $ newTVar Nothing
  let readJust var = maybe retry pure =<< readTVar var
  fork . atomically . writeTVar x $ Just ()
  atomically $ retry `orElse` readJust x

-- | 'retry' is the right identity of 'orElse'.
stmRightRetry :: MonadConc m => m ()
stmRightRetry = do
  x <- atomically $ newTVar Nothing
  let readJust var = maybe retry pure =<< readTVar var
  fork . atomically . writeTVar x $ Just ()
  atomically $ readJust x `orElse` retry

-- | Test case from issue #55.
stmIssue55 :: MonadConc m => m Bool
stmIssue55 = do
  a <- atomically $ newTQueue
  b <- atomically $ newTQueue
  fork . atomically $ writeTQueue b True
  let both a b = readTQueue a `orElse` readTQueue b `orElse` retry
  atomically $ both a b

-- | Test case from issue #111
stmIssue111 :: MonadConc m => m Int
stmIssue111 = do
  v <- atomically $ newTVar 1
  fork . atomically $ do
    writeTVar v 2
    writeTVar v 3
    retry
  atomically $ readTVar v

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

-- | Deadlock trying to throw an exception to an
-- uninterruptibly-masked thread.
threadKillUninterruptibleMask :: MonadConc m => m ()
threadKillUninterruptibleMask = do
  x <- newEmptyMVar
  y <- newEmptyMVar
  tid <- fork $ uninterruptibleMask $ \_ -> putMVar x () >> takeMVar y
  readMVar x
  killThread tid

-- | Sometimes deadlock by killing a thread.
threadKillUmask :: MonadConc m => m ()
threadKillUmask = do
  x <- newEmptyMVar
  y <- newEmptyMVar
  tid <- fork $ mask $ \umask -> putMVar x () >> umask (putMVar y ())
  readMVar x
  killThread tid
  readMVar y

-- | Throw an exception to the main thread with 'throwTo', without a
-- handler.
threadKillToMain1 :: MonadConc m => m ()
threadKillToMain1 = do
  tid <- myThreadId
  j <- spawn $ throwTo tid Overflow
  readMVar j

-- | Throw an exception to the main thread with 'throwTo', with a
-- handler.
threadKillToMain2 :: MonadConc m => m ()
threadKillToMain2 = do
  tid <- myThreadId
  catchArithException (spawn (throwTo tid Overflow) >>= readMVar)
                      (\_ -> pure ())

-------------------------------------------------------------------------------
-- Daemon threads

-- | Fork off a thread where the first action has no dependency with
-- anything the initial thread does, but which has a later action
-- which does. This exhibits issue #40.
schedDaemon :: MonadConc m => m Int
schedDaemon = do
  x <- newCRef 0
  _ <- fork $ myThreadId >> writeCRef x 1
  readCRef x

--------------------------------------------------------------------------------
-- Subconcurrency

-- | Subcomputation deadlocks sometimes.
scDeadlock1 :: Monad n => ConcT r n (Either Failure ())
scDeadlock1 = do
  var <- newEmptyMVar
  subconcurrency $ do
    void . fork $ putMVar var ()
    putMVar var ()

-- | Subcomputation deadlocks sometimes, and action after it still
-- happens.
scDeadlock2 :: Monad n => ConcT r n (Either Failure (), ())
scDeadlock2 = do
  var <- newEmptyMVar
  res <- subconcurrency $ do
    void . fork $ putMVar var ()
    putMVar var ()
  (,) <$> pure res <*> readMVar var

-- | Subcomputation successfully completes.
scSuccess :: Monad n => ConcT r n (Either Failure ())
scSuccess = do
  var <- newMVar ()
  subconcurrency $ do
    out <- newEmptyMVar
    void . fork $ takeMVar var >>= putMVar out
    takeMVar out

-- | Illegal usage
scIllegal :: Monad n => ConcT r n ()
scIllegal = do
  var <- newEmptyMVar
  void . fork $ readMVar var
  void . subconcurrency $ pure ()

-- | Test case from issue 71. This won't fail if the bug is
-- reintroduced, it will just hang.
scIssue71 :: Monad n => ConcT r n ()
scIssue71 = do
  let ma ||| mb = do { j1 <- spawn ma; j2 <- spawn mb; takeMVar j1; takeMVar j2; pure () }
  s <- newEmptyMVar
  _ <- subconcurrency (takeMVar s ||| pure ())
  pure ()

-- | Test case from issue 81.
scIssue81 :: Monad n => ConcT r n (Either Failure (), Int)
scIssue81 = do
  s <- newQSemN 0
  let interfere = waitQSemN s 0 >> signalQSemN s 0
  x <- subconcurrency (signalQSemN s 0 ||| waitQSemN s 0 ||| interfere)
  o <- remainingQSemN s
  pure (x, o)

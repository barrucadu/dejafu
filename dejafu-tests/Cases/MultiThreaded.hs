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
      [ threadId1
      , threadId2
      , threadNoWait
      ]
    , testGroup "MVar"
      [ mvarLock
      , mvarRace
      ]
    , testGroup "CRef"
      [ crefRace
      , crefCASModify
      , crefCASRace
      , crefCASRaceRedo
      , crefCASTickets
      ]
    , testGroup "STM"
      [ stmAtomic
      , stmLeftRetry
      , stmRightRetry
      , stmIssue55
      , stmIssue111
      ]
    , testGroup "Killing Threads"
      [ threadKill
      , threadKillMask
      , threadKillUninterruptibleMask
      , threadKillUmask
      , threadKillToMain1
      , threadKillToMain2
      ]
    , testGroup "Daemons"
      [ schedDaemon
      ]
    , testGroup "Subconcurrency"
      [ scDeadlock1
      , scDeadlock2
      , scSuccess
      , scIllegal
      , scIssue71
      , scIssue81
      ]
    ]

--------------------------------------------------------------------------------
-- Threading

threadId1 :: [Test]
threadId1 = djfuT "Fork reports the thread ID of the child" (gives' [True]) $ do
  var <- newEmptyMVar
  tid <- fork $ myThreadId >>= putMVar var
  (tid ==) <$> readMVar var

threadId2 :: [Test]
threadId2 = djfuT "Different threads have different thread IDs" (gives' [True]) $ do
  tid <- spawn myThreadId
  (/=) <$> myThreadId <*> readMVar tid

threadNoWait :: [Test]
threadNoWait = djfuT "A thread doesn't wait for its children before terminating" (gives' [Nothing, Just ()]) $ do
  x <- newCRef Nothing
  void . fork . writeCRef x $ Just ()
  readCRef x

--------------------------------------------------------------------------------
-- @MVar@s

mvarLock :: [Test]
mvarLock = djfuT "Racey MVar computations may deadlock" (gives [Left Deadlock, Right 0]) $ do
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

mvarRace :: [Test]
mvarRace = djfuT "Racey MVar computations are nondeterministic" (gives' [0,1]) $ do
  x <- newEmptyMVar
  _ <- fork $ putMVar x 0
  _ <-  fork $ putMVar x 1
  readMVar x

--------------------------------------------------------------------------------
-- @CRef@s

crefRace :: [Test]
crefRace = djfuT "Racey CRef computations are nondeterministic" (gives' [0,1]) $ do
  x <- newCRef (0::Int)

  j1 <- spawn $ writeCRef x 0
  j2 <- spawn $ writeCRef x 1

  takeMVar j1
  takeMVar j2

  readCRef x

crefCASModify :: [Test]
crefCASModify = djfuT "CASing CRef changes its value" (gives' [0,1]) $ do
  x <- newCRef (0::Int)
  fork $ modifyCRefCAS x (\_ -> (1, ()))
  readCRef x

crefCASRace :: [Test]
crefCASRace = djfuT "Racey CAS computations are nondeterministic" (gives' [(True, 2), (False, 2)]) $ do
  x <- newCRef (0::Int)
  t <- readForCAS x
  j <- spawn $ casCRef x t 1
  writeCRef x 2
  b <- fst <$> readMVar j
  v <- readCRef x
  pure (b, v)

crefCASRaceRedo :: [Test]
crefCASRaceRedo = djfuT "A failed CAS gives an updated ticket" (gives' [(True, 1), (True, 2)]) $ do
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

crefCASTickets :: [Test]
crefCASTickets = djfuT "A ticket is only good for one CAS" (gives' [(True, False, 1), (False, True, 2)]) $ do
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

stmAtomic :: [Test]
stmAtomic = djfuT "Transactions are atomic" (gives' [0,2]) $ do
  x <- atomically $ newTVar (0::Int)
  void . fork . atomically $ writeTVar x 1 >> writeTVar x 2
  atomically $ readTVar x

stmLeftRetry :: [Test]
stmLeftRetry = djfuT "'retry' is the left identity of 'orElse'" (gives' [()]) $ do
  x <- atomically $ newTVar Nothing
  let readJust var = maybe retry pure =<< readTVar var
  fork . atomically . writeTVar x $ Just ()
  atomically $ retry `orElse` readJust x

stmRightRetry :: [Test]
stmRightRetry = djfuT "'retry' is the right identity of 'orElse'" (gives' [()]) $ do
  x <- atomically $ newTVar Nothing
  let readJust var = maybe retry pure =<< readTVar var
  fork . atomically . writeTVar x $ Just ()
  atomically $ readJust x `orElse` retry

stmIssue55 :: [Test]
stmIssue55 = djfuT "https://github.com/barrucadu/dejafu/issues/55" (gives' [True]) $ do
  a <- atomically $ newTQueue
  b <- atomically $ newTQueue
  fork . atomically $ writeTQueue b True
  let both a b = readTQueue a `orElse` readTQueue b `orElse` retry
  atomically $ both a b

stmIssue111 :: [Test]
stmIssue111 = djfuT "https://github.com/barrucadu/dejafu/issues/111" (gives' [1]) $ do
  v <- atomically $ newTVar 1
  fork . atomically $ do
    writeTVar v 2
    writeTVar v 3
    retry
  atomically $ readTVar v

--------------------------------------------------------------------------------
-- Exceptions

threadKill :: [Test]
threadKill = djfuT "Exceptions can kill unmasked threads" (gives [Left Deadlock, Right ()]) $ do
  x <- newEmptyMVar
  tid <- fork $ putMVar x ()
  killThread tid
  readMVar x

threadKillMask :: [Test]
threadKillMask = djfuT "Exceptions cannot kill nonblocking masked threads" (gives' [()]) $ do
  x <- newEmptyMVar
  y <- newEmptyMVar
  tid <- fork $ mask $ \_ -> putMVar x () >> putMVar y ()
  readMVar x
  killThread tid
  readMVar y

threadKillUninterruptibleMask :: [Test]
threadKillUninterruptibleMask = djfuT "Throwing to an uninterruptible thread blocks" (gives [Left Deadlock]) $ do
  x <- newEmptyMVar
  y <- newEmptyMVar
  tid <- fork $ uninterruptibleMask $ \_ -> putMVar x () >> takeMVar y
  readMVar x
  killThread tid

threadKillUmask :: [Test]
threadKillUmask = djfuT "Exceptions can kill nonblocking masked threads which have unmasked" (gives [Left Deadlock, Right ()]) $ do
  x <- newEmptyMVar
  y <- newEmptyMVar
  tid <- fork $ mask $ \umask -> putMVar x () >> umask (putMVar y ())
  readMVar x
  killThread tid
  readMVar y

threadKillToMain1 :: [Test]
threadKillToMain1 = djfuT "Throwing to main kills the computation, if unhandled" (gives [Left UncaughtException]) $ do
  tid <- myThreadId
  j <- spawn $ throwTo tid Overflow
  readMVar j

threadKillToMain2 :: [Test]
threadKillToMain2 = djfuT "Throwing to main doesn't kill the computation, if handled" (gives' [()]) $ do
  tid <- myThreadId
  catchArithException (spawn (throwTo tid Overflow) >>= readMVar)
                      (\_ -> pure ())

-------------------------------------------------------------------------------
-- Daemon threads

schedDaemon :: [Test]
schedDaemon = djfuT "https://github.com/barrucadu/dejafu/issues/40" (gives' [0,1]) $ do
  x <- newCRef 0
  _ <- fork $ myThreadId >> writeCRef x 1
  readCRef x

--------------------------------------------------------------------------------
-- Subconcurrency

scDeadlock1 :: [Test]
scDeadlock1 = djfuT "Failure is observable" (gives' [Left Deadlock, Right ()]) $ do
  var <- newEmptyMVar
  subconcurrency $ do
    void . fork $ putMVar var ()
    putMVar var ()

scDeadlock2 :: [Test]
scDeadlock2 = djfuT "Failure does not abort the outer computation" (gives' [(Left Deadlock, ()), (Right (), ())]) $ do
  var <- newEmptyMVar
  res <- subconcurrency $ do
    void . fork $ putMVar var ()
    putMVar var ()
  (,) <$> pure res <*> readMVar var

scSuccess :: [Test]
scSuccess = djfuT "Success is observable" (gives' [Right ()]) $ do
  var <- newMVar ()
  subconcurrency $ do
    out <- newEmptyMVar
    void . fork $ takeMVar var >>= putMVar out
    takeMVar out

scIllegal :: [Test]
scIllegal = djfuT "It is illegal to start subconcurrency after forking" (gives [Left IllegalSubconcurrency]) $ do
  var <- newEmptyMVar
  void . fork $ readMVar var
  void . subconcurrency $ pure ()

scIssue71 :: [Test]
scIssue71 = djfuT "https://github.com/barrucadu/dejafu/issues/71" (gives' [()]) $ do
  let ma ||| mb = do { j1 <- spawn ma; j2 <- spawn mb; takeMVar j1; takeMVar j2; pure () }
  s <- newEmptyMVar
  _ <- subconcurrency (takeMVar s ||| pure ())
  pure ()

scIssue81 :: [Test]
scIssue81 = djfuT "https://github.com/barrucadu/dejafu/issues/81" (gives' [(Right (),0)]) $ do
  s <- newQSemN 0
  let interfere = waitQSemN s 0 >> signalQSemN s 0
  x <- subconcurrency (signalQSemN s 0 ||| waitQSemN s 0 ||| interfere)
  o <- remainingQSemN s
  pure (x, o)

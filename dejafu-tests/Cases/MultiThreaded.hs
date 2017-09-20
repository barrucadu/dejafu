module Cases.MultiThreaded where

import Control.Exception (ArithException(..))
import Test.DejaFu (Failure(..), gives, gives')
import Test.Framework (Test)

import Control.Concurrent.Classy hiding (newQSemN, signalQSemN, waitQSemN)
import Test.DejaFu.Conc (subconcurrency)

import Common
import QSemN

tests :: [Test]
tests =
    [ testGroup "Threading" threadingTests
    , testGroup "MVar" mvarTests
    , testGroup "CRef" crefTests
    , testGroup "STM" stmTests
    , testGroup "Exceptions" exceptionTests
    , testGroup "Daemons" daemonTests
    , testGroup "Subconcurrency" subconcurrencyTests
    ]

--------------------------------------------------------------------------------

threadingTests :: [Test]
threadingTests = toTestList
  [ djfuT "Fork reports the thread ID of the child" (gives' [True]) $ do
      var <- newEmptyMVar
      tid <- fork $ myThreadId >>= putMVar var
      (tid ==) <$> readMVar var

  , djfuT "Different threads have different thread IDs" (gives' [True]) $ do
      tid <- spawn myThreadId
      (/=) <$> myThreadId <*> readMVar tid

  , djfuT "A thread doesn't wait for its children before terminating" (gives' [Nothing, Just ()]) $ do
      x <- newCRef Nothing
      _ <- fork . writeCRef x $ Just ()
      readCRef x
  ]

--------------------------------------------------------------------------------

mvarTests :: [Test]
mvarTests = toTestList
  [ djfuT "Racey MVar computations may deadlock" (gives [Left Deadlock, Right 0]) $ do
      a <- newEmptyMVar
      b <- newEmptyMVar
      c <- newMVarInt 0
      let lock m = putMVar m ()
      let unlock = takeMVar
      j1 <- spawn $ lock a >> lock b >> modifyMVar_ c (return . succ) >> unlock b >> unlock a
      j2 <- spawn $ lock b >> lock a >> modifyMVar_ c (return . pred) >> unlock a >> unlock b
      takeMVar j1
      takeMVar j2
      takeMVar c

  , djfuT "Racey MVar computations are nondeterministic" (gives' [0,1]) $ do
      x <- newEmptyMVarInt
      _ <- fork $ putMVar x 0
      _ <- fork $ putMVar x 1
      readMVar x
  ]

--------------------------------------------------------------------------------

crefTests :: [Test]
crefTests = toTestList
  [ djfuT "Racey CRef computations are nondeterministic" (gives' [0,1]) $ do
      x  <- newCRefInt 0
      j1 <- spawn $ writeCRef x 0
      j2 <- spawn $ writeCRef x 1
      takeMVar j1
      takeMVar j2
      readCRef x

  , djfuT "CASing CRef changes its value" (gives' [0,1]) $ do
      x <- newCRefInt 0
      _ <- fork $ modifyCRefCAS x (\_ -> (1, ()))
      readCRef x

  , djfuT "Racey CAS computations are nondeterministic" (gives' [(True, 2), (False, 2)]) $ do
      x <- newCRefInt 0
      t <- readForCAS x
      j <- spawn $ casCRef x t 1
      writeCRef x 2
      b <- fst <$> readMVar j
      v <- readCRef x
      pure (b, v)

  , djfuT "A failed CAS gives an updated ticket" (gives' [(True, 1), (True, 2)]) $ do
      x <- newCRefInt 0
      t <- readForCAS x
      v <- newEmptyMVar
      j <- spawn $ do
        o@(f, t') <- casCRef x t 1
        takeMVar v
        if f then pure o else casCRef x t' 1
      writeCRef x 2
      putMVar v ()
      b <- fst <$> readMVar j
      o <- readCRef x
      pure (b, o)

  , djfuT "A ticket is only good for one CAS" (gives' [(True, False, 1), (False, True, 2)]) $ do
      x  <- newCRefInt 0
      t  <- readForCAS x
      j1 <- spawn $ casCRef x t 1
      j2 <- spawn $ casCRef x t 2
      b1 <- fst <$> readMVar j1
      b2 <- fst <$> readMVar j2
      v  <- readCRef x
      pure (b1, b2, v)
  ]

--------------------------------------------------------------------------------

stmTests :: [Test]
stmTests = toTestList
  [ djfuT "Transactions are atomic" (gives' [0,2]) $ do
      x <- atomically $ newTVarInt 0
      _ <- fork . atomically $ writeTVar x 1 >> writeTVar x 2
      atomically $ readTVar x

  , djfuT "'retry' is the left identity of 'orElse'" (gives' [()]) $ do
      x <- atomically $ newTVar Nothing
      let readJust var = maybe retry pure =<< readTVar var
      _ <- fork . atomically . writeTVar x $ Just ()
      atomically $ retry `orElse` readJust x

  , djfuT "'retry' is the right identity of 'orElse'" (gives' [()]) $ do
      x <- atomically $ newTVar Nothing
      let readJust var = maybe retry pure =<< readTVar var
      fork . atomically . writeTVar x $ Just ()
      atomically $ readJust x `orElse` retry

  , djfuT "https://github.com/barrucadu/dejafu/issues/55" (gives' [True]) $ do
      a <- atomically $ newTQueue
      b <- atomically $ newTQueue
      _ <- fork . atomically $ writeTQueue b True
      let both x y = readTQueue x `orElse` readTQueue y `orElse` retry
      atomically $ both a b

  , djfuT "https://github.com/barrucadu/dejafu/issues/111" (gives' [1]) $ do
      v <- atomically $ newTVarInt 1
      _ <- fork . atomically $ do
        writeTVar v 2
        writeTVar v 3
        retry
      atomically $ readTVar v
  ]

--------------------------------------------------------------------------------

exceptionTests :: [Test]
exceptionTests = toTestList
  [ djfuT "Exceptions can kill unmasked threads" (gives [Left Deadlock, Right ()]) $ do
      x <- newEmptyMVar
      tid <- fork $ putMVar x ()
      killThread tid
      readMVar x

  , djfuT "Exceptions cannot kill nonblocking masked threads" (gives' [()]) $ do
      x <- newEmptyMVar
      y <- newEmptyMVar
      tid <- fork $ mask $ \_ -> putMVar x () >> putMVar y ()
      readMVar x
      killThread tid
      readMVar y

  , djfuT "Throwing to an uninterruptible thread blocks" (gives [Left Deadlock]) $ do
      x <- newEmptyMVar
      y <- newEmptyMVar
      tid <- fork $ uninterruptibleMask $ \_ -> putMVar x () >> takeMVar y
      readMVar x
      killThread tid

  , djfuT "Exceptions can kill masked threads which have unmasked" (gives [Left Deadlock, Right ()]) $ do
      x <- newEmptyMVar
      y <- newEmptyMVar
      tid <- fork $ mask $ \umask -> putMVar x () >> umask (putMVar y ())
      readMVar x
      killThread tid
      readMVar y

  , djfuT "Throwing to main kills the computation, if unhandled" (gives [Left UncaughtException]) $ do
      tid <- myThreadId
      j <- spawn $ throwTo tid Overflow
      readMVar j

  , djfuT "Throwing to main doesn't kill the computation, if handled" (gives' [()]) $ do
      tid <- myThreadId
      catchArithException
        (spawn (throwTo tid Overflow) >>= readMVar)
        (\_ -> pure ())
  ]

-------------------------------------------------------------------------------

daemonTests :: [Test]
daemonTests = toTestList
  [ djfuT "https://github.com/barrucadu/dejafu/issues/40" (gives' [0,1]) $ do
      x <- newCRefInt 0
      _ <- fork $ myThreadId >> writeCRef x 1
      readCRef x
  ]

--------------------------------------------------------------------------------

subconcurrencyTests :: [Test]
subconcurrencyTests = toTestList
  [ djfuT "Failure is observable" (gives' [Left Deadlock, Right ()]) $ do
      var <- newEmptyMVar
      subconcurrency $ do
        _ <- fork $ putMVar var ()
        putMVar var ()

  , djfuT "Failure does not abort the outer computation" (gives' [(Left Deadlock, ()), (Right (), ())]) $ do
      var <- newEmptyMVar
      res <- subconcurrency $ do
        _ <- fork $ putMVar var ()
        putMVar var ()
      (,) <$> pure res <*> readMVar var

  , djfuT "Success is observable" (gives' [Right ()]) $ do
      var <- newMVar ()
      subconcurrency $ do
        out <- newEmptyMVar
        _ <- fork $ takeMVar var >>= putMVar out
        takeMVar out

  , djfuT "It is illegal to start subconcurrency after forking" (gives [Left IllegalSubconcurrency]) $ do
      var <- newEmptyMVar
      _ <- fork $ readMVar var
      _ <- subconcurrency $ pure ()
      pure ()

  , djfuT "https://github.com/barrucadu/dejafu/issues/71" (gives' [()]) $ do
      let ma ||| mb = do { j1 <- spawn ma; j2 <- spawn mb; takeMVar j1; takeMVar j2; pure () }
      s <- newEmptyMVar
      _ <- subconcurrency (takeMVar s ||| pure ())
      pure ()

  , djfuT "https://github.com/barrucadu/dejafu/issues/81" (gives' [(Right (),0)]) $ do
      s <- newQSemN 0
      let interfere = waitQSemN s 0 >> signalQSemN s 0
      x <- subconcurrency (signalQSemN s 0 ||| waitQSemN s 0 ||| interfere)
      o <- remainingQSemN s
      pure (x, o)
  ]

module Integration.MultiThreaded where

import qualified Control.Concurrent        as C
import           Control.Exception         (ArithException(..))
import           Control.Monad.IO.Class    (liftIO)
import           Test.DejaFu               (Failure(..), gives, gives',
                                            isUncaughtException)

import           Control.Concurrent.Classy
import qualified Data.IORef                as IORef
import           Test.DejaFu.Conc          (subconcurrency)

import           Common

tests :: [TestTree]
tests =
  [ testGroup "Threading" threadingTests
  , testGroup "MVar" mvarTests
  , testGroup "CRef" crefTests
  , testGroup "STM" stmTests
  , testGroup "Exceptions" exceptionTests
  , testGroup "Capabilities" capabilityTests
  , testGroup "Subconcurrency" subconcurrencyTests
  , testGroup "IO" ioTests
  ]

--------------------------------------------------------------------------------

threadingTests :: [TestTree]
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

  , djfuT "The main thread is bound" (gives' [(True, True)]) $ do
      b1 <- isCurrentThreadBound
      -- check the thread is *really* bound
      b2 <- liftIO C.isCurrentThreadBound
      pure (b1, b2)

  , djfuT "A thread started with forkOS is bound" (gives' [(True, True)]) $ do
      v <- newEmptyMVar
      _ <- forkOS $ do
        b1 <- isCurrentThreadBound
        b2 <- liftIO C.isCurrentThreadBound
        putMVar v (b1, b2)
      readMVar v

  , djfuT "A thread started with fork is not bound" (gives' [False]) $ do
      v <- newEmptyMVar
      _ <- fork $ putMVar v =<< isCurrentThreadBound
      readMVar v

  , djfuT "An action can be run in an unbound thread" (gives' [(True, False)]) $ do
      v <- newEmptyMVar
      _ <- forkOS $ do
        b1 <- isCurrentThreadBound
        b2 <- runInUnboundThread isCurrentThreadBound
        putMVar v (b1, b2)
      readMVar v

  , djfuT "An action can be run in a bound thread" (gives' [(False, True)]) $ do
      v <- newEmptyMVar
      _ <- fork $ do
        b1 <- isCurrentThreadBound
        b2 <- runInBoundThread isCurrentThreadBound
        putMVar v (b1, b2)
      readMVar v
  ]

--------------------------------------------------------------------------------

mvarTests :: [TestTree]
mvarTests = toTestList
  [ djfuT "Racey MVar computations may deadlock" (gives [Left Deadlock, Right 0]) $ do
      a <- newEmptyMVar
      b <- newEmptyMVar
      c <- newMVarInt 0
      let lock m = putMVar m ()
      let unlock = takeMVar
      j1 <- spawn $ lock a >> lock b >> modifyMVar_ c (pure . succ) >> unlock b >> unlock a
      j2 <- spawn $ lock b >> lock a >> modifyMVar_ c (pure . pred) >> unlock a >> unlock b
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

crefTests :: [TestTree]
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
      _ <- fork $ modifyCRefCAS x (const (1, ()))
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

stmTests :: [TestTree]
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
      _ <- fork . atomically . writeTVar x $ Just ()
      atomically $ readJust x `orElse` retry
  ]

--------------------------------------------------------------------------------

exceptionTests :: [TestTree]
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

  , djfuT "Throwing to main kills the computation, if unhandled" (alwaysFailsWith isUncaughtException) $ do
      tid <- myThreadId
      j <- spawn $ throwTo tid Overflow
      readMVar j

  , djfuT "Throwing to main doesn't kill the computation, if handled" (gives' [()]) $ do
      tid <- myThreadId
      catchArithException
        (spawn (throwTo tid Overflow) >>= readMVar)
        (\_ -> pure ())
  ]

--------------------------------------------------------------------------------

capabilityTests :: [TestTree]
capabilityTests = toTestList
  [ djfu "get/setNumCapabilities are dependent" (gives' [1,3]) $ do
      setNumCapabilities 1
      _ <- fork (setNumCapabilities 3)
      getNumCapabilities
  ]

--------------------------------------------------------------------------------

subconcurrencyTests :: [TestTree]
subconcurrencyTests = toTestList
  [ djfuTS "Failure is observable" (gives' [Left Deadlock, Right ()]) $ do
      var <- newEmptyMVar
      subconcurrency $ do
        _ <- fork $ putMVar var ()
        putMVar var ()

  , djfuTS "Failure does not abort the outer computation" (gives' [(Left Deadlock, ()), (Right (), ())]) $ do
      var <- newEmptyMVar
      res <- subconcurrency $ do
        _ <- fork $ putMVar var ()
        putMVar var ()
      (,) <$> pure res <*> readMVar var

  , djfuTS "Success is observable" (gives' [Right ()]) $ do
      var <- newMVar ()
      subconcurrency $ do
        out <- newEmptyMVar
        _ <- fork $ takeMVar var >>= putMVar out
        takeMVar out

  , djfuTS "It is illegal to start subconcurrency after forking" (gives [Left IllegalSubconcurrency]) $ do
      var <- newEmptyMVar
      _ <- fork $ readMVar var
      _ <- subconcurrency $ pure ()
      pure ()
  ]

-------------------------------------------------------------------------------

ioTests :: [TestTree]
ioTests = toTestList
  [ djfu "Lifted IO actions are dependent" (gives' [0,1,2]) $ do
      r <- liftIO (IORef.newIORef (0::Int))
      _ <- fork $ liftIO (IORef.atomicWriteIORef r 1)
      _ <- fork $ liftIO (IORef.atomicWriteIORef r 2)
      liftIO (IORef.readIORef r)
  ]

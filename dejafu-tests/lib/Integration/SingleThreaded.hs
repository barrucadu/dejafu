{-# LANGUAGE CPP #-}

module Integration.SingleThreaded where

import           Control.Exception         (ArithException(..),
                                            ArrayException(..))
import           Test.DejaFu               (Failure(..), gives, gives', isAbort,
                                            isDeadlock, isIllegalDontCheck,
                                            isUncaughtException)
import           Test.DejaFu.Settings      (defaultLengthBound)

import           Control.Concurrent.Classy
import           Control.Monad             (replicateM_)
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.IORef                as IORef
import           System.Random             (mkStdGen)
import           Test.DejaFu.Conc          (dontCheck, subconcurrency)

import           Common

tests :: [TestTree]
tests =
  [ testGroup "MVar" mvarTests
  , testGroup "CRef" crefTests
  , testGroup "STM" stmTests
  , testGroup "Exceptions" exceptionTests
  , testGroup "Capabilities" capabilityTests
  , testGroup "Hacks" hacksTests
  , testGroup "IO" ioTests
  ]

--------------------------------------------------------------------------------

mvarTests :: [TestTree]
mvarTests = toTestList
  [ djfu "Taking from an empty MVar blocks" (gives [Left Deadlock]) $ do
      var <- newEmptyMVarInt
      takeMVar var

  , djfu "Non-blockingly taking from an empty MVar gives nothing" (gives' [Nothing]) $ do
      var <- newEmptyMVarInt
      tryTakeMVar var

  , djfu "Putting into an empty MVar updates it" (gives' [True]) $ do
      var <- newEmptyMVarInt
      putMVar var 7
      (==7) <$> readMVar var

  , djfu "Non-blockingly putting into an empty MVar updates it" (gives' [True]) $ do
      var <- newEmptyMVarInt
      _   <- tryPutMVar var 7
      (==7) <$> readMVar var

  , djfu "Reading an empty MVar blocks" (gives [Left Deadlock]) $ do
      var <- newEmptyMVarInt
      readMVar var

  , djfu "Non-blockingly reading an empty MVar gives nothing" (gives' [Nothing]) $ do
      var <- newEmptyMVarInt
      tryReadMVar var

  , djfu "Putting into a full MVar blocks" (gives [Left Deadlock]) $ do
      var <- newMVarInt 7
      putMVar var 10

  , djfu "Non-blockingly putting into a full MVar fails" (gives' [False]) $ do
      var <- newMVarInt 7
      tryPutMVar var 10

  , djfu "Taking from a full MVar works" (gives' [True]) $ do
      var <- newMVarInt 7
      (==7) <$> takeMVar var

  , djfu "Non-blockingly taking from a full MVar works" (gives' [True]) $ do
      var <- newMVarInt 7
      (==Just 7) <$> tryTakeMVar var

  , djfu "Reading a full MVar works" (gives' [True]) $ do
      var <- newMVarInt 7
      (==7) <$> readMVar var

  , djfu "Non-blockingly reading a full MVar works" (gives' [True]) $ do
      var <- newMVarInt 7
      (==Just 7) <$> tryReadMVar var
  ]

--------------------------------------------------------------------------------

crefTests :: [TestTree]
crefTests = toTestList
  [ djfu "Reading a non-updated CRef gives its initial value" (gives' [True]) $ do
      ref <- newCRefInt 5
      (5==) <$> readCRef ref

  , djfu "Reading an updated CRef gives its new value" (gives' [True]) $ do
      ref <- newCRefInt 5
      writeCRef ref 6
      (6==) <$> readCRef ref

  , djfu "Updating a CRef by a function changes its value" (gives' [True]) $ do
      ref <- newCRefInt 5
      atomicModifyCRef ref (\i -> (i+1, ()))
      (6==) <$> readCRef ref

  , djfu "A ticket contains the value of the CRef at the time of its creation" (gives' [True]) $ do
      ref  <- newCRefInt 5
      tick <- readForCAS ref
      writeCRef ref 6
      (5==) <$> peekTicket tick

  , djfu "Compare-and-swap returns a ticket containing the new value" (gives' [True]) $ do
      ref  <- newCRefInt 5
      tick <- readForCAS ref
      (_, tick') <- casCRef ref tick 6
      (6==) <$> peekTicket tick'

  , djfu "Compare-and-swap on an unmodified CRef succeeds" (gives' [True]) $ do
      ref  <- newCRefInt 5
      tick <- readForCAS ref
      (suc, _) <- casCRef ref tick 6
      val <- readCRef ref
      pure (suc && (6 == val))

  , djfu "Compare-and-swap on a modified CRef fails" (gives' [True]) $ do
      ref  <- newCRefInt 5
      tick <- readForCAS ref
      writeCRef ref 6
      (suc, _) <- casCRef ref tick 7
      val <- readCRef ref
      pure (not suc && 7 /= val)
  ]

--------------------------------------------------------------------------------

stmTests :: [TestTree]
stmTests = toTestList
  [ djfu "When a TVar is updated, its new value is visible later in same transaction" (gives' [True]) $
      (6==) <$> atomically (do { v <- newTVarInt 5; writeTVar v 6; readTVar v })

  , djfu "When a TVar is updated, its new value is visible in a later transaction" (gives' [True]) $ do
      ctv <- atomically $ newTVarInt 5
      (5==) <$> atomically (readTVar ctv)

  , djfu "Aborting a transaction blocks the thread" (gives [Left STMDeadlock])
      (atomically retry :: MonadConc m => m ()) -- avoid an ambiguous type

  , djfu "Aborting a transaction can be caught and recovered from" (gives' [True]) $ do
      ctv <- atomically $ newTVarInt 5
      atomically $ orElse retry (writeTVar ctv 6)
      (6==) <$> atomically (readTVar ctv)

  , djfu "An exception thrown in a transaction can be caught" (gives' [True]) $ do
      ctv <- atomically $ newTVarInt 5
      atomically $ catchArithException
        (throwSTM Overflow)
        (\_ -> writeTVar ctv 6)
      (6==) <$> atomically (readTVar ctv)

  , djfu "Nested exception handlers in transactions work" (gives' [True]) $ do
      ctv <- atomically $ newTVarInt 5
      atomically $ catchArithException
        (catchArrayException
          (throwSTM Overflow)
          (\_ -> writeTVar ctv 0))
        (\_ -> writeTVar ctv 6)
      (6==) <$> atomically (readTVar ctv)

  , djfu "MonadSTM is a MonadFail" (alwaysFailsWith isUncaughtException)
      (atomically $ fail "hello world" :: MonadConc m => m ())  -- avoid an ambiguous type
  ]

--------------------------------------------------------------------------------

exceptionTests :: [TestTree]
exceptionTests = toTestList
  [ djfu "An exception thrown can be caught" (gives' [True]) $
      catchArithException
        (throw Overflow)
        (\_ -> pure True)

  , djfu "Nested exception handlers work" (gives' [True]) $
      catchArithException
        (catchArrayException
          (throw Overflow)
          (\_ -> pure False))
        (\_ -> pure True)

  , djfu "Uncaught exceptions kill the computation" (alwaysFailsWith isUncaughtException) $
      catchArithException
        (throw $ IndexOutOfBounds "")
        (\_ -> pure False)

  , djfu "SomeException matches all exception types" (gives' [True]) $ do
      a <- catchSomeException
           (throw Overflow)
           (\_ -> pure True)
      b <- catchSomeException
           (throw $ IndexOutOfBounds "")
           (\_ -> pure True)
      pure (a && b)

  , djfu "Exceptions thrown in a transaction can be caught outside it" (gives' [True]) $
      catchArithException
        (atomically $ throwSTM Overflow)
        (\_ -> pure True)

  , djfu "Throwing an unhandled exception to the main thread kills it" (alwaysFailsWith isUncaughtException) $ do
      tid <- myThreadId
      throwTo tid Overflow

  , djfu "Throwing a handled exception to the main thread does not kill it" (gives' [True]) $ do
      tid <- myThreadId
      catchArithException (throwTo tid Overflow >> pure False) (\_ -> pure True)

  , djfu "MonadConc is a MonadFail" (alwaysFailsWith isUncaughtException)
      (fail "hello world" :: MonadConc m => m ())  -- avoid an ambiguous type
  ]

--------------------------------------------------------------------------------

capabilityTests :: [TestTree]
capabilityTests = toTestList
  [ djfu "Reading the capabilities twice without update gives the same result" (gives' [True]) $ do
      c1 <- getNumCapabilities
      c2 <- getNumCapabilities
      pure (c1 == c2)

  , djfu "Getting the updated capabilities gives the new value" (gives' [True]) $ do
      caps <- getNumCapabilities
      setNumCapabilities (caps + 1)
      (== caps + 1) <$> getNumCapabilities
  ]

--------------------------------------------------------------------------------

hacksTests :: [TestTree]
hacksTests = toTestList
  [ testGroup "Subconcurrency"
    [ djfuS "Failures in subconcurrency can be observed" (gives' [True]) $
        either (== Deadlock) (const False) <$>
          subconcurrency (newEmptyMVar >>= readMVar)

    , djfuS "Actions after a failing subconcurrency still happen" (gives' [True]) $ do
        var <- newMVarInt 0
        x <- subconcurrency (putMVar var 1)
        y <- readMVar var
        pure (either (==Deadlock) (const False) x && y == 0)

    , djfuS "Non-failing subconcurrency returns the final result" (gives' [True]) $ do
        var <- newMVarInt 3
        x <- subconcurrency (takeMVar var)
        pure (either (const False) (==3) x)
    ]

  , testGroup "DontCheck"
    [ djfu "Inner state modifications are visible to the outside" (gives' [True]) $ do
        outer <- dontCheck Nothing $ do
          inner <- newEmptyMVarInt
          putMVar inner 5
          pure inner
        (==5) <$> takeMVar outer

    , djfu "Failures abort the whole computation" (alwaysFailsWith isDeadlock) $
        dontCheck Nothing $ takeMVar =<< newEmptyMVarInt

    , djfu "Must be the very first thing" (alwaysFailsWith isIllegalDontCheck) $ do
        v <- newEmptyMVarInt
        dontCheck Nothing $ putMVar v 5

    , djfu "Exceeding the length bound aborts the whole computation" (alwaysFailsWith isAbort) $
        dontCheck (Just 1) $ newEmptyMVarInt >> pure ()

    , djfu "Only counts as one action towards SCT length bounding" (gives' [True]) $ do
        let ntimes = fromIntegral defaultLengthBound * 5
        dontCheck Nothing $ replicateM_ ntimes (pure ())
        pure True

    -- we use 'randomly' here because we specifically want to compare
    -- multiple executions with snapshotting
    , toTestList . testGroup "Snapshotting" $ let snapshotTest n p conc = W n conc p ("randomly", randomly (mkStdGen 0) 150) in
      [ snapshotTest "State updates are applied correctly" (gives' [2]) $ do
          r <- dontCheck Nothing $ do
            r <- newCRefInt 0
            writeCRef r 1
            writeCRef r 2
            pure r
          readCRef r

      , snapshotTest "Lifted IO is re-run (1)" (gives' [2..151]) $ do
          r <- dontCheck Nothing $ do
            r <- liftIO (IORef.newIORef (0::Int))
            liftIO (IORef.modifyIORef r (+1))
            pure r
          liftIO (IORef.readIORef r)

      , snapshotTest "Lifted IO is re-run (2)" (gives' [1]) $ do
          r <- dontCheck Nothing $ do
            let modify r f = liftIO (IORef.readIORef r) >>= liftIO . IORef.writeIORef r . f
            r <- liftIO (IORef.newIORef (0::Int))
            modify r (+1)
            pure r
          liftIO (IORef.readIORef r)

      , snapshotTest "Lifted IO is re-run (3)" (gives' [1]) $ do
          r <- dontCheck Nothing $ do
            r <- liftIO (IORef.newIORef (0::Int))
            liftIO (IORef.writeIORef r 0)
            liftIO (IORef.modifyIORef r (+1))
            pure r
          liftIO (IORef.readIORef r)
      ]
    ]
  ]

-------------------------------------------------------------------------------

ioTests :: [TestTree]
ioTests = toTestList
  [ djfu "Lifted IO is performed" (gives' [3]) $ do
      r <- liftIO (IORef.newIORef (0::Int))
      replicateM_ 3 (liftIO (IORef.atomicModifyIORef r (\i -> (i+1, ()))))
      liftIO (IORef.readIORef r)
  ]

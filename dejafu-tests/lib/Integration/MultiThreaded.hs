module Integration.MultiThreaded where

import qualified Control.Concurrent        as C
import           Control.Exception         (ArithException(..))
import           Control.Monad             (replicateM, void, when)
import           Control.Monad.IO.Class    (liftIO)
import           System.Random             (mkStdGen)
import           Test.DejaFu               (Condition(..), gives, gives',
                                            isUncaughtException, withSetup,
                                            withSetupAndTeardown)

import           Control.Concurrent.Classy hiding (newQSemN, signalQSemN,
                                            waitQSemN)
import           Control.Monad.Catch       (throwM, toException)
import qualified Data.IORef                as IORef

import           Common
import           QSemN

tests :: [TestTree]
tests =
  [ testGroup "Threading" threadingTests
  , testGroup "MVar" mvarTests
  , testGroup "IORef" iorefTests
  , testGroup "STM" stmTests
  , testGroup "Exceptions" exceptionTests
  , testGroup "Capabilities" capabilityTests
  , testGroup "Program" programTests
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
      x <- newIORef Nothing
      _ <- fork . writeIORef x $ Just ()
      readIORef x

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

  , toTestList . (\conc -> W "Shrinking can cope with re-ordering forks" conc (gives' [False, True]) ("randomly", randomly (mkStdGen 0) 150)) $ do
      v <- newEmptyMVar
      _ <- fork $ do
        _ <- fork . void $ replicateM 2 myThreadId
        _ <- fork . void $ replicateM 3 myThreadId
        putMVar v True
      _ <- fork $ do
        _ <- fork . void $ replicateM 4 myThreadId
        _ <- fork . void $ replicateM 5 myThreadId
        putMVar v False
      takeMVar v
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

iorefTests :: [TestTree]
iorefTests = toTestList
  [ djfuT "Racey IORef computations are nondeterministic" (gives' [0,1]) $ do
      x  <- newIORefInt 0
      j1 <- spawn $ writeIORef x 0
      j2 <- spawn $ writeIORef x 1
      takeMVar j1
      takeMVar j2
      readIORef x

  , djfuT "CASing IORef changes its value" (gives' [0,1]) $ do
      x <- newIORefInt 0
      _ <- fork $ modifyIORefCAS x (const (1, ()))
      readIORef x

  , djfuT "Racey CAS computations are nondeterministic" (gives' [(True, 2), (False, 2)]) $ do
      x <- newIORefInt 0
      t <- readForCAS x
      j <- spawn $ casIORef x t 1
      writeIORef x 2
      b <- fst <$> readMVar j
      v <- readIORef x
      pure (b, v)

  , djfuT "A failed CAS gives an updated ticket" (gives' [(True, 1), (True, 2)]) $ do
      x <- newIORefInt 0
      t <- readForCAS x
      v <- newEmptyMVar
      j <- spawn $ do
        o@(f, t') <- casIORef x t 1
        takeMVar v
        if f then pure o else casIORef x t' 1
      writeIORef x 2
      putMVar v ()
      b <- fst <$> readMVar j
      o <- readIORef x
      pure (b, o)

  , djfuT "A ticket is only good for one CAS" (gives' [(True, False, 1), (False, True, 2)]) $ do
      x  <- newIORefInt 0
      t  <- readForCAS x
      j1 <- spawn $ casIORef x t 1
      j2 <- spawn $ casIORef x t 2
      b1 <- fst <$> readMVar j1
      b2 <- fst <$> readMVar j2
      v  <- readIORef x
      pure (b1, b2, v)

  , djfuT "IORef writes may be delayed" (gives' [0,1]) $ do
      x <- newIORefInt 0
      writeIORef x 1
      takeMVar =<< spawn (readIORef x)
  ]

--------------------------------------------------------------------------------

stmTests :: [TestTree]
stmTests = toTestList
  [ djfuT "Transactions are atomic" (gives' [0,2]) $ do
      x <- atomically $ newTVarInt 0
      _ <- fork . atomically $ writeTVar x 1 >> writeTVar x 2
      readTVarConc x

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

  , djfuT "A mask can be escaped" (gives [Left Deadlock, Right ()]) $ do
      x <- newEmptyMVar
      y <- newEmptyMVar
      tid <- fork $ mask $ \umask -> putMVar x () >> umask (putMVar y ())
      readMVar x
      killThread tid
      readMVar y

  , djfuT "Throwing to an uninterruptible thread blocks" (gives [Left Deadlock]) $ do
      x <- newEmptyMVar
      y <- newEmptyMVar
      tid <- fork $ uninterruptibleMask $ \_ -> putMVar x () >> takeMVar y
      readMVar x
      killThread tid

  , djfuT "An uninterruptible mask can be escaped" (gives' [()]) $ do
      x <- newEmptyMVar
      y <- newEmptyMVar
      tid <- fork $ uninterruptibleMask $ \umask -> putMVar x () >> umask (takeMVar y)
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

  , djfuT "A child thread inherits the masking state of the parent" (gives [Left Deadlock]) $ do
      x <- newEmptyMVar
      y <- newEmptyMVar
      tid <- uninterruptibleMask $ \_ -> fork (putMVar x () >> takeMVar y)
      readMVar x
      killThread tid

  , djfuT "A child thread can unmask" (gives' [()]) $ do
      x <- newEmptyMVar
      y <- newEmptyMVar
      tid <- uninterruptibleMask $ \_ -> forkWithUnmask (\unmask -> putMVar x () >> unmask (takeMVar y))
      readMVar x
      killThread tid
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

programTests :: [TestTree]
programTests = toTestList
  [ testGroup "withSetup"
    [ djfuT "Inner action is run with a deterministic scheduler" (gives' [1]) $
        withSetup
          (do r <- newIORefInt 1
              _ <- fork (atomicWriteIORef r 2)
              readIORef r)
          pure

    , djfuT "Threads created by the inner action persist in the outside" (gives' [1,2]) $
        withSetup
          (do r <- newIORefInt 1
              v <- newEmptyMVar
              _ <- fork (takeMVar v >> atomicWriteIORef r 2)
              pure (r, v))
          (\(ref, trigger) -> do
              putMVar trigger ()
              readIORef ref)

    , djfuT "Bound threads created on the inside are bound on the outside" (gives' [True]) $
        withSetup
          (do v <- newEmptyMVar
              o <- newEmptyMVar
              _ <- forkOS (takeMVar v >> isCurrentThreadBound >>= putMVar o)
              pure (o, v))
          (\(out, trigger) -> do
              putMVar trigger ()
              takeMVar out)

    , djfuT "Thread IDs are consistent between the inner action and the outside" (sometimesFailsWith isUncaughtException) $
        withSetup
          (do me <- myThreadId
              v <- newEmptyMVar
              _ <- fork $ takeMVar v >> killThread me
              pure v)
        (\trigger -> putMVar trigger ())

    , djfuT "Inner action is run under sequential consistency" (gives' [1]) $
        withSetup
          (do x <- newIORefInt 0
              writeIORef x 1
              pure x)
          (\x -> takeMVar =<< spawn (readIORef x))

    , djfuTS "MVar state is preserved from setup action" (gives [Left Deadlock, Right ()]) $
        withSetup (newMVar ()) $ \v -> do
          _ <- fork $ takeMVar v
          readMVar v
    ]

  , testGroup "withSetupAndTeardown"
    [ djfuTS "Failure is observable" (gives' [Left Deadlock, Right ()]) $
        withSetupAndTeardown
          newEmptyMVar
          (\_ o -> pure o)
          (\var -> do
              _ <- fork $ putMVar var ()
              putMVar var ())

    , djfuTS "Failure does not abort the outer computation" (gives' [(Left Deadlock, ()), (Right (), ())]) $
        withSetupAndTeardown
          newEmptyMVar
          (\var res -> (,) <$> pure res <*> readMVar var)
          (\var -> do
              _ <- fork $ putMVar var ()
              putMVar var ())

    , djfuTS "Success is observable" (gives' [Right ()]) $
        withSetupAndTeardown
          (newMVar ())
          (\_ o -> pure o)
          (\var -> do
              out <- newEmptyMVar
              _ <- fork $ takeMVar var >>= putMVar out
              takeMVar out)

    -- adaptation of https://github.com/barrucadu/dejafu/issues/81
    , djfuS "Identifiers are not re-used" (gives' [(Right (),0)]) $
        withSetupAndTeardown
          (newQSemN 0)
          (\s x -> do
              o <- remainingQSemN s
              pure (x, o))
          (\s ->
             let interfere_ = waitQSemN s 0 >> signalQSemN s 0
             in signalQSemN s 0 ||| waitQSemN s 0 ||| interfere_)
    ]

    , testGroup "registerInvariant"
      [ djfuT "Invariant failure is nondeterministic if there are races" (gives [Left (InvariantFailure (toException Overflow)), Right (Just 0), Right Nothing]) $
          withSetup
            (do v <- newEmptyMVarInt
                registerInvariant (inspectMVar v >>= \x -> when (x == Just 1) (throwM Overflow))
                pure v)
            (\v -> do
                _ <- fork $ putMVar v 0
                _ <- fork $ putMVar v 1
                tryReadMVar v)
      ]
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

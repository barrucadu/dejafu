{-# LANGUAGE ScopedTypeVariables #-}

module Integration.Regressions where

import           Test.DejaFu               (exceptionsAlways, gives')

import           Control.Concurrent.Classy
import           Control.Exception         (AsyncException(..))
import qualified Control.Monad.Catch       as E
import           System.Random             (mkStdGen)

import           Common

tests :: [TestTree]
tests = toTestList
  [ djfu "https://github.com/barrucadu/dejafu/issues/40" (gives' [0,1]) $ do
      x <- newIORefInt 0
      _ <- fork $ myThreadId >> writeIORef x 1
      readIORef x

  , djfu "https://github.com/barrucadu/dejafu/issues/55" (gives' [True]) $ do
      a <- atomically newTQueue
      b <- atomically newTQueue
      _ <- fork . atomically $ writeTQueue b True
      let both x y = readTQueue x `orElse` readTQueue y `orElse` retry
      atomically $ both a b

  , djfu "https://github.com/barrucadu/dejafu/issues/111" (gives' [1]) $ do
      v <- atomically $ newTVarInt 1
      _ <- fork . atomically $ do
        writeTVar v 2
        writeTVar v 3
        retry
      readTVarConc v

  , djfu "https://github.com/barrucadu/dejafu/issues/118" exceptionsAlways $
      catchSomeException
        (uninterruptibleMask_ (throw ThreadKilled))
        (\_ -> myThreadId >>= killThread)

  , djfu "https://github.com/barrucadu/dejafu/issues/139" (gives' [()]) $
      catchSomeException
        (catchSomeException (throw ThreadKilled) (\_ -> pure ())
         >> throw ThreadKilled)
        (\_ -> pure ())

  , djfu "https://github.com/barrucadu/dejafu/issues/161" (gives' [Just (), Nothing]) $ do
      let try a = (a >> pure ()) `E.catch` (\(_ :: E.SomeException) -> pure ())
      let act s = uninterruptibleMask_ (putMVar s ())
      s <- newEmptyMVar
      t <- mask $ \restore -> fork (try (restore (act s)) >> pure ())
      killThread t
      tryReadMVar s

  , djfu "https://github.com/barrucadu/dejafu/issues/243" (gives' [1,2,3]) $ do
      setNumCapabilities 1
      _ <- fork (setNumCapabilities 2)
      _ <- fork (setNumCapabilities 3)
      getNumCapabilities

  , djfu "https://github.com/barrucadu/dejafu/issues/267" exceptionsAlways $ do
      tid <- myThreadId
      uninterruptibleMask_ (throwTo tid ThreadKilled)

  , djfu "https://github.com/barrucadu/dejafu/issues/324 (a)" (gives' [Left ThreadKilled, Left UserInterrupt]) $ do
      var <- newEmptyMVar
      tId <- uninterruptibleMask $ \restore -> fork $ do
        result <- (Right <$> restore (throw UserInterrupt)) `E.catch` (pure . Left)
        putMVar var result
      killThread tId
      v <- takeMVar var
      pure (v :: Either AsyncException ())

  , djfu "https://github.com/barrucadu/dejafu/issues/324 (b)" (gives' [Left ThreadKilled, Left UserInterrupt]) $ do
      var <- newEmptyMVar
      tId <- uninterruptibleMask $ \restore -> fork $ do
        result <- (Right <$> restore (atomically $ throwSTM UserInterrupt)) `E.catch` (pure . Left)
        putMVar var result
      killThread tId
      v <- takeMVar var
      pure (v :: Either AsyncException ())

  , (:[]) . expectFail $ testDejafuWithSettings (fromWayAndMemType (randomly (mkStdGen 0) 10) defaultMemType) "https://github.com/barrucadu/dejafu/issues/331" (gives' [1]) $
      withSetup (atomically $ newTVar (0::Int)) $ \tvar -> atomically $ do
        modifyTVar tvar (+1)
        readTVar tvar
  ]

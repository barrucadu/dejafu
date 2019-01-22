{-# LANGUAGE ScopedTypeVariables #-}

module Integration.Regressions where

import           Test.DejaFu               (exceptionsAlways, gives')

import           Control.Concurrent.Classy
import           Control.Exception         (AsyncException(..))
import qualified Control.Monad.Catch       as E
import           Test.DejaFu.Conc          (basic)

import           Common

tests :: [TestTree]
tests = toTestList
  [ djfu "https://github.com/barrucadu/dejafu/issues/40" (gives' [0,1]) $ basic $ do
      x <- newIORefInt 0
      _ <- fork $ myThreadId >> writeIORef x 1
      readIORef x

  , djfu "https://github.com/barrucadu/dejafu/issues/55" (gives' [True]) $ basic $ do
      a <- atomically newTQueue
      b <- atomically newTQueue
      _ <- fork . atomically $ writeTQueue b True
      let both x y = readTQueue x `orElse` readTQueue y `orElse` retry
      atomically $ both a b

  , djfu "https://github.com/barrucadu/dejafu/issues/111" (gives' [1]) $ basic $ do
      v <- atomically $ newTVarInt 1
      _ <- fork . atomically $ do
        writeTVar v 2
        writeTVar v 3
        retry
      readTVarConc v

  , djfu "https://github.com/barrucadu/dejafu/issues/118" exceptionsAlways $ basic $
      catchSomeException
        (uninterruptibleMask_ (throw ThreadKilled))
        (\_ -> myThreadId >>= killThread)

  , djfu "https://github.com/barrucadu/dejafu/issues/139" (gives' [()]) $ basic $
      catchSomeException
        (catchSomeException (throw ThreadKilled) (\_ -> pure ())
         >> throw ThreadKilled)
        (\_ -> pure ())

  , djfu "https://github.com/barrucadu/dejafu/issues/161" (gives' [Just (), Nothing]) $ basic $ do
      let try a = (a >> pure ()) `E.catch` (\(_ :: E.SomeException) -> pure ())
      let act s = uninterruptibleMask_ (putMVar s ())
      s <- newEmptyMVar
      t <- mask $ \restore -> fork (try (restore (act s)) >> pure ())
      killThread t
      tryReadMVar s

  , djfu "https://github.com/barrucadu/dejafu/issues/243" (gives' [1,2,3]) $ basic $ do
      setNumCapabilities 1
      _ <- fork (setNumCapabilities 2)
      _ <- fork (setNumCapabilities 3)
      getNumCapabilities
  ]

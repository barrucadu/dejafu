{-# LANGUAGE ScopedTypeVariables #-}

module Integration.Regressions where

import           Test.DejaFu               (exceptionsAlways, gives')

import           Control.Concurrent.Classy hiding (newQSemN, signalQSemN,
                                            waitQSemN)
import           Control.Exception         (AsyncException(..))
import qualified Control.Monad.Catch       as E
import           Test.DejaFu.Conc          (subconcurrency)

import           Common
import           QSemN

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

  , djfuS "https://github.com/barrucadu/dejafu/issues/71" (gives' [()]) $ do
      s <- newEmptyMVar
      _ <- subconcurrency (takeMVar s ||| pure ())
      pure ()

  , djfuS "https://github.com/barrucadu/dejafu/issues/81" (gives' [(Right (),0)]) $ do
      s <- newQSemN 0
      let interfere_ = waitQSemN s 0 >> signalQSemN s 0
      x <- subconcurrency (signalQSemN s 0 ||| waitQSemN s 0 ||| interfere_)
      o <- remainingQSemN s
      pure (x, o)

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
  ]

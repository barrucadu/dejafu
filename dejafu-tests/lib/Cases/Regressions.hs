{-# LANGUAGE ScopedTypeVariables #-}

module Cases.Regressions where

import Test.DejaFu (exceptionsAlways, gives')
import Test.Framework (Test)

import Control.Concurrent.Classy hiding (newQSemN, signalQSemN, waitQSemN)
import Control.Exception (AsyncException(..))
import qualified Control.Monad.Catch as E
import Test.DejaFu.Conc (subconcurrency)

import Common
import QSemN

tests :: [Test]
tests =
  [ djfu "https://github.com/barrucadu/dejafu/issues/40" (gives' [0,1]) $ do
      x <- newCRefInt 0
      _ <- fork $ myThreadId >> writeCRef x 1
      readCRef x

  , djfu "https://github.com/barrucadu/dejafu/issues/55" (gives' [True]) $ do
      a <- atomically $ newTQueue
      b <- atomically $ newTQueue
      _ <- fork . atomically $ writeTQueue b True
      let both x y = readTQueue x `orElse` readTQueue y `orElse` retry
      atomically $ both a b

  , djfu "https://github.com/barrucadu/dejafu/issues/71" (gives' [()]) $ do
      let ma ||| mb = do { j1 <- spawn ma; j2 <- spawn mb; takeMVar j1; takeMVar j2; pure () }
      s <- newEmptyMVar
      _ <- subconcurrency (takeMVar s ||| pure ())
      pure ()

  , djfu "https://github.com/barrucadu/dejafu/issues/81" (gives' [(Right (),0)]) $ do
      s <- newQSemN 0
      let interfere = waitQSemN s 0 >> signalQSemN s 0
      x <- subconcurrency (signalQSemN s 0 ||| waitQSemN s 0 ||| interfere)
      o <- remainingQSemN s
      pure (x, o)

  , djfu "https://github.com/barrucadu/dejafu/issues/111" (gives' [1]) $ do
      v <- atomically $ newTVarInt 1
      _ <- fork . atomically $ do
        writeTVar v 2
        writeTVar v 3
        retry
      atomically $ readTVar v

  , djfu "https://github.com/barrucadu/dejafu/issues/118" exceptionsAlways $ do
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
  ]

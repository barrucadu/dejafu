{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Clock
-- Copyright: Copyright © 2017 Lars Kuhtz <lakuhtz@gmail.com>.
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
-- A sharded clock for use with DejaFu.
--
-- The implementation
--
-- *   avoids unbounded computations by stoping the clock if
--     not timer is active,
-- *   tries not introduce more non-determinism than needed into
--     the system.
--
-- The default value of 'boundsFair' is often not enough when working
-- with a clock. You may trie to increase that value aggressively.
--
module Clock
( Ticks
, Timer
, Clock
, TimeoutException(..)
, withClock
, sleep
, withTimeout
, withTimeout'

-- * Tests
, runTest
, runAllTests
, tests
) where

import Control.Concurrent.Classy
import Control.Monad.Catch (SomeException, Exception, catchAll, toException)
import Control.Monad

import Data.Maybe (isJust)

import Numeric.Natural

-- Tests

import Control.Concurrent.Classy.Async
import Data.Monoid
import Test.DejaFu hiding (check, runTest)
import Test.DejaFu.Conc hiding (check, runTest, ThreadId)
import System.Random

-- -------------------------------------------------------------------------- --
-- Public Interface

newtype Ticks = Ticks Natural
    deriving (Show, Eq, Ord, Enum, Num)

data TimeoutException = TimeoutException deriving (Show, Eq, Ord)
instance Exception TimeoutException

withClock ∷ ∀ m a . MonadConc m ⇒ (Clock m → m a) → m a
withClock = withClockInternal

sleep ∷ MonadConc m ⇒ Clock m → Ticks→ m ()
sleep clock ticks = setTimer clock ticks >>= waitTimer

-- The timeout isn't prompt. If this function returns
-- a value or throws an exception other than TimeoutException,
-- it is guaranteed that it completed within the
-- given number of ticks. If it throws a TimeoutException
-- the computation may have consumed any amount of ticks and
-- it may throw after any amount of ticks.
-- (If the timeout exception isn't thrown by an inner
-- call to `assertTimeout`, it is guaranteed that the
-- computation consumed at least the given number of ticks.)
--
withTimeout ∷ MonadConc m ⇒ Clock m → Ticks → m a → m a
withTimeout clock ticks inner = mask $ \umask → do
    timer ← setTimer clock ticks
    r ← umask inner `catchAll` \e → tryWaitTimer timer >>= \case
        False → throw e
        True → throw TimeoutException
    tryWaitTimer timer >>= \case
        False → return r
        True → throw TimeoutException

-- If this function throws it is guaranteed that the exception
-- is raised within the given number of ticks and no work
-- is done by the computation after that.
--
withTimeout' ∷ MonadConc m ⇒ Clock m → Ticks → m a → m a
withTimeout' clock ticks inner =
    wait =<< (asyncN "withtimeout" $ setTimeout clock ticks >> inner)

data Clock m = Clock
    { timers ∷ !(MVar m [(Ticks, Either (Timeout m) (Timer m))])
    , active ∷ !(MVar m ())
    }

newtype Timer m = Timer (MVar m ())
newtype Timeout m = Timeout (ThreadId m)

-- -------------------------------------------------------------------------- --
-- Internal

withClockInternal ∷ ∀ m a . MonadConc m ⇒ (Clock m → m a) → m a
withClockInternal inner = do
    clock ← Clock <$> newMVarN "timers" [] <*> newEmptyMVarN "active"
    mask $ \umask → do
        tid ← forkN "clock" $ umask $ forever $ do
            readMVar (active clock)
            tick clock
            -- yield
        r ← umask (inner clock) `catchAll` \e → do
            killThread tid
            throw e
        killThread tid
        return r
  where
    tick ∷ Clock m → m ()
    tick (Clock var activeVar) = modifyMVar_ var $ \timers → do
        r ← go timers
        when (null r) $ takeMVar activeVar
        return r

    go
        ∷ [(Ticks, Either (Timeout m) (Timer m))]
        → m [(Ticks, Either (Timeout m) (Timer m))]
    go [] = pure []
    go ((x, Right (Timer var)) : t)
        | x <= 0 = putMVar var () >> go t
        | otherwise = (:) (x - 1, Right (Timer var)) <$> go t
    go ((x, Left (Timeout target)) : t)
        | x <= 0 = throwTo target TimeoutException *> go t
        | otherwise = (:) (x - 1, Left (Timeout target)) <$> go t

setTimer ∷ MonadConc m ⇒ Clock m → Ticks → m (Timer m)
setTimer clock ticks = do
    var ← newEmptyMVar
    modifyMVar_ (timers clock) $ pure . (:) (ticks, Right (Timer var))
    tryPutMVar (active clock) ()
    return $ Timer var

waitTimer ∷ MonadConc m ⇒ Timer m → m ()
waitTimer (Timer var) = void $ readMVar var

tryWaitTimer ∷ MonadConc m ⇒ Timer m → m Bool
tryWaitTimer (Timer var) = isJust <$> tryReadMVar var

-- -------------------------------------------------------------------------- --
-- Asynchronous Timeouts

setTimeout ∷ MonadConc m ⇒ Clock m → Ticks → m ()
setTimeout clock ticks = do
    tid ← myThreadId
    modifyMVar_ (timers clock) $ pure . (:) (ticks, Left (Timeout tid))
    void $ tryPutMVar (active clock) ()

-- -------------------------------------------------------------------------- --
-- Tests

timeout = Left $ UncaughtException $ toException TimeoutException

data Test m a = Test
    { description ∷ String
    , result ∷ !(Predicate a)
    , test ∷ m a
    }

test1 ∷ MonadConc m ⇒ Test m Int
test1 = Test "" (gives [Right 1, Right 2]) $
    withClock $ \c → do
        var ← newCRef 0
        r₁ ← spawn $ sleep c 2 >> atomicWriteCRef var 1
        r₂ ← spawn $ sleep c 1 >> atomicWriteCRef var 2
        void $ readMVar r₁
        void $ readMVar r₂
        readCRef var

test2 ∷ MonadConc m ⇒ Test m Int
test2 = Test "" (gives [Right 1, Right 2, timeout]) $
    withClock $ \c → do
        var ← newCRef 0
        r₁ ← async $ sleep c 2 >> atomicWriteCRef var 1
        r₂ ← async $ withTimeout c 1 $ atomicWriteCRef var 2
        wait r₁
        wait r₂
        readCRef var

test3 ∷ MonadConc m ⇒ Test m Int
test3 = Test "" (gives [Right 1, Right 2]) $
    withClock $ \c → do
        var ← newCRef 0
        r₁ ← spawn $ sleep c 2 >> atomicWriteCRef var 1
        r₂ ← spawn $ atomicWriteCRef var 2
        void $ readMVar r₁
        void $ readMVar r₂
        readCRef var

test4 ∷ MonadConc m ⇒ Test m ()
test4 = Test "" (gives [timeout]) $
    withClock $ \c → withTimeout c 1 $ sleep c 2

test5 ∷ MonadConc m ⇒ Test m ()
test5 = Test "" (representative deadlocksNever) $
    withClock $ \c → sleep c 2

test6 ∷ MonadConc m ⇒ Test m ()
test6 = Test "" (gives [Right(), timeout]) $
    withClock $ \c → withTimeout c 2 $ return ()

test7 ∷ MonadConc m ⇒ Test m ()
test7 = Test "" (gives [timeout, Right ()]) $
    withClock $ \c → do
        withTimeout c 0 $ return ()
        withTimeout c 1 $ return ()
        withTimeout c 3 $ return ()

test8 ∷ MonadConc m ⇒ Test m ()
test8 = Test "" (gives [Right (), timeout]) $
    withClock $ \c → withTimeout c 2 $ withTimeout c 2 $ return ()

test9 ∷ MonadConc m ⇒ Test m ()
test9 = Test "" (gives [Right (), timeout]) $
    withClock $ \c → withTimeout c 1 $ withTimeout c 2 $ return ()

test10 ∷ MonadConc m ⇒ Test m ()
test10 = Test "" (gives [Right (), timeout]) $
    withClock $ \c → withTimeout c 2 $ withTimeout c 1 $ return ()

test11 ∷ MonadConc m ⇒ Test m ()
test11 = Test "" (gives [Right ()]) $
    withClock $ \c → sleep c 1 >> sleep c 1

test12 ∷ MonadConc m ⇒ Test m ()
test12 = Test "" (gives [timeout]) $
    withClock $ \c → sleep c 1 >> withTimeout c 1 (sleep c 2)

test13 ∷ MonadConc m ⇒ Test m ()
test13 = Test "" (gives [timeout]) $
    withClock $ \c → do
        sleep c 1
        withTimeout c 1 $ do
            sleep c 2
            sleep c 2
        sleep c 1

test14 ∷ MonadConc m ⇒ Test m ()
test14 = Test "" (gives [Right (), timeout]) $
    withClock $ \c → do
        sleep c 1
        withTimeout c 5 $ do
            sleep c 2
            sleep c 2
        sleep c 1

-- -------
-- tests for withTimeout'

test15 ∷ MonadConc m ⇒ Test m ()
test15 = Test "withTimeout'" (gives [Right (), timeout]) $
    withClock $ \c → withTimeout' c 1 $ return ()

test16 ∷ MonadConc m ⇒ Test m ()
test16 = Test "withTimeout'" (gives [timeout]) $
    withClock $ \c → withTimeout' c 1 $ sleep c 2

test17 ∷ MonadConc m ⇒ Test m ()
test17 = Test "withTimeout'" (gives [Right (), timeout]) $
    withClock $ \c → withTimeout' c 2 $ sleep c 2

test18 ∷ MonadConc m ⇒ Test m Int
test18 = Test "" (gives [Right 1, Right 2, timeout]) $
    withClock $ \c → do
        var ← newCRef 0
        r₁ ← async $ sleep c 2 >> atomicWriteCRef var 1
        r₂ ← async $ withTimeout' c 1 $ atomicWriteCRef var 2
        wait r₁
        wait r₂
        readCRef var

test19 ∷ MonadConc m ⇒ Test m Int
test19 = Test "" (gives [Right 1, Right 2, timeout]) $
    withClock $ \c → do
        var ← newCRef 0
        withTimeout' c 3 $ do
            r₁ ← async $ sleep c 1 >> atomicWriteCRef var 1
            sleep c 1
            r₂ ← async $ withTimeout' c 1 $ atomicWriteCRef var 2
            wait r₁
            wait r₂
        readCRef var

test20 ∷ MonadConc m ⇒ Test m Int
test20 = Test "" (gives [Right 1, Right 2, timeout]) $
    withClock $ \c → do
        var ← newCRef 0
        withTimeout' c 4 $ do
            r₁ ← async $ sleep c 2 >>  atomicWriteCRef var 1
            sleep c 1 -- Why is this needed?
            r₂ ← async $ withTimeout' c 2 $ atomicWriteCRef var 2
            wait r₁
            wait r₂
        readCRef var

test21 ∷ MonadConc m ⇒ Test m Int
test21 = Test "" (gives [Right 2, timeout]) $
    withClock $ \c → do
        var ← newCRef 0
        withTimeout' c 3 $ do
            r₁ ← async $ sleep c 2 >>  atomicWriteCRef var 1
            sleep c 1 -- Why is this needed?
            r₂ ← async $ withTimeout' c 2 $ atomicWriteCRef var 2
            wait r₁
            wait r₂
        readCRef var

-- -------------------------------------------------------------------------- --
-- running tests

runTestRandom ∷ (Show a) ⇒ Int → (∀ t . Test (ConcST t) a) → IO Bool
runTestRandom i t = do
    gen ← newStdGen
    runTest (randomly gen i) t

runTestDefault ∷ (Show a) ⇒ (∀ t . Test (ConcST t) a) → IO Bool
runTestDefault = runTest (systematically bounds)
  where
    bounds = defaultBounds
        { boundLength = Nothing
        , boundPreemp = Just 2
        , boundFair = Just 25
        }

runTest ∷ (Show a) ⇒ Way → (∀ t . Test (ConcST t) a) → IO Bool
runTest way t = dejafuWay way SequentialConsistency
    (test t)
    (description t, result t)

data SomeTest where
    SomeTest ∷ (Show a) ⇒ (∀ t . Test (ConcST t) a) → SomeTest

runAllTests ∷ IO Bool
runAllTests = (getAll . foldMap All) <$> forM ([1∷Int ..] `zip` tests) go
  where
    go (i, SomeTest t) = do
        putStr (show i <> ": ")
        runTestDefault t

tests ∷ [SomeTest]
tests =
    [ SomeTest test1
    , SomeTest test2
    , SomeTest test3
    , SomeTest test4
    , SomeTest test5
    , SomeTest test6
    , SomeTest test7
    , SomeTest test8
    , SomeTest test9
    , SomeTest test10
    , SomeTest test11
    , SomeTest test12
    , SomeTest test13
    , SomeTest test14
    , SomeTest test15
    , SomeTest test16
    , SomeTest test17
    , SomeTest test18
    , SomeTest test19
    , SomeTest test20
    , SomeTest test21
    ]

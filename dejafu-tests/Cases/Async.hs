{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Cases.Async where

import Control.Concurrent.Classy.Async
import Control.Concurrent.Classy.CRef
import Control.Exception (AsyncException(..), Exception, SomeException, fromException)
import Control.Monad (forever, when)
import Control.Monad.Catch (finally, try)
import Control.Monad.Conc.Class hiding (threadDelay)
import Data.List (sort)
import Data.Maybe (isJust, isNothing)
import Data.Typeable (Typeable)
import Test.DejaFu (alwaysTrue)
import Test.DejaFu.Conc (ConcST)

import Common

{-
Tests from https://github.com/simonmar/async/blob/master/test/test-async.hs

The following are omitted:

  * withasync_waitCatch_blocked: because dejafu does not do
    BlockedIndefinitelyOnMVar

  * concurrently+success, concurrently+failure, race+success,
    race+failure, cancel, withAsync: because they rely on timing
-}

tests :: [Test]
tests =
  [ testGroup "async"
    [ testCase "async_wait" async_wait
    , testCase "async_waitCatch"   async_waitCatch
    , testCase "async_exwait"      async_exwait
    , testCase "async_exwaitCatch" async_exwaitCatch
    , testCase "async_cancel" async_cancel
    , testCase "async_poll"   async_poll
    , testCase "async_poll2"  async_poll2
    ]

  , testGroup "withAsync"
    [ testCase "withasync_waitCatch" withasync_waitCatch
    , testCase "withasync_wait2"     withasync_wait2
    ]

  , testGroup "concurrently"
    [ testCase "concurrently_" case_concurrently_
    , testCase "replicateConcurrently_" case_replicateConcurrently
    , testCase "replicateConcurrently"  case_replicateConcurrently_
    ]
  ]

value :: Int
value = 42

data TestException = TestException deriving (Eq,Show,Typeable)
instance Exception TestException

async_waitCatch :: MonadConc m => m ()
async_waitCatch = do
  a <- async (return value)
  r <- waitCatch a
  case r of
    Left _  -> assertFailure ""
    Right e -> e @?= value

async_wait :: MonadConc m => m ()
async_wait = do
  a <- async (return value)
  r <- wait a
  assertEqual "async_wait" r value

async_exwaitCatch :: MonadConc m => m ()
async_exwaitCatch = do
  a <- async (throwIO TestException)
  r <- waitCatch a
  case r of
    Left e  -> fromException e @?= Just TestException
    Right _ -> assertFailure ""

async_exwait :: MonadConc m => m ()
async_exwait = do
  a <- async (throwIO TestException)
  (wait a >> assertFailure "") `catch` \e -> e @?= TestException

withasync_waitCatch :: MonadConc m => m ()
withasync_waitCatch = do
  withAsync (return value) $ \a -> do
    r <- waitCatch a
    case r of
      Left _  -> assertFailure ""
      Right e -> e @?= value

withasync_wait2 :: MonadConc m => m ()
withasync_wait2 = do
  a <- withAsync (threadDelay 1000000) $ return
  r <- waitCatch a
  case r of
    Left e  -> fromException e @?= Just ThreadKilled
    Right _ -> assertFailure ""

async_cancel :: MonadConc m => m ()
async_cancel = do
  a <- async (return value)
  cancelWith a TestException
  r <- waitCatch a
  case r of
    Left e -> fromException e @?= Just TestException
    Right r -> r @?= value

async_poll :: MonadConc m => m ()
async_poll = do
  a <- async (threadDelay 1000000)
  r <- poll a
  when (isJust r) $ assertFailure ""
  r <- poll a   -- poll twice, just to check we don't deadlock
  when (isJust r) $ assertFailure ""

async_poll2 :: MonadConc m => m ()
async_poll2 = do
  a <- async (return value)
  wait a
  r <- poll a
  when (isNothing r) $ assertFailure ""
  r <- poll a   -- poll twice, just to check we don't deadlock
  when (isNothing r) $ assertFailure ""

case_concurrently_ :: MonadConc m => m ()
case_concurrently_ = do
  ref <- newCRef 0
  () <- concurrently_
    (atomicModifyCRef ref (\x -> (x + 1, True)))
    (atomicModifyCRef ref (\x -> (x + 2, 'x')))
  res <- readCRef ref
  res @?= 3

case_replicateConcurrently :: MonadConc m => m ()
case_replicateConcurrently = do
  ref <- newCRef 0
  let action = atomicModifyCRef ref (\x -> (x + 1, x + 1))
  resList <- replicateConcurrently 4 action
  resVal <- readCRef ref
  resVal @?= 4
  sort resList @?= [1..4]

case_replicateConcurrently_ :: MonadConc m => m ()
case_replicateConcurrently_ = do
  ref <- newCRef 0
  let action = atomicModifyCRef ref (\x -> (x + 1, x + 1))
  () <- replicateConcurrently_ 4 action
  resVal <- readCRef ref
  resVal @?= 4

-------------------------------------------------------------------------------

data TestFailed = TestFailed String deriving (Eq,Show,Typeable)
instance Exception TestFailed

assertFailure :: MonadConc m => String -> m b
assertFailure = throw . TestFailed

throwIO :: (Exception e, MonadConc m) => e -> m a
throwIO = throw

-- the tests use 'threadDelay' with a big delay to represent a blocked thread
threadDelay :: MonadConc m => Int -> m ()
threadDelay _ = forever yield

(@?=) :: (Eq a, MonadConc m) => a -> a -> m ()
(@?=) = assertEqual "not equal"

assertEqual :: (Eq a, MonadConc m) => String -> a -> a -> m ()
assertEqual err a1 a2
  | a1 == a2  = pure ()
  | otherwise = assertFailure err

testCase :: String -> (forall t. ConcST t ()) -> Test
testCase name c = djfu name (alwaysTrue p) (try c) where
  p (Right (Left (e::SomeException))) = False
  p (Right _) = True
  p (Left _)  = False

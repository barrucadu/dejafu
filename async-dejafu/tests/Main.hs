{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent.Async
import Control.Exception (AsyncException(..), BlockedIndefinitelyOnMVar(..), Exception, SomeException(..), fromException)
import Control.Monad (forever)
import Control.Monad.Conc.Class
import Data.Functor (void)
import Data.Typeable (Typeable, cast)
import Test.DejaFu hiding (MemType(..))
import Test.HUnit (Test(..), runTestTT, test)
import Test.HUnit.DejaFu

main :: IO ()
main = void . runTestTT $ TestList
  [ TestLabel "async" $ test
    [ testDejafu async_wait        "async_wait"        $ alwaysTrue (==Right value)
    , testDejafu async_waitCatch   "async_waitCatch"   $ alwaysTrue (\case Right (Right v) -> v == value; _ -> False)
    , testDejafu async_exwait      "async_exwait"      $ alwaysTrue (==Right (Just TestException))
    , testDejafu async_exwaitCatch "async_exwaitCatch" $ alwaysTrue (==Right (Just TestException))
    , testDejafu async_cancel      "async_cancel"      $ gives' [Left (Just TestException), Right value]
    , testDejafu async_poll        "async_poll"        $ alwaysTrue (\case Right Nothing -> True; _ -> False)
    , testDejafu async_poll2       "async_poll2"       $ alwaysTrue (\case Right (Just (Right v)) -> v == value; _ -> False)
    ]

  , TestLabel "withAsync" $ test
    [ testDejafu withasync_waitCatch "withasync_waitCatch" $ alwaysTrue (\case Right (Right v) -> v == value; _ -> False)
    , testDejafu withasync_wait2     "withasync_wait2"     $ alwaysTrue (\case Right (Left (Just ThreadKilled)) -> True; _ -> False)

    -- this fails because dejafu doesn't throw 'BlockedIndefinitelyOnMVar' in testing yet
    -- , testDejafu withasync_waitCatch_blocked "withasync_waitCatch_blocked" $ alwaysTrue (\case Right (Just BlockedIndefinitelyOnMVar) -> True; _ -> False)
    ]
  ]

value :: Int
value = 42

data TestException = TestException deriving (Eq,Show,Typeable)
instance Exception TestException

async_wait :: MonadConc m => m Int
async_wait = do
  a <- async $ return value
  wait a

async_waitCatch :: MonadConc m => m (Either SomeException Int)
async_waitCatch = do
  a <- async $ return value
  waitCatch a

async_exwait :: MonadConc m => m (Maybe TestException)
async_exwait = do
  a <- async $ throw TestException
  (wait a >> return Nothing) `catch` (return . Just)

async_exwaitCatch :: MonadConc m => m (Maybe TestException)
async_exwaitCatch = do
  a <- async $ throw TestException
  r <- waitCatch a
  return $ case r of
    Left  e -> fromException e
    Right _ -> Nothing

async_cancel :: MonadConc m => m (Either (Maybe TestException) Int)
async_cancel = do
  a <- async $ return value
  cancelWith a TestException
  r <- waitCatch a
  return $ case r of
    Left  e -> Left $ fromException e
    Right v -> Right v

async_poll :: MonadConc m => m (Maybe (Either SomeException Int))
async_poll = do
  a <- async $ forever yield
  poll a

async_poll2 :: MonadConc m => m (Maybe (Either SomeException Int))
async_poll2 = do
  a <- async $ return value
  wait a
  poll a

withasync_waitCatch :: MonadConc m => m (Either SomeException Int)
withasync_waitCatch = withAsync (return value) waitCatch

withasync_wait2 :: MonadConc m => m (Either (Maybe AsyncException) ())
withasync_wait2 = do
  a <- withAsync (forever yield) return
  r <- waitCatch a
  return $ case r of
    -- dejafu-0.2 needs the 'cast', whereas dejafu-0.3 needs the
    -- 'fromException'. This is due to me fixing some bugs with
    -- exception handling in the test implementations.
    Left  e@(SomeException e') -> Left (fromException e <|> cast e')
    Right x -> Right x

withasync_waitCatch_blocked :: MonadConc m => m (Maybe BlockedIndefinitelyOnMVar)
withasync_waitCatch_blocked = do
  r <- withAsync (newEmptyMVar >>= takeMVar) waitCatch
  return $ case r of
    Left e -> fromException e
    _      -> Nothing

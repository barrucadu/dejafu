{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}

module Main where

import Control.Concurrent.Async
import Control.Exception (BlockedIndefinitelyOnMVar(..), Exception, SomeException, fromException)
import Control.Monad (forever)
import Control.Monad.Conc.Class
import Data.Functor (void)
import Data.Typeable (Typeable)
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

    -- this doesn't quite work as dejafu can't deliver the exception in time! @return@ can't be pre-empted, this is
    -- a bug which may require migrating away from Cont to fix.
    -- , testDejafu async_cancel      "async_cancel"      $ gives [Left (Just TestException), Right value]

    -- this hangs dejafu doesn't cope well with forever (return ())
    -- , testDejafu async_poll        "async_poll"        $ alwaysTrue (\case Right Nothing -> True; _ -> False)

    , testDejafu async_poll2       "async_poll2"       $ alwaysTrue (\case Right (Just (Right v)) -> v == value; _ -> False)
    ]

  , TestLabel "withAsync" $ test
    [ testDejafu withasync_waitCatch "withasync_waitCatch" $ alwaysTrue (\case Right (Right v) -> v == value; _ -> False)

    -- this hangs because dejafu doesn't cope well with @forever (return ())@
    -- , testDejafu withasync_wait2     "withasync_wait2"     $ alwaysTrue (\case Right (Left _) -> True; _ -> False)

    -- this fails because dejafu doesn't throw 'BlockedIndefinitelyOnMVar' in testing yet
    -- , testDejafu withasync_waitCatch_blocked "withasync_waitCatch_blocked" $ alwaysTrue (\case Right (Just BlockedIndefinitelyOnMVar) -> True; _ -> False)
    ]
  ]

  where
    -- Taken from the dejafu test suite. Because this has been used in
    -- two separate places, it's probably worth bringing into dejafu
    -- proper.
    gives :: Eq a => [a] -> Predicate a
    gives expected results = go expected [] results Result { _pass = False, _casesChecked = 0, _failures = failures } where
      go waitingFor alreadySeen ((Right x, _):xs) res
        | x `elem` waitingFor  = go (filter (/=x) waitingFor) (x:alreadySeen) xs res { _casesChecked = _casesChecked res + 1 }
        | x `elem` alreadySeen = go waitingFor alreadySeen xs res { _casesChecked = _casesChecked res + 1 }
        | otherwise = res { _casesChecked = _casesChecked res + 1 }
      go waitingFor alreadySeen (_:xs) res = go waitingFor alreadySeen xs res
      go [] _ [] res = res { _pass = True }
      go _ _ _ res = res

      failures = filter (\(r, _) -> either (const True) (`notElem` expected) r) results

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
  a <- async . forever $ return ()
  poll a

async_poll2 :: MonadConc m => m (Maybe (Either SomeException Int))
async_poll2 = do
  a <- async $ return value
  wait a
  poll a

withasync_waitCatch :: MonadConc m => m (Either SomeException Int)
withasync_waitCatch = withAsync (return value) waitCatch

withasync_wait2 :: MonadConc m => m (Either SomeException ())
withasync_wait2 = do
  a <- withAsync (forever $ return ()) return
  waitCatch a

withasync_waitCatch_blocked :: MonadConc m => m (Maybe BlockedIndefinitelyOnMVar)
withasync_waitCatch_blocked = do
  _concAllKnown
  r <- withAsync (_concAllKnown >> newEmptyCVar >>= takeCVar) waitCatch
  return $ case r of
    Left e -> fromException e
    _      -> Nothing

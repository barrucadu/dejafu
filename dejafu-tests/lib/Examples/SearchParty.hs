{-# LANGUAGE Rank2Types #-}

{-
The Search Party library:
https://github.com/barrucadu/search-party

Originally intended as a first nontrivial test for dejafu, I found a
bug with the initial implementation of result lists, which was later
replaced with a @Stream@ type. This is a trimmed down version with
most of the functions not essential to exhbiting the bug removed.
-}

-- | Concurrent nondeterministic search.
module Examples.SearchParty where

import           Control.Concurrent.Classy.STM.TMVar (TMVar, isEmptyTMVar,
                                                      newEmptyTMVar, putTMVar,
                                                      readTMVar, tryPutTMVar,
                                                      tryTakeTMVar)
import           Control.Monad                       (unless, when)
import           Control.Monad.Conc.Class
import qualified Control.Monad.Fail                  as F
import           Control.Monad.STM.Class
import           Data.Functor                        (void)
import           Data.Maybe                          (fromJust, isNothing)

-- test imports
import           Data.List                           (sort)
import           Test.DejaFu                         (alwaysSameOn)

import           Common

import           Examples.SearchParty.Impredicative

tests :: [TestTree]
tests =
  [ testDejafu "concurrent filter" (failing checkResultLists) concFilter
  ]

-- | Filter a list concurrently.
concFilter :: MonadConc m => m [Int]
concFilter = unsafeRunFind $ [0..5] @! const True

-- | Check that two lists of results are equal, modulo order.
checkResultLists :: Ord a => Predicate [a]
checkResultLists = alwaysSameOn sort

-------------------------------------------------------------------------------

-- | A value of type @Find m a@ represents a concurrent search
-- computation (happening in the 'MonadConc' monad @m@) which may
-- produce a value of type @a@, or fail. If a value can be returned,
-- one will be (although it's nondeterministic which one will actually
-- be returned). Usually you will be working with values of type @Find
-- IO a@, but the generality allows for testing.
--
-- You should prefer using the 'Applicative' instance over the 'Monad'
-- instance if you can, as the 'Applicative' preserves parallelism.
newtype Find m a = Find { unFind :: m (WorkItem m a) }

-------------------------------------------------------------------------------
-- Instances

-- | 'fmap' delays applying the function until the value is demanded,
-- to avoid blocking.
instance MonadConc m => Functor (Find m) where
  fmap g (Find mf) = Find $ fmap g <$> mf

-- | '<*>' performs both computations in parallel, and immediately
-- fails as soon as one does, giving a symmetric short-circuiting
-- behaviour.
instance MonadConc m => Applicative (Find m) where
  pure a = Find . workItem' $ Just a

  (Find mf) <*> (Find ma) = Find $ do
    f <- mf
    a <- ma

    success_ <- blockOn [void f, void a]

    if success_
    then do
      fres <- unsafeResult f
      ares <- unsafeResult a

      workItem' . Just $ fres ares

    else workItem' Nothing

-- | '>>=' should be avoided, as it necessarily imposes sequencing,
-- and blocks until the value being bound has been computed.
instance MonadConc m => Monad (Find m) where
  return = pure

  (Find mf) >>= g = Find $ do
    f   <- mf
    res <- result f

    unFind $ case res of
      Just a  -> g a
      Nothing -> F.fail ""

instance MonadConc m => F.MonadFail (Find m) where
    fail _ = Find $ workItem' Nothing

--------------------------------------------------------------------------------
-- Execution

-- | Unsafe version of 'runFind'. This will error at runtime if the
-- computation fails.
unsafeRunFind :: MonadConc m => Find m a -> m a
unsafeRunFind (Find mf) = mf >>= unsafeResult

--------------------------------------------------------------------------------
-- Basic Searches

-- | Return all elements of a list satisfying a predicate, the order
-- may not be consistent between executions.
(@!) :: MonadConc m => [a] -> (a -> Bool) -> Find m [a]
as @! f = allOf [if f a then success a else failure | a <- as]

-- | Search which always succeeds.
success :: MonadConc m => a -> Find m a
success = pure

-- | Search which always fails.
failure :: MonadConc m => Find m a
failure = fail ""

-- | Return all non-failing results, the order is nondeterministic.
allOf :: MonadConc m => [Find m a] -> Find m [a]
allOf [] = success []
allOf as = Find $ do
  (var, kill) <- work False $ map unFind as
  pure $ workItem var id kill

-------------------------------------------------------------------------------
-- Combinators

-- INTERNAL --

-------------------------------------------------------------------------------
-- Types

-- See SearchPartyImpred.hs

-------------------------------------------------------------------------------
-- Processing work items

-- | Block until all computations interested in have successfully
-- completed. If any fail, this immediately returns 'False' and kills
-- the still-running ones.
blockOn :: MonadConc m => [WorkItem m ()] -> m Bool
blockOn fs = do
  -- Block until one thing fails, or everything succeeds.
  success_ <- atomically $ do
    states <- mapM getState fs
    case (HasFailed `elem` states, all (==HasSucceeded) states) of
      (True, _) -> pure False
      (_, True) -> pure True
      _ -> retry

  -- Kill everything if something failed.
  unless success_ $ mapM_ (\x -> _killme (unWrap x)) fs

  pure success_

-- | Get the result of a computation, this blocks until the result is
-- present, so be careful not to lose parallelism.
result :: MonadConc m => WorkItem m a -> m (Maybe a)
result f = fmap (_mapped $ unWrap f) <$> res where
  res = atomically . readTMVar . _result $ unWrap f

-- | Unsafe version of 'result', this will error at runtime if the
-- computation fails.
unsafeResult :: MonadConc m => WorkItem m a -> m a
unsafeResult = fmap fromJust . result

-- | Get the current state of a work item.
getState :: MonadConc m => WorkItem m a -> STM m WorkState
getState f = do
  empty <- isEmptyTMVar . _result $ unWrap f
  if empty
  then pure StillComputing
  else do
    failed <- hasFailed f
    pure $ if failed then HasFailed else HasSucceeded

-- | Check if a work item has failed. If the computation has not
-- terminated, this immediately returns 'False'.
hasFailed :: MonadConc m => WorkItem m a -> STM m Bool
hasFailed f = do
  working <- isEmptyTMVar . _result $ unWrap f
  if working
  then pure False
  else isNothing <$> readTMVar (_result $ unWrap f)

-------------------------------------------------------------------------------
-- Work stealing

-- | Push a batch of work to the queue, returning a 'TMVar' that can
-- be blocked on to get the result, and an action that can be used to
-- kill the computation. If the first argument is true, as soon as one
-- succeeds, the others are killed; otherwise all results are
-- gathered.
work :: MonadConc m => Bool -> [m (WorkItem m a)] -> m (TMVar (STM m) (Maybe [a]), m ())
work shortcircuit workitems = do
  res    <- atomically newEmptyTMVar
  kill   <- atomically newEmptyTMVar
  caps   <- getNumCapabilities
  dtid   <- fork $ driver caps res kill
  killme <- atomically $ readTMVar kill

  pure (res, killme >> killThread dtid)

  where
    -- If there's only one capability don't bother with threads.
    driver 1 res kill = do
      atomically . putTMVar kill $ failit res
      remaining <- newIORef workitems
      process remaining res

    -- Fork off as many threads as there are capabilities, and queue
    -- up the remaining work.
    driver caps res kill = do
      remaining <- newIORef workitems
      tids <- mapM (\cap -> forkOn cap $ process remaining res) [0..caps-1]

      -- Construct an action to short-circuit the computation.
      atomically . putTMVar kill $ failit res >> mapM_ killThread tids

      -- If short-circuiting, block until there is a result then kill
      -- any still-running threads.
      when shortcircuit $ do
        _ <- atomically $ readTMVar res
        mapM_ killThread tids

    -- Process a work item and store the result if it is a success,
    -- otherwise continue.
    process remaining res = do
      mitem <- atomicModifyIORef remaining $ \rs -> if null rs then ([], Nothing) else (tail rs, Just $ head rs)
      case mitem of
        Just item -> do
          fwrap  <- item
          maybea <- result fwrap

          case maybea of
            Just a -> atomically $ do
              val <- tryTakeTMVar res
              case val of
                Just (Just as) -> putTMVar res $ Just (a:as)
                _ -> putTMVar res $ Just [a]
            Nothing -> process remaining res
        Nothing -> failit res

    -- Record that a computation failed.
    failit res = atomically . void $ tryPutTMVar res Nothing

-- |
-- Module      : Control.Concurrent.Classy.STM.TVar
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : portable
--
-- Transactional variables, for use with 'MonadSTM'.
--
-- __Deviations:__ There is no @Eq@ instance for @MonadSTM@ the @TVar@
-- type. Furthermore, the @newTVarIO@ and @mkWeakTVar@ functions are
-- not provided.
module Control.Concurrent.Classy.STM.TVar
  ( -- * @TVar@s
    TVar
  , newTVar
  , newTVarN
  , readTVar
  , readTVarConc
  , writeTVar
  , modifyTVar
  , modifyTVar'
  , stateTVar
  , swapTVar
  , registerDelay
  ) where

import           Control.Monad.Conc.Class
import           Control.Monad.STM.Class
import           Data.Functor             (void)

-- * @TVar@s

-- | Mutate the contents of a 'TVar'. This is non-strict.
--
-- @since 1.0.0.0
modifyTVar :: MonadSTM stm => TVar stm a -> (a -> a) -> stm ()
modifyTVar ctvar f = do
  a <- readTVar ctvar
  writeTVar ctvar $ f a

-- | Mutate the contents of a 'TVar' strictly.
--
-- @since 1.0.0.0
modifyTVar' :: MonadSTM stm => TVar stm a -> (a -> a) -> stm ()
modifyTVar' ctvar f = do
  a <- readTVar ctvar
  writeTVar ctvar $! f a

-- | Like 'modifyTVar'' but the function is a simple state transition that can
-- return a side value which is passed on as the result of the STM.
--
-- @since 1.6.1.0
stateTVar :: MonadSTM stm => TVar stm s -> (s -> (a, s)) -> stm a
stateTVar var f = do
   s <- readTVar var
   let (a, s') = f s -- since we destructure this, we are strict in f
   writeTVar var s'
   pure a

-- | Swap the contents of a 'TVar', returning the old value.
--
-- @since 1.0.0.0
swapTVar :: MonadSTM stm => TVar stm a -> a -> stm a
swapTVar ctvar a = do
  old <- readTVar ctvar
  writeTVar ctvar a
  pure old

-- | Set the value of returned 'TVar' to @True@ after a given number
-- of microseconds. The caveats associated with 'threadDelay' also
-- apply.
--
-- @since 1.0.0.0
registerDelay :: MonadConc m => Int -> m (TVar (STM m) Bool)
registerDelay delay = do
  var <- atomically (newTVar False)
  void . fork $ do
    threadDelay delay
    atomically (writeTVar var True)
  pure var

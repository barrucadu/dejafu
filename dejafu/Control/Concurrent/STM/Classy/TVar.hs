-- | Transactional variables, for use with 'MonadSTM'.
module Control.Concurrent.STM.Classy.TVar
  ( -- * @TVar@s
    TVar
  , newTVar
  , newTVarN
  , readTVar
  , writeTVar
  , modifyTVar
  , modifyTVar'
  , swapTVar
  ) where

import Control.Monad.STM.Class

-- * @TVar@s

-- | Mutate the contents of a 'TVar'. This is non-strict.
modifyTVar :: MonadSTM m => TVar m a -> (a -> a) -> m ()
modifyTVar ctvar f = do
  a <- readTVar ctvar
  writeTVar ctvar $ f a

-- | Mutate the contents of a 'TVar' strictly.
modifyTVar' :: MonadSTM m => TVar m a -> (a -> a) -> m ()
modifyTVar' ctvar f = do
  a <- readTVar ctvar
  writeTVar ctvar $! f a

-- | Swap the contents of a 'TVar', returning the old value.
swapTVar :: MonadSTM m => TVar m a -> a -> m a
swapTVar ctvar a = do
  old <- readTVar ctvar
  writeTVar ctvar a
  return old

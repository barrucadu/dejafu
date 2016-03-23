-- | Transactional variables, for use with 'MonadSTM'.
module Control.Concurrent.STM.Classy.TVar
  ( -- * @TVar@s
    TVar
  , newTVar
  , newTVarN
  , readTVar
  , readTVarConc
  , writeTVar
  , modifyTVar
  , modifyTVar'
  , swapTVar
  , registerDelay
  ) where

import Control.Monad.STM.Class
import Control.Monad.Conc.Class
import Data.Functor (void)

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

-- | Set the value of returned 'TVar' to @True@ after a given number
-- of microseconds. The caveats associated with 'threadDelay' also
-- apply.
registerDelay :: MonadConc m => Int -> m (TVar (STM m) Bool)
registerDelay delay = do
  var <- atomically (newTVar False)
  void . fork $ do
    threadDelay delay
    atomically (writeTVar var True)
  pure var

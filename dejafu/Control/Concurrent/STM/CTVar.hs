-- | Transactional variables, for use with 'MonadSTM'.
module Control.Concurrent.STM.CTVar
  ( -- * @CTVar@s
    CTVar
  , newCTVar
  , newCTVarN
  , readCTVar
  , writeCTVar
  , modifyCTVar
  , modifyCTVar'
  , swapCTVar
  ) where

import Control.Monad.STM.Class

-- * @CTVar@s

-- | Mutate the contents of a 'CTVar'. This is non-strict.
modifyCTVar :: MonadSTM m => CTVar m a -> (a -> a) -> m ()
modifyCTVar ctvar f = do
  a <- readCTVar ctvar
  writeCTVar ctvar $ f a

-- | Mutate the contents of a 'CTVar' strictly.
modifyCTVar' :: MonadSTM m => CTVar m a -> (a -> a) -> m ()
modifyCTVar' ctvar f = do
  a <- readCTVar ctvar
  writeCTVar ctvar $! f a

-- | Swap the contents of a 'CTVar', returning the old value.
swapCTVar :: MonadSTM m => CTVar m a -> a -> m a
swapCTVar ctvar a = do
  old <- readCTVar ctvar
  writeCTVar ctvar a
  return old

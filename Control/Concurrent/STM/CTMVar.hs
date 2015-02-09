-- | Transactional @CVar@s, for use with 'MonadSTM'.
module Control.Concurrent.STM.CTMVar
  ( -- * @CTMVar@s
    CTMVar
  , newCTMVar
  , newEmptyCTMVar
  , takeCTMVar
  , putCTMVar
  , readCTMVar
  , tryTakeCTMVar
  , tryPutCTMVar
  , tryReadCTMVar
  , isEmptyCTMVar
  , swapCTMVar
  ) where

import Control.Monad (liftM, when, unless)
import Control.Monad.STM.Class
import Data.Maybe (isJust, isNothing)

-- | A @CTMVar@ is like an @MVar@ or a @CVar@, but using transactional
-- memory. As transactions are atomic, this makes dealing with
-- multiple @CTMVar@s easier than wrangling multiple @CVar@s.
newtype CTMVar m a = CTMVar (CTVar m (Maybe a))

-- | Create a 'CTMVar' containing the given value.
newCTMVar :: MonadSTM m => a -> m (CTMVar m a)
newCTMVar a = do
  ctvar <- newCTVar $ Just a
  return $ CTMVar ctvar

-- | Create a new empty 'CTMVar'.
newEmptyCTMVar :: MonadSTM m => m (CTMVar m a)
newEmptyCTMVar = do
  ctvar <- newCTVar Nothing
  return $ CTMVar ctvar

-- | Take the contents of a 'CTMVar', or 'retry' if it is empty.
takeCTMVar :: MonadSTM m => CTMVar m a -> m a
takeCTMVar ctmvar = do
  taken <- tryTakeCTMVar ctmvar
  maybe retry return taken

-- | Write to a 'CTMVar', or 'retry' if it is full.
putCTMVar :: MonadSTM m => CTMVar m a -> a -> m ()
putCTMVar ctmvar a = do
  putted <- tryPutCTMVar ctmvar a
  unless putted retry

-- | Read from a 'CTMVar' without emptying, or 'retry' if it is empty.
readCTMVar :: MonadSTM m => CTMVar m a -> m a
readCTMVar ctmvar = do
  readed <- tryReadCTMVar ctmvar
  maybe retry return readed

-- | Try to take the contents of a 'CTMVar', returning 'Nothing' if it
-- is empty.
tryTakeCTMVar :: MonadSTM m => CTMVar m a -> m (Maybe a)
tryTakeCTMVar (CTMVar ctvar) = do
  val <- readCTVar ctvar
  when (isJust val) $ writeCTVar ctvar Nothing
  return val

-- | Try to write to a 'CTMVar', returning 'False' if it is full.
tryPutCTMVar :: MonadSTM m => CTMVar m a -> a -> m Bool
tryPutCTMVar (CTMVar ctvar) a = do
  val <- readCTVar ctvar
  when (isNothing val) $ writeCTVar ctvar (Just a)
  return $ isNothing val

-- | Try to read from a 'CTMVar' without emptying, returning 'Nothing'
-- if it is empty.
tryReadCTMVar :: MonadSTM m => CTMVar m a -> m (Maybe a)
tryReadCTMVar (CTMVar ctvar) = readCTVar ctvar

-- | Check if a 'CTMVar' is empty or not.
isEmptyCTMVar :: MonadSTM m => CTMVar m a -> m Bool
isEmptyCTMVar ctmvar = isNothing `liftM` tryReadCTMVar ctmvar

-- | Swap the contents of a 'CTMVar' returning the old contents, or
-- 'retry' if it is empty.
swapCTMVar :: MonadSTM m => CTMVar m a -> a -> m a
swapCTMVar ctmvar a = do
  val <- takeCTMVar ctmvar
  putCTMVar ctmvar a
  return val

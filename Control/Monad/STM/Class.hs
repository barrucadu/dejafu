{-# LANGUAGE TypeFamilies #-}

-- | This module provides an abstraction over 'STM', which can be used
-- with 'MonadConc'.
module Control.Monad.STM.Class where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Exception (Exception)
import Control.Monad (unless)

import qualified Control.Monad.STM as S

-- | @MonadSTM@ is an abstraction over 'STM', in the same spirit as
-- 'MonadConc' is an abstraction over 'IO's concurrency.
--
-- This class does not provide any way to run transactions, rather
-- each 'MonadConc' has an associated 'MonadSTM' from which it can
-- atomically run a transaction.
--
-- A minimal implementation consists of 'retry', 'orElse', 'newCTVar',
-- 'readCTVar', and 'writeCTVar'.
class Monad m => MonadSTM m where
  -- | The mutable reference type. These behave like 'TVar's, in that
  -- they always contain a value and updates are non-blocking and
  -- synchronised.
  type CTVar m :: * -> *

  -- | Retry execution of this transaction because it has seen values
  -- in @CTVar@s that it shouldn't have. This may result in the thread
  -- running the transaction being blocked until any @CTVar@s
  -- referenced in it have been mutated.
  retry :: m a

  -- | Run the first transaction and, if it @retry@s, run the second
  -- instead. If the monad is an instance of
  -- 'Alternative'/'MonadPlus', 'orElse' should be the '(<|>)'/'mplus'
  -- function.
  orElse :: m a -> m a -> m a

  -- | Check whether a condition is true and, if not, call @retry@.
  --
  -- > check b = unless b retry
  check :: Bool -> m ()
  check b = unless b retry

  -- | Create a new @CTVar@ containing the given value.
  newCTVar :: a -> m (CTVar m a)

  -- | Return the current value stored in a @CTVar@.
  readCTVar :: CTVar m a -> m a

  -- | Write the supplied value into the @CTVar@.
  writeCTVar :: CTVar m a -> a -> m ()

  -- | Throw an exception. This aborts the transaction and propagates
  -- the exception.
  throwSTM :: Exception e => e -> m a

  -- | Handling exceptions from 'throwSTM'.
  catchSTM :: Exception e => m a -> (e -> m a) -> m a

instance MonadSTM STM where
  type CTVar STM = TVar

  retry      = S.retry
  orElse     = S.orElse
  newCTVar   = newTVar
  readCTVar  = readTVar
  writeCTVar = writeTVar
  throwSTM   = S.throwSTM
  catchSTM   = S.catchSTM

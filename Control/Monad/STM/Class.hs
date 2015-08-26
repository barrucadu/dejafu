{-# LANGUAGE TypeFamilies #-}

-- | This module provides an abstraction over 'STM', which can be used
-- with 'MonadConc'.
module Control.Monad.STM.Class where

import Control.Applicative (Applicative)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM, catch)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans (lift)
import Data.Monoid (Monoid)

import qualified Control.Monad.RWS.Lazy as RL
import qualified Control.Monad.RWS.Strict as RS
import qualified Control.Monad.STM as S
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS

-- | @MonadSTM@ is an abstraction over 'STM'.
--
-- This class does not provide any way to run transactions, rather
-- each 'MonadConc' has an associated @MonadSTM@ from which it can
-- atomically run a transaction.
--
-- A minimal implementation consists of 'retry', 'orElse', 'newCTVar',
-- 'readCTVar', and 'writeCTVar'.
class (Applicative m, Monad m, MonadCatch m, MonadThrow m) => MonadSTM m where
  -- | The mutable reference type. These behave like 'TVar's, in that
  -- they always contain a value and updates are non-blocking and
  -- synchronised.
  type CTVar m :: * -> *

  -- | Retry execution of this transaction because it has seen values
  -- in @CTVar@s that it shouldn't have. This will result in the
  -- thread running the transaction being blocked until any @CTVar@s
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
  --
  -- > throwSTM = Control.Monad.Catch.throwM
  throwSTM :: Exception e => e -> m a
  throwSTM = throwM

  -- | Handling exceptions from 'throwSTM'.
  --
  -- > catchSTM = Control.Monad.Catch.catch
  catchSTM :: Exception e => m a -> (e -> m a) -> m a
  catchSTM = Control.Monad.Catch.catch

instance MonadSTM STM where
  type CTVar STM = TVar

  retry      = S.retry
  orElse     = S.orElse
  newCTVar   = newTVar
  readCTVar  = readTVar
  writeCTVar = writeTVar

-------------------------------------------------------------------------------
-- Transformer instances

instance MonadSTM m => MonadSTM (ReaderT r m) where
  type CTVar (ReaderT r m) = CTVar m

  retry        = lift retry
  orElse ma mb = ReaderT $ \r -> orElse (runReaderT ma r) (runReaderT mb r)
  check        = lift . check
  newCTVar     = lift . newCTVar
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

instance (MonadSTM m, Monoid w) => MonadSTM (WL.WriterT w m) where
  type CTVar (WL.WriterT w m) = CTVar m

  retry        = lift retry
  orElse ma mb = WL.WriterT $ orElse (WL.runWriterT ma) (WL.runWriterT mb)
  check        = lift . check
  newCTVar     = lift . newCTVar
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

instance (MonadSTM m, Monoid w) => MonadSTM (WS.WriterT w m) where
  type CTVar (WS.WriterT w m) = CTVar m

  retry        = lift retry
  orElse ma mb = WS.WriterT $ orElse (WS.runWriterT ma) (WS.runWriterT mb)
  check        = lift . check
  newCTVar     = lift . newCTVar
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

instance MonadSTM m => MonadSTM (SL.StateT s m) where
  type CTVar (SL.StateT s m) = CTVar m

  retry        = lift retry
  orElse ma mb = SL.StateT $ \s -> orElse (SL.runStateT ma s) (SL.runStateT mb s)
  check        = lift . check
  newCTVar     = lift . newCTVar
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

instance MonadSTM m => MonadSTM (SS.StateT s m) where
  type CTVar (SS.StateT s m) = CTVar m

  retry        = lift retry
  orElse ma mb = SS.StateT $ \s -> orElse (SS.runStateT ma s) (SS.runStateT mb s)
  check        = lift . check
  newCTVar     = lift . newCTVar
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

instance (MonadSTM m, Monoid w) => MonadSTM (RL.RWST r w s m) where
  type CTVar (RL.RWST r w s m) = CTVar m

  retry        = lift retry
  orElse ma mb = RL.RWST $ \r s -> orElse (RL.runRWST ma r s) (RL.runRWST mb r s)
  check        = lift . check
  newCTVar     = lift . newCTVar
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

instance (MonadSTM m, Monoid w) => MonadSTM (RS.RWST r w s m) where
  type CTVar (RS.RWST r w s m) = CTVar m

  retry        = lift retry
  orElse ma mb = RS.RWST $ \r s -> orElse (RS.runRWST ma r s) (RS.runRWST mb r s)
  check        = lift . check
  newCTVar     = lift . newCTVar
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

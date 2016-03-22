{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | This module provides an abstraction over 'STM', which can be used
-- with 'MonadConc'.
module Control.Monad.STM.Class
  ( MonadSTM(..)
  , check
  , throwSTM
  , catchSTM

  -- * Utilities for instance writers
  , makeTransSTM
  , liftedOrElse
  ) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM, catch)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadTransControl, StT, liftWith)
import Language.Haskell.TH (DecsQ, Info(VarI), Name, Type(..), reify, varE)

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
  {-# MINIMAL
        retry
      , orElse
      , (newCTVar | newCTVarN)
      , readCTVar
      , writeCTVar
    #-}

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

  -- | Create a new @CTVar@ containing the given value.
  --
  -- > newCTVar = newCTVarN ""
  newCTVar :: a -> m (CTVar m a)
  newCTVar = newCTVarN ""

  -- | Create a new @CTVar@ containing the given value, but it is
  -- given a name which may be used to present more useful debugging
  -- information.
  --
  -- If an empty name is given, a counter starting from 0 is used. If
  -- names conflict, successive @CTVar@s with the same name are given
  -- a numeric suffix, counting up from 1.
  --
  -- > newCTVarN _ = newCTVar
  newCTVarN :: String -> a -> m (CTVar m a)
  newCTVarN _ = newCTVar

  -- | Return the current value stored in a @CTVar@.
  readCTVar :: CTVar m a -> m a

  -- | Write the supplied value into the @CTVar@.
  writeCTVar :: CTVar m a -> a -> m ()

-- | Check whether a condition is true and, if not, call @retry@.
check :: MonadSTM m => Bool -> m ()
check b = unless b retry

-- | Throw an exception. This aborts the transaction and propagates
-- the exception.
throwSTM :: (MonadSTM m, Exception e) => e -> m a
throwSTM = throwM

-- | Handling exceptions from 'throwSTM'.
catchSTM :: (MonadSTM m, Exception e) => m a -> (e -> m a) -> m a
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
  orElse       = liftedOrElse id
  newCTVar     = lift . newCTVar
  newCTVarN n  = lift . newCTVarN n
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

instance (MonadSTM m, Monoid w) => MonadSTM (WL.WriterT w m) where
  type CTVar (WL.WriterT w m) = CTVar m

  retry        = lift retry
  orElse       = liftedOrElse fst
  newCTVar     = lift . newCTVar
  newCTVarN n  = lift . newCTVarN n
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

instance (MonadSTM m, Monoid w) => MonadSTM (WS.WriterT w m) where
  type CTVar (WS.WriterT w m) = CTVar m

  retry        = lift retry
  orElse       = liftedOrElse fst
  newCTVar     = lift . newCTVar
  newCTVarN n  = lift . newCTVarN n
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

instance MonadSTM m => MonadSTM (SL.StateT s m) where
  type CTVar (SL.StateT s m) = CTVar m

  retry        = lift retry
  orElse       = liftedOrElse fst
  newCTVar     = lift . newCTVar
  newCTVarN n  = lift . newCTVarN n
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

instance MonadSTM m => MonadSTM (SS.StateT s m) where
  type CTVar (SS.StateT s m) = CTVar m

  retry        = lift retry
  orElse       = liftedOrElse fst
  newCTVar     = lift . newCTVar
  newCTVarN n  = lift . newCTVarN n
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

instance (MonadSTM m, Monoid w) => MonadSTM (RL.RWST r w s m) where
  type CTVar (RL.RWST r w s m) = CTVar m

  retry        = lift retry
  orElse       = liftedOrElse (\(a,_,_) -> a)
  newCTVar     = lift . newCTVar
  newCTVarN n  = lift . newCTVarN n
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

instance (MonadSTM m, Monoid w) => MonadSTM (RS.RWST r w s m) where
  type CTVar (RS.RWST r w s m) = CTVar m

  retry        = lift retry
  orElse       = liftedOrElse (\(a,_,_) -> a)
  newCTVar     = lift . newCTVar
  newCTVarN n  = lift . newCTVarN n
  readCTVar    = lift . readCTVar
  writeCTVar v = lift . writeCTVar v

-------------------------------------------------------------------------------

-- | Make an instance @MonadSTM m => MonadSTM (t m)@ for a given
-- transformer, @t@. The parameter should be the name of a function
-- @:: forall a. StT t a -> a@.
makeTransSTM :: Name -> DecsQ
makeTransSTM unstN = do
  unstI <- reify unstN
  case unstI of
    VarI _ (ForallT _ _ (AppT (AppT ArrowT (AppT (AppT (ConT _) t) _)) _)) _ _ ->
      [d|
        instance (MonadSTM m, MonadTransControl $(pure t)) => MonadSTM ($(pure t) m) where
          type CTVar ($(pure t) m) = CTVar m

          retry        = lift retry
          orElse       = liftedOrElse $(varE unstN)
          newCTVar     = lift . newCTVar
          newCTVarN n  = lift . newCTVarN n
          readCTVar    = lift . readCTVar
          writeCTVar v = lift . writeCTVar v
      |]
    _ -> fail "Expected a value of type (forall a -> StT t a -> a)"

-- | Given a function to remove the transformer-specific state, lift
-- an @orElse@ invocation.
liftedOrElse :: (MonadTransControl t, MonadSTM m)
  => (forall x. StT t x -> x)
  -> t m a -> t m a -> t m a
liftedOrElse unst ma mb = liftWith $ \run ->
  let ma' = unst <$> run ma
      mb' = unst <$> run mb
  in ma' `orElse` mb'

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

import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM, catch)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadTransControl, StT, liftWith)
import Language.Haskell.TH (DecsQ, Info(VarI), Name, Type(..), reify, varE)

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.RWS.Lazy as RL
import qualified Control.Monad.RWS.Strict as RS
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS

-- | @MonadSTM@ is an abstraction over 'STM'.
--
-- This class does not provide any way to run transactions, rather
-- each 'MonadConc' has an associated @MonadSTM@ from which it can
-- atomically run a transaction.
class (Applicative m, Monad m, MonadCatch m, MonadThrow m) => MonadSTM m where
  {-# MINIMAL
        retry
      , orElse
      , (newTVar | newTVarN)
      , readTVar
      , writeTVar
    #-}

  -- | The mutable reference type. These behave like 'TVar's, in that
  -- they always contain a value and updates are non-blocking and
  -- synchronised.
  type TVar m :: * -> *

  -- | Retry execution of this transaction because it has seen values
  -- in @TVar@s that it shouldn't have. This will result in the
  -- thread running the transaction being blocked until any @TVar@s
  -- referenced in it have been mutated.
  retry :: m a

  -- | Run the first transaction and, if it @retry@s, run the second
  -- instead. If the monad is an instance of
  -- 'Alternative'/'MonadPlus', 'orElse' should be the '(<|>)'/'mplus'
  -- function.
  orElse :: m a -> m a -> m a

  -- | Create a new @TVar@ containing the given value.
  --
  -- > newTVar = newTVarN ""
  newTVar :: a -> m (TVar m a)
  newTVar = newTVarN ""

  -- | Create a new @TVar@ containing the given value, but it is
  -- given a name which may be used to present more useful debugging
  -- information.
  --
  -- If an empty name is given, a counter starting from 0 is used. If
  -- names conflict, successive @TVar@s with the same name are given
  -- a numeric suffix, counting up from 1.
  --
  -- > newTVarN _ = newTVar
  newTVarN :: String -> a -> m (TVar m a)
  newTVarN _ = newTVar

  -- | Return the current value stored in a @TVar@.
  readTVar :: TVar m a -> m a

  -- | Write the supplied value into the @TVar@.
  writeTVar :: TVar m a -> a -> m ()

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

instance MonadSTM STM.STM where
  type TVar STM.STM = STM.TVar

  retry     = STM.retry
  orElse    = STM.orElse
  newTVar   = STM.newTVar
  readTVar  = STM.readTVar
  writeTVar = STM.writeTVar

-------------------------------------------------------------------------------
-- Transformer instances

instance MonadSTM m => MonadSTM (ReaderT r m) where
  type TVar (ReaderT r m) = TVar m

  retry       = lift retry
  orElse      = liftedOrElse id
  newTVar     = lift . newTVar
  newTVarN n  = lift . newTVarN n
  readTVar    = lift . readTVar
  writeTVar v = lift . writeTVar v

instance (MonadSTM m, Monoid w) => MonadSTM (WL.WriterT w m) where
  type TVar (WL.WriterT w m) = TVar m

  retry       = lift retry
  orElse      = liftedOrElse fst
  newTVar     = lift . newTVar
  newTVarN n  = lift . newTVarN n
  readTVar    = lift . readTVar
  writeTVar v = lift . writeTVar v

instance (MonadSTM m, Monoid w) => MonadSTM (WS.WriterT w m) where
  type TVar (WS.WriterT w m) = TVar m

  retry       = lift retry
  orElse      = liftedOrElse fst
  newTVar     = lift . newTVar
  newTVarN n  = lift . newTVarN n
  readTVar    = lift . readTVar
  writeTVar v = lift . writeTVar v

instance MonadSTM m => MonadSTM (SL.StateT s m) where
  type TVar (SL.StateT s m) = TVar m

  retry       = lift retry
  orElse      = liftedOrElse fst
  newTVar     = lift . newTVar
  newTVarN n  = lift . newTVarN n
  readTVar    = lift . readTVar
  writeTVar v = lift . writeTVar v

instance MonadSTM m => MonadSTM (SS.StateT s m) where
  type TVar (SS.StateT s m) = TVar m

  retry       = lift retry
  orElse      = liftedOrElse fst
  newTVar     = lift . newTVar
  newTVarN n  = lift . newTVarN n
  readTVar    = lift . readTVar
  writeTVar v = lift . writeTVar v

instance (MonadSTM m, Monoid w) => MonadSTM (RL.RWST r w s m) where
  type TVar (RL.RWST r w s m) = TVar m

  retry       = lift retry
  orElse      = liftedOrElse (\(a,_,_) -> a)
  newTVar     = lift . newTVar
  newTVarN n  = lift . newTVarN n
  readTVar    = lift . readTVar
  writeTVar v = lift . writeTVar v

instance (MonadSTM m, Monoid w) => MonadSTM (RS.RWST r w s m) where
  type TVar (RS.RWST r w s m) = TVar m

  retry       = lift retry
  orElse      = liftedOrElse (\(a,_,_) -> a)
  newTVar     = lift . newTVar
  newTVarN n  = lift . newTVarN n
  readTVar    = lift . readTVar
  writeTVar v = lift . writeTVar v

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
          type TVar ($(pure t) m) = TVar m

          retry       = lift retry
          orElse      = liftedOrElse $(varE unstN)
          newTVar     = lift . newTVar
          newTVarN n  = lift . newTVarN n
          readTVar    = lift . readTVar
          writeTVar v = lift . writeTVar v
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

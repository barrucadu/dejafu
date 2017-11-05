{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Control.Monad.STM.Class
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, RankNTypes, TemplateHaskell, TypeFamilies
--
-- This module provides an abstraction over 'STM', which can be used
-- with 'MonadConc'.
--
-- This module only defines the 'STM' class; you probably want to
-- import "Control.Concurrent.Classy.STM" (which exports
-- "Control.Monad.STM.Class").
--
-- __Deviations:__ An instance of @MonadSTM@ is not required to be a
-- @MonadFix@, unlike @STM@. The @always@ and @alwaysSucceeds@
-- functions are not provided; if you need these file an issue and
-- I'll look into it.
module Control.Monad.STM.Class
  ( MonadSTM(..)
  , retry
  , check
  , orElse
  , throwSTM
  , catchSTM

    -- * Utilities for type shenanigans
  , IsSTM
  , toIsSTM
  , fromIsSTM
) where

import           Control.Applicative          (Alternative(..))
import           Control.Exception            (Exception)
import           Control.Monad                (MonadPlus(..), unless)
import           Control.Monad.Reader         (ReaderT)
import           Control.Monad.Trans          (lift)
import           Control.Monad.Trans.Identity (IdentityT)

import qualified Control.Concurrent.STM       as STM
import qualified Control.Monad.Catch          as Ca
import qualified Control.Monad.RWS.Lazy       as RL
import qualified Control.Monad.RWS.Strict     as RS
import qualified Control.Monad.State.Lazy     as SL
import qualified Control.Monad.State.Strict   as SS
import qualified Control.Monad.Writer.Lazy    as WL
import qualified Control.Monad.Writer.Strict  as WS

-- | @MonadSTM@ is an abstraction over 'STM'.
--
-- This class does not provide any way to run transactions, rather
-- each 'MonadConc' has an associated @MonadSTM@ from which it can
-- atomically run a transaction.
--
-- @since 1.2.0.0
class (Ca.MonadCatch stm, MonadPlus stm) => MonadSTM stm where
  {-# MINIMAL
        (newTVar | newTVarN)
      , readTVar
      , writeTVar
    #-}

  -- | The mutable reference type. These behave like 'TVar's, in that
  -- they always contain a value and updates are non-blocking and
  -- synchronised.
  --
  -- @since 1.0.0.0
  type TVar stm :: * -> *

  -- | Create a new @TVar@ containing the given value.
  --
  -- > newTVar = newTVarN ""
  --
  -- @since 1.0.0.0
  newTVar :: a -> stm (TVar stm a)
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
  --
  -- @since 1.0.0.0
  newTVarN :: String -> a -> stm (TVar stm a)
  newTVarN _ = newTVar

  -- | Return the current value stored in a @TVar@.
  --
  -- @since 1.0.0.0
  readTVar :: TVar stm a -> stm a

  -- | Write the supplied value into the @TVar@.
  --
  -- @since 1.0.0.0
  writeTVar :: TVar stm a -> a -> stm ()

-- | Retry execution of this transaction because it has seen values in
-- @TVar@s that it shouldn't have. This will result in the thread
-- running the transaction being blocked until any @TVar@s referenced
-- in it have been mutated.
--
-- This is just 'mzero'.
--
-- @since 1.2.0.0
retry :: MonadSTM stm => stm a
retry = mzero

-- | Check whether a condition is true and, if not, call @retry@.
--
-- @since 1.0.0.0
check :: MonadSTM stm => Bool -> stm ()
check b = unless b retry

-- | Run the first transaction and, if it @retry@s, run the second
-- instead.
--
-- This is just 'mplus'.
--
-- @since 1.2.0.0
orElse :: MonadSTM stm => stm a -> stm a -> stm a
orElse = mplus

-- | Throw an exception. This aborts the transaction and propagates
-- the exception.
--
-- @since 1.0.0.0
throwSTM :: (MonadSTM stm, Exception e) => e -> stm a
throwSTM = Ca.throwM

-- | Handling exceptions from 'throwSTM'.
--
-- @since 1.0.0.0
catchSTM :: (MonadSTM stm, Exception e) => stm a -> (e -> stm a) -> stm a
catchSTM = Ca.catch

-- | @since 1.0.0.0
instance MonadSTM STM.STM where
  type TVar STM.STM = STM.TVar

  newTVar   = STM.newTVar
  readTVar  = STM.readTVar
  writeTVar = STM.writeTVar

-------------------------------------------------------------------------------
-- Type shenanigans

-- | A value of type @IsSTM m a@ can only be constructed if @m@ has a
-- @MonadSTM@ instance.
--
-- @since 1.2.2.0
newtype IsSTM m a = IsSTM { unIsSTM :: m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Ca.MonadThrow, Ca.MonadCatch)

-- | Wrap an @m a@ value inside an @IsSTM@ if @m@ has a @MonadSTM@
-- instance.
--
-- @since 1.2.2.0
toIsSTM :: MonadSTM m => m a -> IsSTM m a
toIsSTM = IsSTM

-- | Unwrap an @IsSTM@ value.
--
-- @since 1.2.2.0
fromIsSTM :: MonadSTM m => IsSTM m a -> m a
fromIsSTM = unIsSTM

instance MonadSTM m => MonadSTM (IsSTM m) where
  type TVar (IsSTM m) = TVar m

  newTVar     = toIsSTM . newTVar
  newTVarN n  = toIsSTM . newTVarN n
  readTVar    = toIsSTM . readTVar
  writeTVar v = toIsSTM . writeTVar v

-------------------------------------------------------------------------------
-- Transformer instances

#define INSTANCE(T,C,F)                                  \
instance C => MonadSTM (T stm) where { \
  type TVar (T stm) = TVar stm      ; \
                                      \
  newTVar     = lift . newTVar      ; \
  newTVarN n  = lift . newTVarN n   ; \
  readTVar    = lift . readTVar     ; \
  writeTVar v = lift . writeTVar v  }

-- | @since 1.0.0.0
INSTANCE(ReaderT r, MonadSTM stm, id)

-- | @since 1.0.0.0
INSTANCE(IdentityT, MonadSTM stm, id)

-- | @since 1.0.0.0
INSTANCE(WL.WriterT w, (MonadSTM stm, Monoid w), fst)

-- | @since 1.0.0.0
INSTANCE(WS.WriterT w, (MonadSTM stm, Monoid w), fst)

-- | @since 1.0.0.0
INSTANCE(SL.StateT s, MonadSTM stm, fst)

-- | @since 1.0.0.0
INSTANCE(SS.StateT s, MonadSTM stm, fst)

-- | @since 1.0.0.0
INSTANCE(RL.RWST r w s, (MonadSTM stm, Monoid w), (\(a,_,_) -> a))

-- | @since 1.0.0.0
INSTANCE(RS.RWST r w s, (MonadSTM stm, Monoid w), (\(a,_,_) -> a))

#undef INSTANCE

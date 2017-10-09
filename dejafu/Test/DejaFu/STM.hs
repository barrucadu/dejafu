{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Test.DejaFu.STM
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, GeneralizedNewtypeDeriving, TypeFamilies
--
-- A 'MonadSTM' implementation, which can be run on top of 'IO' or
-- 'ST'.
module Test.DejaFu.STM
  ( -- * The @STMLike@ Monad
    STMLike
  , STMST
  , STMIO

  -- * Executing Transactions
  , Result(..)
  , TTrace
  , TAction(..)
  , TVarId
  , runTransaction
  ) where

import           Control.Applicative      (Alternative(..))
import           Control.Monad            (MonadPlus(..), unless)
import           Control.Monad.Catch      (MonadCatch(..), MonadThrow(..))
import           Control.Monad.Ref        (MonadRef)
import           Control.Monad.ST         (ST)
import           Data.IORef               (IORef)
import           Data.STRef               (STRef)

import qualified Control.Monad.STM.Class  as C
import           Test.DejaFu.Common
import           Test.DejaFu.STM.Internal

#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail       as Fail
#endif

-- | @since 0.3.0.0
newtype STMLike n r a = S { runSTM :: M n r a } deriving (Functor, Applicative, Monad)

#if MIN_VERSION_base(4,9,0)
-- | @since 0.9.1.0
instance Fail.MonadFail (STMLike r n) where
  fail = S . fail
#endif

-- | Create a new STM continuation.
toSTM :: ((a -> STMAction n r) -> STMAction n r) -> STMLike n r a
toSTM = S . cont

-- | A 'MonadSTM' implementation using @ST@, it encapsulates a single
-- atomic transaction. The environment, that is, the collection of
-- defined 'TVar's is implicit, there is no list of them, they exist
-- purely as references. This makes the types simpler, but means you
-- can't really get an aggregate of them (if you ever wanted to for
-- some reason).
--
-- @since 0.3.0.0
type STMST t = STMLike (ST t) (STRef t)

-- | A 'MonadSTM' implementation using @ST@, it encapsulates a single
-- atomic transaction. The environment, that is, the collection of
-- defined 'TVar's is implicit, there is no list of them, they exist
-- purely as references. This makes the types simpler, but means you
-- can't really get an aggregate of them (if you ever wanted to for
-- some reason).
--
-- @since 0.3.0.0
type STMIO = STMLike IO IORef

instance MonadThrow (STMLike n r) where
  throwM = toSTM . const . SThrow

instance MonadCatch (STMLike n r) where
  catch (S stm) handler = toSTM (SCatch (runSTM . handler) stm)

-- | @since 0.7.2.0
instance Alternative (STMLike n r) where
  S a <|> S b = toSTM (SOrElse a b)
  empty = toSTM (const SRetry)

-- | @since 0.7.2.0
instance MonadPlus (STMLike n r)

instance C.MonadSTM (STMLike n r) where
  type TVar (STMLike n r) = TVar r

#if MIN_VERSION_concurrency(1,2,0)
  -- retry and orElse are top-level definitions in
  -- Control.Monad.STM.Class in 1.2 and up
#else
  retry = empty
  orElse = (<|>)
#endif

  newTVarN n = toSTM . SNew n

  readTVar = toSTM . SRead

  writeTVar tvar a = toSTM (\c -> SWrite tvar a (c ()))

-- | Run a transaction, returning the result and new initial
-- 'TVarId'. If the transaction ended by calling 'retry', any 'TVar'
-- modifications are undone.
--
-- @since 0.4.0.0
runTransaction :: MonadRef r n
               => STMLike n r a -> IdSource -> n (Result a, IdSource, TTrace)
runTransaction ma tvid = do
  (res, undo, tvid', trace) <- doTransaction (runSTM ma) tvid

  unless (isSTMSuccess res) undo

  pure (res, tvid', trace)

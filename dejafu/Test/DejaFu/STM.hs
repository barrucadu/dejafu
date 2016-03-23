{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | A 'MonadSTM' implementation, which can be run on top of 'IO' or
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
  , runTransactionST
  , runTransactionIO
  ) where

import Control.Monad (liftM)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Cont (cont)
import Control.Monad.ST (ST)
import Data.IORef (IORef)
import Data.STRef (STRef)
import Test.DejaFu.Deterministic.Internal.Common (TVarId, IdSource, TAction(..), TTrace)
import Test.DejaFu.Internal
import Test.DejaFu.STM.Internal

import qualified Control.Monad.STM.Class as C

{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}

newtype STMLike n r a = S { runSTM :: M n r a } deriving (Functor, Applicative, Monad)

-- | Create a new STM continuation.
toSTM :: ((a -> STMAction n r) -> STMAction n r) -> STMLike n r a
toSTM = S . cont

-- | A 'MonadSTM' implementation using @ST@, it encapsulates a single
-- atomic transaction. The environment, that is, the collection of
-- defined 'TVar's is implicit, there is no list of them, they exist
-- purely as references. This makes the types simpler, but means you
-- can't really get an aggregate of them (if you ever wanted to for
-- some reason).
type STMST t = STMLike (ST t) (STRef t)

-- | A 'MonadSTM' implementation using @ST@, it encapsulates a single
-- atomic transaction. The environment, that is, the collection of
-- defined 'TVar's is implicit, there is no list of them, they exist
-- purely as references. This makes the types simpler, but means you
-- can't really get an aggregate of them (if you ever wanted to for
-- some reason).
type STMIO = STMLike IO IORef

instance MonadThrow (STMLike n r) where
  throwM = toSTM . const . SThrow

instance MonadCatch (STMLike n r) where
  catch (S stm) handler = toSTM (SCatch (runSTM . handler) stm)

instance Monad n => C.MonadSTM (STMLike n r) where
  type TVar (STMLike n r) = TVar r

  retry = toSTM (const SRetry)

  orElse (S a) (S b) = toSTM (SOrElse a b)

  newTVarN n = toSTM . SNew n

  readTVar = toSTM . SRead

  writeTVar tvar a = toSTM (\c -> SWrite tvar a (c ()))

-- | Run a transaction in the 'ST' monad, returning the result and new
-- initial 'TVarId'. If the transaction ended by calling 'retry', any
-- 'TVar' modifications are undone.
runTransactionST :: STMST t a -> IdSource -> ST t (Result a, IdSource, TTrace)
runTransactionST = runTransactionM fixedST where
  fixedST = refST $ \mb -> cont (\c -> SLift $ c `liftM` mb)

-- | Run a transaction in the 'IO' monad, returning the result and new
-- initial 'TVarId'. If the transaction ended by calling 'retry', any
-- 'TVar' modifications are undone.
runTransactionIO :: STMIO a -> IdSource -> IO (Result a, IdSource, TTrace)
runTransactionIO = runTransactionM fixedIO where
  fixedIO = refIO $ \mb -> cont (\c -> SLift $ c `liftM` mb)

-- | Run a transaction in an arbitrary monad.
runTransactionM :: Monad n
  => Fixed n r -> STMLike n r a -> IdSource -> n (Result a, IdSource, TTrace)
runTransactionM ref ma tvid = do
  (res, undo, tvid', trace) <- doTransaction ref (runSTM ma) tvid

  case res of
    Success _ _ _ -> return (res, tvid', trace)
    _ -> undo >> return (res, tvid, trace)

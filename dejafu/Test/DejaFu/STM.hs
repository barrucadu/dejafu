{-# LANGUAGE CPP                        #-}
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
  , CTVarId
  , runTransactionST
  , runTransactionIO
  ) where

import Control.Monad (liftM)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Cont (cont)
import Control.Monad.ST (ST)
import Data.IORef (IORef)
import Data.STRef (STRef)
import Test.DejaFu.Internal
import Test.DejaFu.STM.Internal

import qualified Control.Monad.STM.Class as C

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative)
#endif

{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}

newtype STMLike t n r a = S { runSTM :: M t n r a } deriving (Functor, Applicative, Monad)

-- | Create a new STM continuation.
toSTM :: ((a -> STMAction t n r) -> STMAction t n r) -> STMLike t n r a
toSTM = S . cont

-- | A 'MonadSTM' implementation using @ST@, it encapsulates a single
-- atomic transaction. The environment, that is, the collection of
-- defined 'CTVar's is implicit, there is no list of them, they exist
-- purely as references. This makes the types simpler, but means you
-- can't really get an aggregate of them (if you ever wanted to for
-- some reason).
type STMST t = STMLike t (ST t) (STRef t)

-- | A 'MonadSTM' implementation using @ST@, it encapsulates a single
-- atomic transaction. The environment, that is, the collection of
-- defined 'CTVar's is implicit, there is no list of them, they exist
-- purely as references. This makes the types simpler, but means you
-- can't really get an aggregate of them (if you ever wanted to for
-- some reason).
type STMIO t = STMLike t IO IORef

instance MonadThrow (STMLike t n r) where
  throwM e = toSTM (\_ -> SThrow e)

instance MonadCatch (STMLike t n r) where
  catch stm handler = toSTM (SCatch (runSTM . handler) (runSTM stm))

instance Monad n => C.MonadSTM (STMLike t n r) where
  type CTVar (STMLike t n r) = CTVar t r

  retry = toSTM (\_ -> SRetry)

  orElse a b = toSTM (SOrElse (runSTM a) (runSTM b))

  newCTVar a = toSTM (SNew a)

  readCTVar ctvar = toSTM (SRead ctvar)

  writeCTVar ctvar a = toSTM (\c -> SWrite ctvar a (c ()))

-- | Run a transaction in the 'ST' monad, returning the result and new
-- initial 'CTVarId'. If the transaction ended by calling 'retry', any
-- 'CTVar' modifications are undone.
runTransactionST :: STMST t a -> CTVarId -> ST t (Result a, CTVarId)
runTransactionST = runTransactionM fixedST where
  fixedST = refST $ \mb -> cont (\c -> SLift $ c `liftM` mb)

-- | Run a transaction in the 'IO' monad, returning the result and new
-- initial 'CTVarId'. If the transaction ended by calling 'retry', any
-- 'CTVar' modifications are undone.
runTransactionIO :: STMIO t a -> CTVarId -> IO (Result a, CTVarId)
runTransactionIO = runTransactionM fixedIO where
  fixedIO = refIO $ \mb -> cont (\c -> SLift $ c `liftM` mb)

-- | Run a transaction in an arbitrary monad.
runTransactionM :: Monad n
  => Fixed t n r -> STMLike t n r a -> CTVarId -> n (Result a, CTVarId)
runTransactionM ref ma ctvid = do
  (res, undo, ctvid') <- doTransaction ref (runSTM ma) ctvid

  case res of
    Success _ _ _ -> return (res, ctvid')
    _ -> undo >> return (res, ctvid)

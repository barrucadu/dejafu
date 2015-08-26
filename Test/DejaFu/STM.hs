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
  , Result(..)
  , runTransaction
  , runTransactionST
  , runTransactionIO
  , retry
  , orElse
  , check
  , throwSTM
  , catchSTM

  -- * @CTVar@s
  , CTVar
  , CTVarId
  , newCTVar
  , readCTVar
  , writeCTVar
  ) where

import Control.Applicative (Applicative)
import Control.Exception (Exception, SomeException(..))
import Control.Monad (liftM)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Cont (cont)
import Control.Monad.ST (ST, runST)
import Data.IORef (IORef)
import Data.STRef (STRef)
import Test.DejaFu.Internal
import Test.DejaFu.STM.Internal

import qualified Control.Monad.STM.Class as C

{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}

-- | The 'MonadSTM' implementation, it encapsulates a single atomic
-- transaction. The environment, that is, the collection of defined
-- 'CTVar's is implicit, there is no list of them, they exist purely
-- as references. This makes the types simpler, but means you can't
-- really get an aggregate of them (if you ever wanted to for some
-- reason).
newtype STMLike t n r a = S { unS :: M t n r a } deriving (Functor, Applicative, Monad)

-- | A convenience wrapper around 'STMLike' using 'STRef's.
type STMST t a = STMLike t (ST t) (STRef t) a

-- | A convenience wrapper around 'STMLike' using 'IORef's.
type STMIO t a = STMLike t IO IORef a

instance Monad n => MonadThrow (STMLike t n r) where
  throwM = throwSTM

instance Monad n => MonadCatch (STMLike t n r) where
  catch = catchSTM

instance Monad n => C.MonadSTM (STMLike t n r) where
  type CTVar (STMLike t n r) = CTVar t r

  retry      = retry
  orElse     = orElse
  newCTVar   = newCTVar
  readCTVar  = readCTVar
  writeCTVar = writeCTVar

-- | Abort the current transaction, restoring any 'CTVar's written to,
-- and returning the list of 'CTVar's read.
retry :: Monad n => STMLike t n r a
retry = S $ cont $ const ARetry

-- | Run the first transaction and, if it 'retry's, 
orElse :: Monad n => STMLike t n r a -> STMLike t n r a -> STMLike t n r a
orElse a b = S $ cont $ AOrElse (unS a) (unS b)

-- | Check whether a condition is true and, if not, call 'retry'.
check :: Monad n => Bool -> STMLike t n r ()
check = C.check

-- | Throw an exception. This aborts the transaction and propagates
-- the exception.
throwSTM :: Exception e => e -> STMLike t n r a
throwSTM e = S $ cont $ const $ AThrow (SomeException e)

-- | Handling exceptions from 'throwSTM'.
catchSTM :: Exception e => STMLike t n r a -> (e -> STMLike t n r a) -> STMLike t n r a
catchSTM stm handler = S $ cont $ ACatch (unS stm) (unS . handler)

-- | Create a new 'CTVar' containing the given value.
newCTVar :: Monad n => a -> STMLike t n r (CTVar t r a)
newCTVar a = S $ cont lifted where
  lifted c = ANew $ \ref ctvid -> c `liftM` newCTVar' ref ctvid
  newCTVar' ref ctvid = (\r -> V (ctvid, r)) `liftM` newRef ref a

-- | Return the current value stored in a 'CTVar'.
readCTVar :: Monad n => CTVar t r a -> STMLike t n r a
readCTVar ctvar = S $ cont $ ARead ctvar

-- | Write the supplied value into the 'CTVar'.
writeCTVar :: Monad n => CTVar t r a -> a -> STMLike t n r ()
writeCTVar ctvar a = S $ cont $ \c -> AWrite ctvar a $ c ()

-- | Run a transaction in the 'ST' monad, starting from a clean
-- environment, and discarding the environment afterwards. This is
-- suitable for testing individual transactions, but not for composing
-- multiple ones.
runTransaction :: (forall t. STMST t a) -> Result a
runTransaction ma = fst $ runST $ runTransactionST ma 0

-- | Run a transaction in the 'ST' monad, returning the result and new
-- initial 'CTVarId'. If the transaction ended by calling 'retry', any
-- 'CTVar' modifications are undone.
runTransactionST :: STMST t a -> CTVarId -> ST t (Result a, CTVarId)
runTransactionST = runTransactionM fixedST where
  fixedST = refST $ \mb -> cont (\c -> ALift $ c `liftM` mb)

-- | Run a transaction in the 'IO' monad, returning the result and new
-- initial 'CTVarId'. If the transaction ended by calling 'retry', any
-- 'CTVar' modifications are undone.
runTransactionIO :: STMIO t a -> CTVarId -> IO (Result a, CTVarId)
runTransactionIO = runTransactionM fixedIO where
  fixedIO = refIO $ \mb -> cont (\c -> ALift $ c `liftM` mb)

-- | Run a transaction in an arbitrary monad.
runTransactionM :: Monad n
  => Fixed t n r -> STMLike t n r a -> CTVarId -> n (Result a, CTVarId)
runTransactionM ref ma ctvid = do
  (res, undo, ctvid') <- doTransaction ref (unS ma) ctvid

  case res of
    Success _ _ _ -> return (res, ctvid')
    _ -> undo >> return (res, ctvid)

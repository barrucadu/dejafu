{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- | A 'MonadSTM' implementation, which can be run on top of 'IO' or
-- 'ST'.
module Test.DejaFu.STM
  ( -- * The @STMLike@ Monad
    STMLike
  , Result(..)
  , runTransaction
  , runTransactionST
  , runTransactionIO
  , retry
  , orElse
  , check

  -- * @CTVar@s
  , CTVar
  , CTVarId
  , newCTVar
  , readCTVar
  , writeCTVar
  ) where

import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Control.Monad.Cont (Cont, cont, runCont)
import Control.Monad.ST (ST, runST)
import Control.State
import Data.List (nub)
import Data.IORef (IORef)
import Data.STRef (STRef)

import qualified Control.Monad.STM.Class as C

-- | The 'MonadSTM' implementation, it encapsulates a single atomic
-- transaction. The environment, that is, the collection of defined
-- 'CTVar's is implicit, there is no list of them, they exist purely
-- as references. This makes the types simpler, but means you can't
-- really get an aggregate of them (if you ever wanted to for some
-- reason).
newtype STMLike t n r a = S { unS :: Cont (STMAction t n r) a } deriving (Functor, Applicative, Monad)

instance Monad n => C.MonadSTM (STMLike t n r) where
  type CTVar (STMLike t n r) = CTVar t r

  retry      = retry
  orElse     = orElse
  newCTVar   = newCTVar
  readCTVar  = readCTVar
  writeCTVar = writeCTVar

-- | STM transactions are represented as a sequence of primitive
-- actions.
data STMAction t n r =
    forall a. ARead  (CTVar t r a) (a -> STMAction t n r)
  | forall a. AWrite (CTVar t r a) a (STMAction t n r)
  | forall a. AOrElse (STMLike t n r a) (STMLike t n r a) (a -> STMAction t n r)
  | ANew (Ref n r -> CTVarId -> n (STMAction t n r))
  | ALift (n (STMAction t n r))
  | ARetry
  | AStop

type Fixed t n r = Wrapper n r (STMLike t n r)

fixedST :: Fixed t (ST t) (STRef t)
fixedST = Wrapper refST lift where
  lift ma = S $ cont (\c -> ALift $ c `liftM` ma)

fixedIO :: Fixed t IO IORef
fixedIO = Wrapper refIO lift where
  lift ma = S $ cont (\c -> ALift $ c `liftM` ma)

-- | A 'CTVar' is a tuple of a unique ID and the value contained. The
-- ID is so that blocked transactions can be re-run when a 'CTVar'
-- they depend on has changed.
newtype CTVar t r a = V (CTVarId, r a)

-- | The unique ID of a 'CTVar'. Only meaningful within a single
-- concurrent computation.
type CTVarId = Int

-- | Abort the current transaction, restoring any 'CTVar's written to,
-- and returning the list of 'CTVar's read.
retry :: Monad n => STMLike t n r a
retry = S $ cont $ const ARetry

-- | Run the first transaction and, if it 'retry's, 
orElse :: Monad n => STMLike t n r a -> STMLike t n r a -> STMLike t n r a
orElse a b = S $ cont $ \c -> AOrElse a b c

-- | Check whether a condition is true and, if not, call 'retry'.
check :: Monad n => Bool -> STMLike t n r ()
check = C.check

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

-- | The result of an STM transaction, along with which 'CTVar's it
-- touched whilst executing.
data Result a =
    Success [CTVarId] a
  -- ^ The transaction completed successfully, and mutated the returned 'CTVar's.
  | Retry   [CTVarId]
  -- ^ The transaction aborted by calling 'retry', and read the
  -- returned 'CTVar's. It should be retried when at least one of the
  -- 'CTVar's has been mutated.
  deriving (Show, Eq)

-- | Run a transaction in the 'ST' monad, starting from a clean
-- environment, and discarding the environment afterwards. This is
-- suitable for testing individual transactions, but not for composing
-- multiple ones.
runTransaction :: (forall t. STMLike t (ST t) (STRef t) a) -> Result a
runTransaction ma = fst $ runST $ runTransactionST ma 0

-- | Run a transaction in the 'ST' monad, returning the result and new
-- initial 'CTVarId'. If the transaction ended by calling 'retry', any
-- 'CTVar' modifications are undone.
runTransactionST :: STMLike t (ST t) (STRef t) a -> CTVarId -> ST t (Result a, CTVarId)
runTransactionST ma ctvid = do
  (res, undo, ctvid') <- doTransaction fixedST ma ctvid

  case res of
    Retry _ -> undo >> return (res, ctvid)
    _ -> return (res, ctvid')

-- | Run a transaction in the 'IO' monad, returning the result and new
-- initial 'CTVarId'. If the transaction ended by calling 'retry', any
-- 'CTVar' modifications are undone.
runTransactionIO :: STMLike t IO IORef a -> CTVarId -> IO (Result a, CTVarId)
runTransactionIO ma ctvid = do
  (res, undo, ctvid') <- doTransaction fixedIO ma ctvid

  case res of
    Retry _ -> undo >> return (res, ctvid)
    _ -> return (res, ctvid')

-- | Run a STM transaction, returning an action to undo its effects.
doTransaction :: Monad n => Fixed t n r -> STMLike t n r a -> CTVarId -> n (Result a, n (), CTVarId)
doTransaction fixed ma newctvid = do
  ref <- newRef (wref fixed) Nothing

  let c = runCont (unS $ ma >>= liftN fixed . writeRef (wref fixed) ref . Just) $ const AStop

  (newctvid', undo, readen, written) <- go ref c (return ()) newctvid [] []

  res <- readRef (wref fixed) ref

  case res of
    Just val -> return (Success (nub written) val, undo, newctvid')
    Nothing  -> undo >> return (Retry $ nub readen, undo, newctvid')

  where
    go ref act undo nctvid readen written = do
      (act', undo', nctvid', readen', written') <- stepTrans fixed act nctvid
      case act' of
        AStop  -> return (nctvid', undo >> undo', readen' ++ readen, written' ++ written)
        ARetry -> writeRef (wref fixed) ref Nothing >> return (nctvid', undo >> undo', readen' ++ readen, written' ++ written)
        _      -> go ref act' (undo >> undo') nctvid' (readen' ++ readen) (written' ++ written)

-- | Run a transaction for one step.
stepTrans :: forall t n r. Monad n => Fixed t n r -> STMAction t n r -> CTVarId -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
stepTrans fixed act newctvid = case act of
  ARead   ref c   -> stepRead ref c
  AWrite  ref a c -> stepWrite ref a c
  ANew    na      -> stepNew na
  AOrElse a b c   -> stepOrElse a b c
  ALift   na      -> stepLift na
  ARetry -> return (ARetry, nothing, newctvid, [], [])
  AStop  -> return (AStop, nothing, newctvid, [], [])

  where
    nothing = return ()

    stepRead :: CTVar t r a -> (a -> STMAction t n r) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepRead (V (ctvid, ref)) c = do
      val <- readRef (wref fixed) ref
      return (c val, nothing, newctvid, [ctvid], [])

    stepWrite :: CTVar t r a -> a -> STMAction t n r -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepWrite (V (ctvid, ref)) a c = do
      old <- readRef (wref fixed) ref
      writeRef (wref fixed) ref a
      return (c, writeRef (wref fixed) ref old, newctvid, [], [ctvid])

    stepNew :: (Ref n r -> CTVarId -> n (STMAction t n r)) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepNew na = do
      let newctvid' = newctvid + 1
      a <- na (wref fixed) newctvid
      return (a, nothing, newctvid', [], [newctvid])

    stepOrElse :: STMLike t n r a -> STMLike t n r a -> (a -> STMAction t n r) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepOrElse a b c = do
      (resa, undoa, newctvida') <- doTransaction fixed a newctvid
      case resa of
        Success written val -> return (c val, undoa, newctvida', [], written)
        Retry _ -> do
          undoa
          (resb, undob, newctvidb') <- doTransaction fixed b newctvid
          case resb of
            Success written val -> return (c val, undob, newctvidb', [], written)
            Retry readen -> return (ARetry, undob, newctvidb', readen, [])

    stepLift :: n (STMAction t n r) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepLift na = do
      a <- na
      return (a, nothing, newctvid, [], [])

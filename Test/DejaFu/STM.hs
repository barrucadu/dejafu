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
import Control.Exception (Exception, SomeException(..), fromException)
import Control.Monad (liftM)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
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

-- | STM transactions are represented as a sequence of primitive
-- actions.
data STMAction t n r
  = forall a e. Exception e => ACatch (STMLike t n r a) (e -> STMLike t n r a) (a -> STMAction t n r)
  | forall a. ARead  (CTVar t r a) (a -> STMAction t n r)
  | forall a. AWrite (CTVar t r a) a (STMAction t n r)
  | forall a. AOrElse (STMLike t n r a) (STMLike t n r a) (a -> STMAction t n r)
  | ANew (Ref n r -> CTVarId -> n (STMAction t n r))
  | ALift (n (STMAction t n r))
  | AThrow SomeException
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
orElse a b = S $ cont $ AOrElse a b

-- | Check whether a condition is true and, if not, call 'retry'.
check :: Monad n => Bool -> STMLike t n r ()
check = C.check

-- | Throw an exception. This aborts the transaction and propagates
-- the exception.
throwSTM :: Exception e => e -> STMLike t n r a
throwSTM e = S $ cont $ const $ AThrow (SomeException e)

-- | Handling exceptions from 'throwSTM'.
catchSTM :: Exception e => STMLike t n r a -> (e -> STMLike t n r a) -> STMLike t n r a
catchSTM stm handler = S $ cont $ ACatch stm handler

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
  | Exception SomeException
  -- ^ The transaction aborted by throwing an exception.
  deriving Show

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
    Success _ _ -> return (res, ctvid')
    _ -> undo >> return (res, ctvid)

-- | Run a transaction in the 'IO' monad, returning the result and new
-- initial 'CTVarId'. If the transaction ended by calling 'retry', any
-- 'CTVar' modifications are undone.
runTransactionIO :: STMLike t IO IORef a -> CTVarId -> IO (Result a, CTVarId)
runTransactionIO ma ctvid = do
  (res, undo, ctvid') <- doTransaction fixedIO ma ctvid

  case res of
    Success _ _ -> return (res, ctvid')
    _ -> undo >> return (res, ctvid)

-- | Run a STM transaction, returning an action to undo its effects.
doTransaction :: Monad n => Fixed t n r -> STMLike t n r a -> CTVarId -> n (Result a, n (), CTVarId)
doTransaction fixed ma newctvid = do
  ref <- newRef (wref fixed) Nothing

  let c = runCont (unS $ ma >>= liftN fixed . writeRef (wref fixed) ref . Just . Right) $ const AStop

  (newctvid', undo, readen, written) <- go ref c (return ()) newctvid [] []

  res <- readRef (wref fixed) ref

  case res of
    Just (Right val) -> return (Success (nub written) val, undo, newctvid')

    Just (Left  exc) -> undo >> return (Exception exc,      return (), newctvid)
    Nothing          -> undo >> return (Retry $ nub readen, return (), newctvid)

  where
    go ref act undo nctvid readen written = do
      (act', undo', nctvid', readen', written') <- stepTrans fixed act nctvid
      let ret = (nctvid', undo >> undo', readen' ++ readen, written' ++ written)
      case act' of
        AStop      -> return ret
        ARetry     -> writeRef (wref fixed) ref Nothing >> return ret
        AThrow exc -> writeRef (wref fixed) ref (Just $ Left exc) >> return ret

        _ -> go ref act' (undo >> undo') nctvid' (readen' ++ readen) (written' ++ written)

-- | Run a transaction for one step.
stepTrans :: forall t n r. Monad n => Fixed t n r -> STMAction t n r -> CTVarId -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
stepTrans fixed act newctvid = case act of
  ACatch  stm h c -> stepCatch stm h c
  ARead   ref c   -> stepRead ref c
  AWrite  ref a c -> stepWrite ref a c
  ANew    na      -> stepNew na
  AOrElse a b c   -> stepOrElse a b c
  ALift   na      -> stepLift na

  AThrow exc -> return (AThrow exc, nothing, newctvid, [], [])
  ARetry     -> return (ARetry,     nothing, newctvid, [], [])
  AStop      -> return (AStop,      nothing, newctvid, [], [])

  where
    nothing = return ()

    stepCatch :: Exception e => STMLike t n r a -> (e -> STMLike t n r a) -> (a -> STMAction t n r) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepCatch stm h c = do
      (res, undo, newctvid') <- doTransaction fixed stm newctvid
      case res of
        Success written val -> return (c val, undo, newctvid', [], written)
        Retry readen -> return (ARetry, nothing, newctvid, readen, [])
        Exception exc -> case fromException exc of
          Just exc' -> do
            (rese, undoe, newctvide') <- doTransaction fixed (h exc') newctvid
            case rese of
              Success written val -> return (c val, undoe, newctvide', [], written)
              Exception exce -> return (AThrow exce, nothing, newctvid, [], [])
              Retry readen -> return (ARetry, nothing, newctvid, readen, [])
          Nothing -> return (AThrow exc, nothing, newctvid, [], [])

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
        Exception exc -> return (AThrow exc, nothing, newctvid, [], [])
        Retry _ -> do
          (resb, undob, newctvidb') <- doTransaction fixed b newctvid
          case resb of
            Success written val -> return (c val, undob, newctvidb', [], written)
            Exception exc -> return (AThrow exc, nothing, newctvid, [], [])
            Retry readen -> return (ARetry, nothing, newctvid, readen, [])

    stepLift :: n (STMAction t n r) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepLift na = do
      a <- na
      return (a, nothing, newctvid, [], [])

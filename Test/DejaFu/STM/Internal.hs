{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | 'MonadSTM' testing implementation, internal types and
-- definitions.
module Test.DejaFu.STM.Internal where

import Control.Exception (Exception, SomeException(..), fromException)
import Control.Monad.Cont (Cont, runCont)
import Data.Foldable (Foldable(..))
import Data.List (nub)
import Data.Monoid (mempty)
import Test.DejaFu.Internal

--------------------------------------------------------------------------------
-- The @STMLike@ monad

-- | The underlying monad is based on continuations over primitive
-- actions.
type M t n r a = Cont (STMAction t n r) a

-- | Dict of methods for implementations to override.
type Fixed t n r = Ref n r (Cont (STMAction t n r))

--------------------------------------------------------------------------------
-- * Primitive actions

-- | STM transactions are represented as a sequence of primitive
-- actions.
data STMAction t n r
  = forall a e. Exception e => ACatch (M t n r a) (e -> M t n r a) (a -> STMAction t n r)
  | forall a. ARead  (CTVar t r a) (a -> STMAction t n r)
  | forall a. AWrite (CTVar t r a) a (STMAction t n r)
  | forall a. AOrElse (M t n r a) (M t n r a) (a -> STMAction t n r)
  | ANew (Fixed t n r -> CTVarId -> n (STMAction t n r))
  | ALift (n (STMAction t n r))
  | AThrow SomeException
  | ARetry
  | AStop

--------------------------------------------------------------------------------
-- * @CTVar@s

-- | A 'CTVar' is a tuple of a unique ID and the value contained. The
-- ID is so that blocked transactions can be re-run when a 'CTVar'
-- they depend on has changed.
newtype CTVar t r a = V (CTVarId, r a)

-- | The unique ID of a 'CTVar'. Only meaningful within a single
-- concurrent computation.
type CTVarId = Int

--------------------------------------------------------------------------------
-- * Output

-- | The result of an STM transaction, along with which 'CTVar's it
-- touched whilst executing.
data Result a =
    Success [CTVarId] [CTVarId] a
  -- ^ The transaction completed successfully, reading the first list
  -- 'CTVar's and writing to the second.
  | Retry   [CTVarId]
  -- ^ The transaction aborted by calling 'retry', and read the
  -- returned 'CTVar's. It should be retried when at least one of the
  -- 'CTVar's has been mutated.
  | Exception SomeException
  -- ^ The transaction aborted by throwing an exception.
  deriving Show

instance Functor Result where
  fmap f (Success rs ws a) = Success rs ws $ f a
  fmap _ (Retry rs)    = Retry rs
  fmap _ (Exception e) = Exception e

instance Foldable Result where
  foldMap f (Success _ _ a) = f a
  foldMap _ _ = mempty

--------------------------------------------------------------------------------
-- * Execution

-- | Run a STM transaction, returning an action to undo its effects.
doTransaction :: Monad n => Fixed t n r -> M t n r a -> CTVarId -> n (Result a, n (), CTVarId)
doTransaction fixed ma newctvid = do
  ref <- newRef fixed Nothing

  let c = runCont (ma >>= liftN fixed . writeRef fixed ref . Just . Right) $ const AStop

  (newctvid', undo, readen, written) <- go ref c (return ()) newctvid [] []

  res <- readRef fixed ref

  case res of
    Just (Right val) -> return (Success (nub readen) (nub written) val, undo, newctvid')

    Just (Left  exc) -> undo >> return (Exception exc,      return (), newctvid)
    Nothing          -> undo >> return (Retry $ nub readen, return (), newctvid)

  where
    go ref act undo nctvid readen written = do
      (act', undo', nctvid', readen', written') <- stepTrans fixed act nctvid
      let ret = (nctvid', undo >> undo', readen' ++ readen, written' ++ written)
      case act' of
        AStop      -> return ret
        ARetry     -> writeRef fixed ref Nothing >> return ret
        AThrow exc -> writeRef fixed ref (Just $ Left exc) >> return ret

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

    stepCatch :: Exception e => M t n r a -> (e -> M t n r a) -> (a -> STMAction t n r) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepCatch stm h c = do
      (res, undo, newctvid') <- doTransaction fixed stm newctvid
      case res of
        Success readen written val -> return (c val, undo, newctvid', readen, written)
        Retry readen -> return (ARetry, nothing, newctvid, readen, [])
        Exception exc -> case fromException exc of
          Just exc' -> do
            (rese, undoe, newctvide') <- doTransaction fixed (h exc') newctvid
            case rese of
              Success readen written val -> return (c val, undoe, newctvide', readen, written)
              Exception exce -> return (AThrow exce, nothing, newctvid, [], [])
              Retry readen -> return (ARetry, nothing, newctvid, readen, [])
          Nothing -> return (AThrow exc, nothing, newctvid, [], [])

    stepRead :: CTVar t r a -> (a -> STMAction t n r) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepRead (V (ctvid, ref)) c = do
      val <- readRef fixed ref
      return (c val, nothing, newctvid, [ctvid], [])

    stepWrite :: CTVar t r a -> a -> STMAction t n r -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepWrite (V (ctvid, ref)) a c = do
      old <- readRef fixed ref
      writeRef fixed ref a
      return (c, writeRef fixed ref old, newctvid, [], [ctvid])

    stepNew :: (Fixed t n r -> CTVarId -> n (STMAction t n r)) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepNew na = do
      let newctvid' = newctvid + 1
      a <- na fixed newctvid
      return (a, nothing, newctvid', [], [newctvid])

    stepOrElse :: M t n r a -> M t n r a -> (a -> STMAction t n r) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepOrElse a b c = do
      (resa, undoa, newctvida') <- doTransaction fixed a newctvid
      case resa of
        Success readen written val -> return (c val, undoa, newctvida', readen, written)
        Exception exc -> return (AThrow exc, nothing, newctvid, [], [])
        Retry _ -> do
          (resb, undob, newctvidb') <- doTransaction fixed b newctvid
          case resb of
            Success readen written val -> return (c val, undob, newctvidb', readen, written)
            Exception exc -> return (AThrow exc, nothing, newctvid, [], [])
            Retry readen -> return (ARetry, nothing, newctvid, readen, [])

    stepLift :: n (STMAction t n r) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
    stepLift na = do
      a <- na
      return (a, nothing, newctvid, [], [])

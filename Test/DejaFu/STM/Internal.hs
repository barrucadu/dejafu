{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | 'MonadSTM' testing implementation, internal types and
-- definitions.
module Test.DejaFu.STM.Internal where

import Control.Exception (Exception, SomeException(..), fromException)
import Control.Monad.Cont (Cont, runCont)
import Control.State
import Data.List (nub)

--------------------------------------------------------------------------------
-- The @STMLike@ monad

-- | The underlying monad is based on continuations over primitive
-- actions.
type M t n r a = Cont (STMAction t n r) a

-- | Dict of methods for implementations to override.
type Fixed t n r = Wrapper n r (Cont (STMAction t n r))

--------------------------------------------------------------------------------
-- * Primitive actions

-- | STM transactions are represented as a sequence of primitive
-- actions.
data STMAction t n r
  = forall a e. Exception e => ACatch (M t n r a) (e -> M t n r a) (a -> STMAction t n r)
  | forall a. ARead  (CTVar t r a) (a -> STMAction t n r)
  | forall a. AWrite (CTVar t r a) a (STMAction t n r)
  | forall a. AOrElse (M t n r a) (M t n r a) (a -> STMAction t n r)
  | ANew (Ref n r -> CTVarId -> n (STMAction t n r))
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
    Success [CTVarId] a
  -- ^ The transaction completed successfully, and mutated the returned 'CTVar's.
  | Retry   [CTVarId]
  -- ^ The transaction aborted by calling 'retry', and read the
  -- returned 'CTVar's. It should be retried when at least one of the
  -- 'CTVar's has been mutated.
  | Exception SomeException
  -- ^ The transaction aborted by throwing an exception.
  deriving Show

--------------------------------------------------------------------------------
-- * Execution

-- | Run a STM transaction, returning an action to undo its effects.
doTransaction :: Monad n => Fixed t n r -> M t n r a -> CTVarId -> n (Result a, n (), CTVarId)
doTransaction fixed ma newctvid = do
  ref <- newRef (wref fixed) Nothing

  let c = runCont (ma >>= liftN fixed . writeRef (wref fixed) ref . Just . Right) $ const AStop

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

    stepCatch :: Exception e => M t n r a -> (e -> M t n r a) -> (a -> STMAction t n r) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
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

    stepOrElse :: M t n r a -> M t n r a -> (a -> STMAction t n r) -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

-- | 'MonadSTM' testing implementation, internal types and
-- definitions.
module Test.DejaFu.STM.Internal where

import Control.Exception (Exception, SomeException, fromException, toException)
import Control.Monad.Cont (Cont, runCont)
import Data.List (nub)
import Test.DejaFu.Deterministic.Internal.Common (CTVarId, IdSource, TAction(..), TTrace, nextCTVId)
import Test.DejaFu.Internal

--------------------------------------------------------------------------------
-- The @STMLike@ monad

-- | The underlying monad is based on continuations over primitive
-- actions.
type M n r a = Cont (STMAction n r) a

-- | Dict of methods for implementations to override.
type Fixed n r = Ref n r (Cont (STMAction n r))

--------------------------------------------------------------------------------
-- * Primitive actions

-- | STM transactions are represented as a sequence of primitive
-- actions.
data STMAction n r
  = forall a e. Exception e => SCatch (e -> M n r a) (M n r a) (a -> STMAction n r)
  | forall a. SRead  (CTVar r a) (a -> STMAction n r)
  | forall a. SWrite (CTVar r a) a (STMAction n r)
  | forall a. SOrElse (M n r a) (M n r a) (a -> STMAction n r)
  | forall a. SNew String a (CTVar r a -> STMAction n r)
  | SLift (n (STMAction n r))
  | forall e. Exception e => SThrow e
  | SRetry
  | SStop

--------------------------------------------------------------------------------
-- * @CTVar@s

-- | A 'CTVar' is a tuple of a unique ID and the value contained. The
-- ID is so that blocked transactions can be re-run when a 'CTVar'
-- they depend on has changed.
newtype CTVar r a = CTVar (CTVarId, r a)

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
doTransaction :: Monad n => Fixed n r -> M n r a -> IdSource -> n (Result a, n (), IdSource, TTrace)
doTransaction fixed ma idsource = do
  ref <- newRef fixed Nothing

  let c = runCont (ma >>= liftN fixed . writeRef fixed ref . Just . Right) $ const SStop

  (idsource', undo, readen, written, trace) <- go ref c (return ()) idsource [] [] []

  res <- readRef fixed ref

  case res of
    Just (Right val) -> return (Success (nub readen) (nub written) val, undo, idsource', reverse trace)

    Just (Left  exc) -> undo >> return (Exception exc,      return (), idsource, reverse trace)
    Nothing          -> undo >> return (Retry $ nub readen, return (), idsource, reverse trace)

  where
    go ref act undo nidsrc readen written sofar = do
      (act', undo', nidsrc', readen', written', tact) <- stepTrans fixed act nidsrc

      let newIDSource = nidsrc'
          newAct = act'
          newUndo = undo >> undo'
          newReaden = readen' ++ readen
          newWritten = written' ++ written
          newSofar = tact : sofar

      case tact of
        TStop  -> return (newIDSource, newUndo, newReaden, newWritten, TStop:newSofar)
        TRetry -> writeRef fixed ref Nothing
          >> return (newIDSource, newUndo, newReaden, newWritten, TRetry:newSofar)
        TThrow -> writeRef fixed ref (Just . Left $ case act of SThrow e -> toException e; _ -> undefined)
          >> return (newIDSource, newUndo, newReaden, newWritten, TThrow:newSofar)
        _ -> go ref newAct newUndo newIDSource newReaden newWritten newSofar

-- | Run a transaction for one step.
stepTrans :: Monad n => Fixed n r -> STMAction n r -> IdSource -> n (STMAction n r, n (), IdSource, [CTVarId], [CTVarId], TAction)
stepTrans fixed act idsource = case act of
  SCatch  h stm c -> stepCatch h stm c
  SRead   ref c   -> stepRead ref c
  SWrite  ref a c -> stepWrite ref a c
  SNew    n a c   -> stepNew n a c
  SOrElse a b c   -> stepOrElse a b c
  SLift   na      -> stepLift na

  SThrow e -> return (SThrow e, nothing, idsource, [], [], TThrow)
  SRetry   -> return (SRetry,   nothing, idsource, [], [], TRetry)
  SStop    -> return (SStop,    nothing, idsource, [], [], TStop)

  where
    nothing = return ()

    stepCatch h stm c = cases TCatch stm c
      (\trace readen -> return (SRetry, nothing, idsource, readen, [], TCatch trace Nothing))
      (\trace exc    -> case fromException exc of
        Just exc' -> transaction (TCatch trace . Just) (h exc') c
        Nothing   -> return (SThrow exc, nothing, idsource, [], [], TCatch trace Nothing))

    stepRead (CTVar (ctvid, ref)) c = do
      val <- readRef fixed ref
      return (c val, nothing, idsource, [ctvid], [], TRead ctvid)

    stepWrite (CTVar (ctvid, ref)) a c = do
      old <- readRef fixed ref
      writeRef fixed ref a
      return (c, writeRef fixed ref old, idsource, [], [ctvid], TWrite ctvid)

    stepNew n a c = do
      let (idsource', ctvid) = nextCTVId n idsource
      ref <- newRef fixed a
      let ctvar = CTVar (ctvid, ref)
      return (c ctvar, nothing, idsource', [], [ctvid], TNew)

    stepOrElse a b c = cases TOrElse a c
      (\trace _   -> transaction (TOrElse trace . Just) b c)
      (\trace exc -> return (SThrow exc, nothing, idsource, [], [], TOrElse trace Nothing))

    stepLift na = do
      a <- na
      return (a, nothing, idsource, [], [], TLift)

    cases tact stm onSuccess onRetry onException = do
      (res, undo, idsource', trace) <- doTransaction fixed stm idsource
      case res of
        Success readen written val -> return (onSuccess val, undo, idsource', readen, written, tact trace Nothing)
        Retry readen  -> onRetry     trace readen
        Exception exc -> onException trace exc

    transaction tact stm onSuccess = cases (\t _ -> tact t) stm onSuccess
      (\trace readen -> return (SRetry, nothing, idsource, readen, [], tact trace))
      (\trace exc    -> return (SThrow exc, nothing, idsource, [], [], tact trace))

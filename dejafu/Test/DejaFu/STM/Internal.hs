{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Test.DejaFu.STM.Internal
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : ExistentialQuantification, RankNTypes
--
-- 'MonadSTM' testing implementation, internal types and
-- definitions. This module is NOT considered to form part of the
-- public interface of this library.
module Test.DejaFu.STM.Internal where

import Control.Exception (Exception, SomeException, fromException, toException)
import Control.Monad.Cont (Cont, runCont)
import Control.Monad.Ref (MonadRef, newRef, readRef, writeRef)
import Data.List (nub)

import Test.DejaFu.Common

{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}

--------------------------------------------------------------------------------
-- The @STMLike@ monad

-- | The underlying monad is based on continuations over primitive
-- actions.
type M n r a = Cont (STMAction n r) a

--------------------------------------------------------------------------------
-- * Primitive actions

-- | STM transactions are represented as a sequence of primitive
-- actions.
data STMAction n r
  = forall a e. Exception e => SCatch (e -> M n r a) (M n r a) (a -> STMAction n r)
  | forall a. SRead  (TVar r a) (a -> STMAction n r)
  | forall a. SWrite (TVar r a) a (STMAction n r)
  | forall a. SOrElse (M n r a) (M n r a) (a -> STMAction n r)
  | forall a. SNew String a (TVar r a -> STMAction n r)
  | forall e. Exception e => SThrow e
  | SRetry
  | SStop (n ())

--------------------------------------------------------------------------------
-- * @TVar@s

-- | A 'TVar' is a tuple of a unique ID and the value contained. The
-- ID is so that blocked transactions can be re-run when a 'TVar' they
-- depend on has changed.
newtype TVar r a = TVar (TVarId, r a)

--------------------------------------------------------------------------------
-- * Output

-- | The result of an STM transaction, along with which 'TVar's it
-- touched whilst executing.
data Result a =
    Success [TVarId] [TVarId] a
  -- ^ The transaction completed successfully, reading the first list
  -- 'TVar's and writing to the second.
  | Retry [TVarId]
  -- ^ The transaction aborted by calling 'retry', and read the
  -- returned 'TVar's. It should be retried when at least one of the
  -- 'TVar's has been mutated.
  | Exception SomeException
  -- ^ The transaction aborted by throwing an exception.
  deriving Show

-- | Check if a 'Result' is a @Success@.
isSTMSuccess :: Result a -> Bool
isSTMSuccess (Success _ _ _) = True
isSTMSuccess _ = False

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
doTransaction :: MonadRef r n => M n r a -> IdSource -> n (Result a, n (), IdSource, TTrace)
doTransaction ma idsource = do
  ref <- newRef Nothing

  let c = runCont ma (SStop . writeRef ref . Just . Right)

  (idsource', undo, readen, written, trace) <- go ref c (return ()) idsource [] [] []

  res <- readRef ref

  case res of
    Just (Right val) -> return (Success (nub readen) (nub written) val, undo, idsource', reverse trace)

    Just (Left  exc) -> undo >> return (Exception exc,      return (), idsource, reverse trace)
    Nothing          -> undo >> return (Retry $ nub readen, return (), idsource, reverse trace)

  where
    go ref act undo nidsrc readen written sofar = do
      (act', undo', nidsrc', readen', written', tact) <- stepTrans act nidsrc

      let newIDSource = nidsrc'
          newAct = act'
          newUndo = undo >> undo'
          newReaden = readen' ++ readen
          newWritten = written' ++ written
          newSofar = tact : sofar

      case tact of
        TStop  -> return (newIDSource, newUndo, newReaden, newWritten, TStop:newSofar)
        TRetry -> writeRef ref Nothing
          >> return (newIDSource, newUndo, newReaden, newWritten, TRetry:newSofar)
        TThrow -> writeRef ref (Just . Left $ case act of SThrow e -> toException e; _ -> undefined)
          >> return (newIDSource, newUndo, newReaden, newWritten, TThrow:newSofar)
        _ -> go ref newAct newUndo newIDSource newReaden newWritten newSofar

-- | Run a transaction for one step.
stepTrans :: MonadRef r n => STMAction n r -> IdSource -> n (STMAction n r, n (), IdSource, [TVarId], [TVarId], TAction)
stepTrans act idsource = case act of
  SCatch  h stm c -> stepCatch h stm c
  SRead   ref c   -> stepRead ref c
  SWrite  ref a c -> stepWrite ref a c
  SNew    n a c   -> stepNew n a c
  SOrElse a b c   -> stepOrElse a b c
  SStop   na      -> stepStop na

  SThrow e -> return (SThrow e, nothing, idsource, [], [], TThrow)
  SRetry   -> return (SRetry,   nothing, idsource, [], [], TRetry)

  where
    nothing = return ()

    stepCatch h stm c = cases TCatch stm c
      (\trace -> return (SRetry, nothing, idsource, [], [], TCatch trace Nothing))
      (\trace exc    -> case fromException exc of
        Just exc' -> transaction (TCatch trace . Just) (h exc') c
        Nothing   -> return (SThrow exc, nothing, idsource, [], [], TCatch trace Nothing))

    stepRead (TVar (tvid, ref)) c = do
      val <- readRef ref
      return (c val, nothing, idsource, [tvid], [], TRead tvid)

    stepWrite (TVar (tvid, ref)) a c = do
      old <- readRef ref
      writeRef ref a
      return (c, writeRef ref old, idsource, [], [tvid], TWrite tvid)

    stepNew n a c = do
      let (idsource', tvid) = nextTVId n idsource
      ref <- newRef a
      let tvar = TVar (tvid, ref)
      return (c tvar, nothing, idsource', [], [tvid], TNew)

    stepOrElse a b c = cases TOrElse a c
      (\trace   -> transaction (TOrElse trace . Just) b c)
      (\trace exc -> return (SThrow exc, nothing, idsource, [], [], TOrElse trace Nothing))

    stepStop na = do
      na
      return (SStop na, nothing, idsource, [], [], TStop)

    cases tact stm onSuccess onRetry onException = do
      (res, undo, idsource', trace) <- doTransaction stm idsource
      case res of
        Success readen written val -> return (onSuccess val, undo, idsource', readen, written, tact trace Nothing)
        Retry readen -> do
          (res', undo', idsource'', readen', written', trace') <- onRetry trace
          pure (res', undo', idsource'', readen ++ readen', written', trace')
        Exception exc -> onException trace exc

    transaction tact stm onSuccess = cases (\t _ -> tact t) stm onSuccess
      (\trace     -> return (SRetry, nothing, idsource, [], [], tact trace))
      (\trace exc -> return (SThrow exc, nothing, idsource, [], [], tact trace))

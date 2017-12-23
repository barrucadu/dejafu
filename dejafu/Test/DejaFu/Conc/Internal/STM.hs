{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- Must come after TypeFamilies
{-# LANGUAGE NoMonoLocalBinds #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal.STM
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, ExistentialQuantification, MultiParamTypeClasses, NoMonoLocalBinds, TypeFamilies
--
-- 'MonadSTM' testing implementation, internal types and definitions.
-- This module is NOT considered to form part of the public interface
-- of this library.
module Test.DejaFu.Conc.Internal.STM where

import           Control.Applicative     (Alternative(..))
import           Control.Exception       (Exception, SomeException,
                                          fromException, toException)
import           Control.Monad           (MonadPlus(..))
import           Control.Monad.Catch     (MonadCatch(..), MonadThrow(..))
import           Control.Monad.Ref       (MonadRef, newRef, readRef, writeRef)
import           Data.List               (nub)

import qualified Control.Monad.STM.Class as C
import           Test.DejaFu.Internal
import           Test.DejaFu.Types

#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail      as Fail
#endif

--------------------------------------------------------------------------------
-- * The @S@ monad

-- | The underlying monad is based on continuations over primitive
-- actions.
--
-- This is not @Cont@ because we want to give it a custom @MonadFail@
-- instance.
newtype S n r a = S { runSTM :: (a -> STMAction n r) -> STMAction n r }

instance Functor (S n r) where
    fmap f m = S $ \c -> runSTM m (c . f)

instance Applicative (S n r) where
    pure x  = S $ \c -> c x
    f <*> v = S $ \c -> runSTM f (\g -> runSTM v (c . g))

instance Monad (S n r) where
    return  = pure
    m >>= k = S $ \c -> runSTM m (\x -> runSTM (k x) c)

#if MIN_VERSION_base(4,9,0)
    fail = Fail.fail

instance Fail.MonadFail (S n r) where
#endif
    fail e = S $ \_ -> SThrow (MonadFailException e)

instance MonadThrow (S n r) where
  throwM e = S $ \_ -> SThrow e

instance MonadCatch (S n r) where
  catch stm handler = S $ SCatch handler stm

instance Alternative (S n r) where
  a <|> b = S $ SOrElse a b
  empty = S $ const SRetry

instance MonadPlus (S n r)

instance C.MonadSTM (S n r) where
  type TVar (S n r) = TVar r

  newTVarN n = S . SNew n

  readTVar = S . SRead

  writeTVar tvar a = S $ \c -> SWrite tvar a (c ())

--------------------------------------------------------------------------------
-- * Primitive actions

-- | STM transactions are represented as a sequence of primitive
-- actions.
data STMAction n r
  = forall a e. Exception e => SCatch (e -> S n r a) (S n r a) (a -> STMAction n r)
  | forall a. SRead  (TVar r a) (a -> STMAction n r)
  | forall a. SWrite (TVar r a) a (STMAction n r)
  | forall a. SOrElse (S n r a) (S n r a) (a -> STMAction n r)
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


--------------------------------------------------------------------------------
-- * Execution

-- | Run a transaction, returning the result and new initial 'TVarId'.
-- If the transaction failed, any effects are undone.
runTransaction :: MonadRef r n
  => S n r a
  -> IdSource
  -> n (Result a, IdSource, [TAction])
runTransaction ma tvid = do
  (res, _, tvid', trace) <- doTransaction ma tvid
  pure (res, tvid', trace)

-- | Run a STM transaction, returning an action to undo its effects.
--
-- If the transaction fails, its effects will automatically be undone,
-- so the undo action returned will be @pure ()@.
doTransaction :: MonadRef r n
  => S n r a
  -> IdSource
  -> n (Result a, n (), IdSource, [TAction])
doTransaction ma idsource = do
  (c, ref) <- runRefCont SStop (Just . Right) (runSTM ma)
  (idsource', undo, readen, written, trace) <- go ref c (pure ()) idsource [] [] []
  res <- readRef ref

  case res of
    Just (Right val) -> pure (Success (nub readen) (nub written) val, undo, idsource', reverse trace)
    Just (Left  exc) -> undo >> pure (Exception exc,      pure (), idsource, reverse trace)
    Nothing          -> undo >> pure (Retry $ nub readen, pure (), idsource, reverse trace)

  where
    go ref act undo nidsrc readen written sofar = do
      (act', undo', nidsrc', readen', written', tact) <- stepTrans act nidsrc

      let newIDSource = nidsrc'
          newAct = act'
          newUndo = undo' >> undo
          newReaden = readen' ++ readen
          newWritten = written' ++ written
          newSofar = tact : sofar

      case tact of
        TStop  -> pure (newIDSource, newUndo, newReaden, newWritten, TStop:newSofar)
        TRetry -> do
          writeRef ref Nothing
          pure (newIDSource, newUndo, newReaden, newWritten, TRetry:newSofar)
        TThrow -> do
          writeRef ref (Just . Left $ case act of SThrow e -> toException e; _ -> undefined)
          pure (newIDSource, newUndo, newReaden, newWritten, TThrow:newSofar)
        _ -> go ref newAct newUndo newIDSource newReaden newWritten newSofar

-- | Run a transaction for one step.
stepTrans :: MonadRef r n
  => STMAction n r
  -> IdSource
  -> n (STMAction n r, n (), IdSource, [TVarId], [TVarId], TAction)
stepTrans act idsource = case act of
  SCatch  h stm c -> stepCatch h stm c
  SRead   ref c   -> stepRead ref c
  SWrite  ref a c -> stepWrite ref a c
  SNew    n a c   -> stepNew n a c
  SOrElse a b c   -> stepOrElse a b c
  SStop   na      -> stepStop na

  SThrow e -> pure (SThrow e, nothing, idsource, [], [], TThrow)
  SRetry   -> pure (SRetry,   nothing, idsource, [], [], TRetry)

  where
    nothing = pure ()

    stepCatch h stm c = cases TCatch stm c
      (\trace -> pure (SRetry, nothing, idsource, [], [], TCatch trace Nothing))
      (\trace exc    -> case fromException exc of
        Just exc' -> transaction (TCatch trace . Just) (h exc') c
        Nothing   -> pure (SThrow exc, nothing, idsource, [], [], TCatch trace Nothing))

    stepRead (TVar (tvid, ref)) c = do
      val <- readRef ref
      pure (c val, nothing, idsource, [tvid], [], TRead tvid)

    stepWrite (TVar (tvid, ref)) a c = do
      old <- readRef ref
      writeRef ref a
      pure (c, writeRef ref old, idsource, [], [tvid], TWrite tvid)

    stepNew n a c = do
      let (idsource', tvid) = nextTVId n idsource
      ref <- newRef a
      let tvar = TVar (tvid, ref)
      pure (c tvar, nothing, idsource', [], [tvid], TNew tvid)

    stepOrElse a b c = cases TOrElse a c
      (\trace   -> transaction (TOrElse trace . Just) b c)
      (\trace exc -> pure (SThrow exc, nothing, idsource, [], [], TOrElse trace Nothing))

    stepStop na = do
      na
      pure (SStop na, nothing, idsource, [], [], TStop)

    cases tact stm onSuccess onRetry onException = do
      (res, undo, idsource', trace) <- doTransaction stm idsource
      case res of
        Success readen written val -> pure (onSuccess val, undo, idsource', readen, written, tact trace Nothing)
        Retry readen -> do
          (res', undo', idsource'', readen', written', trace') <- onRetry trace
          pure (res', undo', idsource'', readen ++ readen', written', trace')
        Exception exc -> onException trace exc

    transaction tact stm onSuccess = cases (\t _ -> tact t) stm onSuccess
      (\trace     -> pure (SRetry, nothing, idsource, [], [], tact trace))
      (\trace exc -> pure (SThrow exc, nothing, idsource, [], [], tact trace))

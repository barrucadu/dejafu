{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- Must come after TypeFamilies
{-# LANGUAGE NoMonoLocalBinds #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal.STM
-- Copyright   : (c) 2017--2019 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, ExistentialQuantification, NoMonoLocalBinds, RecordWildCards, TypeFamilies
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
import qualified Control.Monad.Fail      as Fail
import qualified Control.Monad.STM.Class as S
import           Data.List               (nub)

import           Test.DejaFu.Internal
import           Test.DejaFu.Types

--------------------------------------------------------------------------------
-- * The @ModelSTM@ monad

-- | The underlying monad is based on continuations over primitive
-- actions.
--
-- This is not @Cont@ because we want to give it a custom @MonadFail@
-- instance.
newtype ModelSTM n a = ModelSTM { runModelSTM :: (a -> STMAction n) -> STMAction n }

instance Functor (ModelSTM n) where
    fmap f m = ModelSTM $ \c -> runModelSTM m (c . f)

instance Applicative (ModelSTM n) where
    pure x  = ModelSTM $ \c -> c x
    f <*> v = ModelSTM $ \c -> runModelSTM f (\g -> runModelSTM v (c . g))

instance Monad (ModelSTM n) where
    return  = pure
    m >>= k = ModelSTM $ \c -> runModelSTM m (\x -> runModelSTM (k x) c)

#if MIN_VERSION_base(4,13,0)
#else
    fail = Fail.fail
#endif

instance Fail.MonadFail (ModelSTM n) where
    fail e = ModelSTM $ \_ -> SThrow (MonadFailException e)

instance MonadThrow (ModelSTM n) where
  throwM e = ModelSTM $ \_ -> SThrow e

instance MonadCatch (ModelSTM n) where
  catch stm handler = ModelSTM $ SCatch handler stm

instance Alternative (ModelSTM n) where
  a <|> b = ModelSTM $ SOrElse a b
  empty = ModelSTM $ const SRetry

instance MonadPlus (ModelSTM n)

instance S.MonadSTM (ModelSTM n) where
  type TVar (ModelSTM n) = ModelTVar n

  newTVarN n = ModelSTM . SNew n

  readTVar = ModelSTM . SRead

  writeTVar tvar a = ModelSTM $ \c -> SWrite tvar a (c ())

--------------------------------------------------------------------------------
-- * Primitive actions

-- | STM transactions are represented as a sequence of primitive
-- actions.
data STMAction n
  = forall a e. Exception e => SCatch (e -> ModelSTM n a) (ModelSTM n a) (a -> STMAction n)
  | forall a. SRead  (ModelTVar n a) (a -> STMAction n)
  | forall a. SWrite (ModelTVar n a) a (STMAction n)
  | forall a. SOrElse (ModelSTM n a) (ModelSTM n a) (a -> STMAction n)
  | forall a. SNew String a (ModelTVar n a -> STMAction n)
  | forall e. Exception e => SThrow e
  | SRetry
  | SStop (n ())

--------------------------------------------------------------------------------
-- * @TVar@s

-- | A @TVar@ is modelled as a unique ID and a reference holding a
-- value.
data ModelTVar n a = ModelTVar
  { tvarId  :: TVarId
  , tvarRef :: Ref n a
  }

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
runTransaction :: MonadDejaFu n
  => ModelSTM n a
  -> IdSource
  -> n (Result a, n (), IdSource, [TAction])
runTransaction ma tvid = do
  (res, effect, _, tvid', trace) <- doTransaction ma tvid
  pure (res, effect, tvid', trace)

-- | Run a STM transaction, returning an action to undo its effects.
--
-- If the transaction fails, its effects will automatically be undone,
-- so the undo action returned will be @pure ()@.
doTransaction :: MonadDejaFu n
  => ModelSTM n a
  -> IdSource
  -> n (Result a, n (), n (), IdSource, [TAction])
doTransaction ma idsource = do
  (c, ref) <- runRefCont SStop (Just . Right) (runModelSTM ma)
  (idsource', effect, undo, readen, written, trace) <- go ref c (pure ()) (pure ()) idsource [] [] []
  res <- readRef ref

  case res of
    Just (Right val) -> pure (Success (nub readen) (nub written) val, effect, undo, idsource', reverse trace)
    Just (Left  exc) -> undo >> pure (Exception exc,      pure (), pure (), idsource, reverse trace)
    Nothing          -> undo >> pure (Retry $ nub readen, pure (), pure (), idsource, reverse trace)

  where
    go ref act effect undo nidsrc readen written sofar = do
      (act', effect', undo', nidsrc', readen', written', tact) <- stepTrans act nidsrc

      let newIDSource = nidsrc'
          newAct = act'
          newEffect = effect >> effect'
          newUndo = undo' >> undo
          newReaden = readen' ++ readen
          newWritten = written' ++ written
          newSofar = tact : sofar

      case tact of
        TStop  -> pure (newIDSource, newEffect, newUndo, newReaden, newWritten, TStop:newSofar)
        TRetry -> do
          writeRef ref Nothing
          pure (newIDSource, newEffect, newUndo, newReaden, newWritten, TRetry:newSofar)
        TThrow -> do
          writeRef ref (Just . Left $ case act of SThrow e -> toException e; _ -> undefined)
          pure (newIDSource, newEffect, newUndo, newReaden, newWritten, TThrow:newSofar)
        _ -> go ref newAct newEffect newUndo newIDSource newReaden newWritten newSofar

-- | Run a transaction for one step.
stepTrans :: MonadDejaFu n
  => STMAction n
  -> IdSource
  -> n (STMAction n, n (), n (), IdSource, [TVarId], [TVarId], TAction)
stepTrans act idsource = case act of
  SCatch  h stm c -> stepCatch h stm c
  SRead   ref c   -> stepRead ref c
  SWrite  ref a c -> stepWrite ref a c
  SNew    n a c   -> stepNew n a c
  SOrElse a b c   -> stepOrElse a b c
  SStop   na      -> stepStop na

  SThrow e -> pure (SThrow e, nothing, nothing, idsource, [], [], TThrow)
  SRetry   -> pure (SRetry,   nothing, nothing, idsource, [], [], TRetry)

  where
    nothing = pure ()

    stepCatch h stm c = cases TCatch stm c
      (\trace -> pure (SRetry, nothing, nothing, idsource, [], [], TCatch trace Nothing))
      (\trace exc    -> case fromException exc of
        Just exc' -> transaction (TCatch trace . Just) (h exc') c
        Nothing   -> pure (SThrow exc, nothing, nothing, idsource, [], [], TCatch trace Nothing))

    stepRead ModelTVar{..} c = do
      val <- readRef tvarRef
      pure (c val, nothing, nothing, idsource, [tvarId], [], TRead tvarId)

    stepWrite ModelTVar{..} a c = do
      old <- readRef tvarRef
      writeRef tvarRef a
      pure (c, writeRef tvarRef a, writeRef tvarRef old, idsource, [], [tvarId], TWrite tvarId)

    stepNew n a c = do
      let (idsource', tvid) = nextTVId n idsource
      ref <- newRef a
      let tvar = ModelTVar tvid ref
      pure (c tvar, writeRef ref a, nothing, idsource', [], [tvid], TNew tvid)

    stepOrElse a b c = cases TOrElse a c
      (\trace   -> transaction (TOrElse trace . Just) b c)
      (\trace exc -> pure (SThrow exc, nothing, nothing, idsource, [], [], TOrElse trace Nothing))

    stepStop na = do
      na
      pure (SStop na, nothing, nothing, idsource, [], [], TStop)

    cases tact stm onSuccess onRetry onException = do
      (res, effect, undo, idsource', trace) <- doTransaction stm idsource
      case res of
        Success readen written val -> pure (onSuccess val, effect, undo, idsource', readen, written, tact trace Nothing)
        Retry readen -> do
          (res', effect', undo', idsource'', readen', written', trace') <- onRetry trace
          pure (res', effect', undo', idsource'', readen ++ readen', written', trace')
        Exception exc -> onException trace exc

    transaction tact stm onSuccess = cases (\t _ -> tact t) stm onSuccess
      (\trace     -> pure (SRetry, nothing, nothing, idsource, [], [], tact trace))
      (\trace exc -> pure (SThrow exc, nothing, nothing, idsource, [], [], tact trace))

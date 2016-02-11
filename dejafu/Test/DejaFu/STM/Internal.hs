{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | 'MonadSTM' testing implementation, internal types and
-- definitions.
module Test.DejaFu.STM.Internal where

import Control.DeepSeq (NFData(..))
import Control.Exception (Exception, SomeException(..), fromException)
import Control.Monad.Cont (Cont, runCont)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Typeable (cast)
import Test.DejaFu.Internal

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable(..))
import Data.Monoid (mempty)
#endif

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
  | forall a. SNew a (CTVar r a -> STMAction n r)
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

-- | The unique ID of a 'CTVar'. Only meaningful within a single
-- concurrent computation.
type CTVarId = Int

--------------------------------------------------------------------------------
-- * Traces

-- | A trace of an STM transaction is just a list of actions that
-- occurred, as there are no scheduling decisions to make.
type TTrace = [TAction]

-- | All the actions that an STM transaction can perform.
data TAction =
    TNew
  -- ^ Create a new @CTVar@
  | TRead  CTVarId
  -- ^ Read from a @CTVar@.
  | TWrite CTVarId
  -- ^ Write to a @CTVar@.
  | TRetry
  -- ^ Abort and discard effects.
  | TOrElse TTrace (Maybe TTrace)
  -- ^ Execute a transaction until it succeeds (@STMStop@) or aborts
  -- (@STMRetry@) and, if it aborts, execute the other transaction.
  | TThrow
  -- ^ Throw an exception, abort, and discard effects.
  | TCatch TTrace (Maybe TTrace)
  -- ^ Execute a transaction until it succeeds (@STMStop@) or aborts
  -- (@STMThrow@). If the exception is of the appropriate type, it is
  -- handled and execution continues; otherwise aborts, propagating
  -- the exception upwards.
  | TStop
  -- ^ Terminate successfully and commit effects.
  | TLift
  -- ^ Lifts an action from the underlying monad. Note that the
  -- penultimate action in a trace will always be a @Lift@, this is an
  -- artefact of how the runner works.
  deriving (Eq, Show)

instance NFData TAction where
  rnf (TRead  v) = rnf v
  rnf (TWrite v) = rnf v
  rnf (TCatch  s m) = rnf (s, m)
  rnf (TOrElse s m) = rnf (s, m)
  rnf a = a `seq` ()

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
doTransaction :: Monad n => Fixed n r -> M n r a -> CTVarId -> n (Result a, n (), CTVarId, TTrace)
doTransaction fixed ma newctvid = do
  ref <- newRef fixed Nothing

  let c = runCont (ma >>= liftN fixed . writeRef fixed ref . Just . Right) $ const SStop

  (newctvid', undo, readen, written, trace) <- go ref c (return ()) newctvid [] [] []

  res <- readRef fixed ref

  case res of
    Just (Right val) -> return (Success (nub readen) (nub written) val, undo, newctvid', reverse trace)

    Just (Left  exc) -> undo >> return (Exception exc,      return (), newctvid, reverse trace)
    Nothing          -> undo >> return (Retry $ nub readen, return (), newctvid, reverse trace)

  where
    go ref act undo nctvid readen written sofar = do
      (act', undo', nctvid', readen', written', tact) <- stepTrans fixed act nctvid

      let newCTVid = nctvid'
          newAct = act'
          newUndo = undo >> undo'
          newReaden = readen' ++ readen
          newWritten = written' ++ written
          newSofar = tact : sofar

      case tact of
        TStop  -> return (newCTVid, newUndo, newReaden, newWritten, TStop:newSofar)
        TRetry -> writeRef fixed ref Nothing
          >> return (newCTVid, newUndo, newReaden, newWritten, TRetry:newSofar)
        TThrow -> writeRef fixed ref (Just . Left $ case act of SThrow e -> wrap e)
          >> return (newCTVid, newUndo, newReaden, newWritten, TThrow:newSofar)

        _ -> go ref newAct newUndo newCTVid newReaden newWritten newSofar

    -- | This wraps up an uncaught exception inside a @SomeException@,
    -- unless it already is a @SomeException@. This is because
    -- multiple levels of @SomeException@ do not play nicely with
    -- @fromException@.
    wrap e = fromMaybe (SomeException e) $ cast e

-- | Run a transaction for one step.
stepTrans :: Monad n => Fixed n r -> STMAction n r -> CTVarId -> n (STMAction n r, n (), CTVarId, [CTVarId], [CTVarId], TAction)
stepTrans fixed act newctvid = case act of
  SCatch  h stm c -> stepCatch h stm c
  SRead   ref c   -> stepRead ref c
  SWrite  ref a c -> stepWrite ref a c
  SNew    a c     -> stepNew a c
  SOrElse a b c   -> stepOrElse a b c
  SLift   na      -> stepLift na

  SThrow e -> return (SThrow e, nothing, newctvid, [], [], TThrow)
  SRetry   -> return (SRetry,   nothing, newctvid, [], [], TRetry)
  SStop    -> return (SStop,    nothing, newctvid, [], [], TStop)

  where
    nothing = return ()

    stepCatch h stm c = cases TCatch stm c
      (\trace readen -> return (SRetry, nothing, newctvid, readen, [], TCatch trace Nothing))
      (\trace exc    -> case fromException exc of
        Just exc' -> transaction (TCatch trace . Just) (h exc') c
        Nothing   -> return (SThrow exc, nothing, newctvid, [], [], TCatch trace Nothing))

    stepRead (CTVar (ctvid, ref)) c = do
      val <- readRef fixed ref
      return (c val, nothing, newctvid, [ctvid], [], TRead ctvid)

    stepWrite (CTVar (ctvid, ref)) a c = do
      old <- readRef fixed ref
      writeRef fixed ref a
      return (c, writeRef fixed ref old, newctvid, [], [ctvid], TWrite ctvid)

    stepNew a c = do
      let newctvid' = newctvid + 1
      ref <- newRef fixed a
      let ctvar = CTVar (newctvid, ref)
      return (c ctvar, nothing, newctvid', [], [newctvid], TNew)

    stepOrElse a b c = cases TOrElse a c
      (\trace _   -> transaction (TOrElse trace . Just) b c)
      (\trace exc -> return (SThrow exc, nothing, newctvid, [], [], TOrElse trace Nothing))

    stepLift na = do
      a <- na
      return (a, nothing, newctvid, [], [], TLift)

    cases tact stm onSuccess onRetry onException = do
      (res, undo, newctvid', trace) <- doTransaction fixed stm newctvid
      case res of
        Success readen written val -> return (onSuccess val, undo, newctvid', readen, written, tact trace Nothing)
        Retry readen  -> onRetry     trace readen
        Exception exc -> onException trace exc

    transaction tact stm onSuccess = cases (\t _ -> tact t) stm onSuccess
      (\trace readen -> return (SRetry, nothing, newctvid, readen, [], tact trace))
      (\trace exc    -> return (SThrow exc, nothing, newctvid, [], [], tact trace))

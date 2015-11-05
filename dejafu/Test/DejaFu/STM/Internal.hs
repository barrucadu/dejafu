{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | 'MonadSTM' testing implementation, internal types and
-- definitions.
module Test.DejaFu.STM.Internal where

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
type M t n r a = Cont (STMAction t n r) a

-- | Dict of methods for implementations to override.
type Fixed t n r = Ref n r (Cont (STMAction t n r))

--------------------------------------------------------------------------------
-- * Primitive actions

-- | STM transactions are represented as a sequence of primitive
-- actions.
data STMAction t n r
  = forall a e. Exception e => SCatch (e -> M t n r a) (M t n r a) (a -> STMAction t n r)
  | forall a. SRead  (CTVar t r a) (a -> STMAction t n r)
  | forall a. SWrite (CTVar t r a) a (STMAction t n r)
  | forall a. SOrElse (M t n r a) (M t n r a) (a -> STMAction t n r)
  | forall a. SNew a (CTVar t r a -> STMAction t n r)
  | SLift (n (STMAction t n r))
  | forall e. Exception e => SThrow e
  | SRetry
  | SStop

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

  let c = runCont (ma >>= liftN fixed . writeRef fixed ref . Just . Right) $ const SStop

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
        SStop  -> return ret
        SRetry -> writeRef fixed ref Nothing >> return ret
        SThrow exc -> writeRef fixed ref (Just . Left $ wrap exc) >> return ret

        _ -> go ref act' (undo >> undo') nctvid' (readen' ++ readen) (written' ++ written)

    -- | This wraps up an uncaught exception inside a @SomeException@,
    -- unless it already is a @SomeException@. This is because
    -- multiple levels of @SomeException@ do not play nicely with
    -- @fromException@.
    wrap e = fromMaybe (SomeException e) $ cast e

-- | Run a transaction for one step.
stepTrans :: forall t n r. Monad n => Fixed t n r -> STMAction t n r -> CTVarId -> n (STMAction t n r, n (), CTVarId, [CTVarId], [CTVarId])
stepTrans fixed act newctvid = case act of
  SCatch  h stm c -> stepCatch h stm c
  SRead   ref c   -> stepRead ref c
  SWrite  ref a c -> stepWrite ref a c
  SNew    a c     -> stepNew a c
  SOrElse a b c   -> stepOrElse a b c
  SLift   na      -> stepLift na

  halt -> return (halt, nothing, newctvid, [], [])

  where
    nothing = return ()

    stepCatch h stm c = onFailure stm c
      (\readen -> return (SRetry, nothing, newctvid, readen, []))
      (\exc    -> case fromException exc of
        Just exc' -> transaction (h exc') c
        Nothing   -> return (SThrow exc, nothing, newctvid, [], []))

    stepRead (V (ctvid, ref)) c = do
      val <- readRef fixed ref
      return (c val, nothing, newctvid, [ctvid], [])

    stepWrite (V (ctvid, ref)) a c = do
      old <- readRef fixed ref
      writeRef fixed ref a
      return (c, writeRef fixed ref old, newctvid, [], [ctvid])

    stepNew a c = do
      let newctvid' = newctvid + 1
      ref <- newRef fixed a
      let ctvar = V (newctvid, ref)
      return (c ctvar, nothing, newctvid', [], [newctvid])

    stepOrElse a b c = onFailure a c
      (\_   -> transaction b c)
      (\exc -> return (SThrow exc, nothing, newctvid, [], []))

    stepLift na = do
      a <- na
      return (a, nothing, newctvid, [], [])

    onFailure stm onSuccess onRetry onException = do
      (res, undo, newctvid') <- doTransaction fixed stm newctvid
      case res of
        Success readen written val -> return (onSuccess val, undo, newctvid', readen, written)
        Retry readen  -> onRetry readen
        Exception exc -> onException exc

    transaction stm onSuccess = onFailure stm onSuccess
      (\readen -> return (SRetry, nothing, newctvid, readen, []))
      (\exc    -> return (SThrow exc, nothing, newctvid, [], []))

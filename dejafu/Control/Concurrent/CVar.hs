-- | Combinators using @CVar@s. These provide many of the helpful
-- functions found in Control.Concurrent.MVar, but for @CVar@s.
module Control.Concurrent.CVar
 ( -- *@CVar@s
  CVar
 , newEmptyCVar
 , newEmptyCVarN
 , newCVar
 , newCVarN
 , takeCVar
 , putCVar
 , readCVar
 , swapCVar
 , tryTakeCVar
 , tryPutCVar
 , isEmptyCVar
 , withCVar
 , withCVarMasked
 , modifyCVar_
 , modifyCVar
 , modifyCVarMasked_
 , modifyCVarMasked

 -- * Binary semaphores
 -- | A common use of @CVar@s is in making binary semaphores to
 -- control mutual exclusion over a resource, so a couple of helper
 -- functions are provided.
 , lock
 , unlock
 ) where

import Control.Monad.Catch (mask_, onException)
import Control.Monad.Conc.Class

-- | Swap the contents of a @CVar@, and return the value taken. This
-- function is atomic only if there are no other producers fro this
-- @CVar@.
swapCVar :: MonadConc m => CVar m a -> a -> m a
swapCVar cvar a = mask_ $ do
  old <- takeCVar cvar
  putCVar cvar a
  return old

-- | Check if a @CVar@ is empty.
isEmptyCVar :: MonadConc m => CVar m a -> m Bool
isEmptyCVar cvar = do
  val <- tryTakeCVar cvar
  case val of
    Just val' -> putCVar cvar val' >> return True
    Nothing   -> return False

-- | Operate on the contents of a @CVar@, replacing the contents after
-- finishing. This operation is exception-safe: it will replace the
-- original contents of the @CVar@ if an exception is raised. However,
-- it is only atomic if there are no other producers for this @CVar@.
{-# INLINE withCVar #-}
withCVar :: MonadConc m => CVar m a -> (a -> m b) -> m b
withCVar cvar f = mask $ \restore -> do
  val <- takeCVar cvar
  out <- restore (f val) `onException` putCVar cvar val
  putCVar cvar val

  return out

-- | Like 'withCVar', but the @IO@ action in the second argument is
-- executed with asynchronous exceptions masked.
{-# INLINE withCVarMasked #-}
withCVarMasked :: MonadConc m => CVar m a -> (a -> m b) -> m b
withCVarMasked cvar f = mask_ $ do
  val <- takeCVar cvar
  out <- f val `onException` putCVar cvar val
  putCVar cvar val

  return out

-- | An exception-safe wrapper for modifying the contents of a @CVar@.
-- Like 'withCVar', 'modifyCVar' will replace the original contents of
-- the @CVar@ if an exception is raised during the operation. This
-- function is only atomic if there are no other producers for this
-- @CVar@.
{-# INLINE modifyCVar_ #-}
modifyCVar_ :: MonadConc m => CVar m a -> (a -> m a) -> m ()
modifyCVar_ cvar f = modifyCVar cvar $ fmap (\a -> (a,())) . f

-- | A slight variation on 'modifyCVar_' that allows a value to be
-- returned (@b@) in addition to the modified value of the @CVar@.
{-# INLINE modifyCVar #-}
modifyCVar :: MonadConc m => CVar m a -> (a -> m (a, b)) -> m b
modifyCVar cvar f = mask $ \restore -> do
  val <- takeCVar cvar
  (val', out) <- restore (f val) `onException` putCVar cvar val
  putCVar cvar val'
  return out

-- | Like 'modifyCVar_', but the @IO@ action in the second argument is
-- executed with asynchronous exceptions masked.
{-# INLINE modifyCVarMasked_ #-}
modifyCVarMasked_ :: MonadConc m => CVar m a -> (a -> m a) -> m ()
modifyCVarMasked_ cvar f = modifyCVarMasked cvar $ fmap (\a -> (a,())) . f

-- | Like 'modifyCVar', but the @IO@ action in the second argument is
-- executed with asynchronous exceptions masked.
{-# INLINE modifyCVarMasked #-}
modifyCVarMasked :: MonadConc m => CVar m a -> (a -> m (a, b)) -> m b
modifyCVarMasked cvar f = mask_ $ do
  val <- takeCVar cvar
  (val', out) <- f val `onException` putCVar cvar val
  putCVar cvar val'
  return out

-- | Put a @()@ into a @CVar@, claiming the lock. This is atomic.
lock :: MonadConc m => CVar m () -> m ()
lock = flip putCVar ()

-- | Empty a @CVar@, releasing the lock. This is atomic.
unlock :: MonadConc m => CVar m () -> m ()
unlock = takeCVar

-- | Combinators using @CVar@s. These provide many of the helpful
-- functions found in Control.Concurrent.MVar, but for @CVar@s. Note
-- that these do not in general mask exceptions, and are not atomic,
-- being implemented in terms of the primitives in the 'MonadConc'
-- typeclass.
module Control.Concurrent.CVar
 ( -- *@CVar@s
  CVar
 , newEmptyCVar
 , newCVar
 , takeCVar
 , putCVar
 , readCVar
 , swapCVar
 , tryTakeCVar
 , tryPutCVar
 , isEmptyCVar
 , withCVar
 , modifyCVar_
 , modifyCVar

 -- * Binary semaphores
 -- | A common use of @CVar@s is in making binary semaphores to
 -- control mutual exclusion over a resource, so a couple of helper
 -- functions are provided.
 , lock
 , unlock
 ) where

import Control.Monad (liftM)
import Control.Monad.Conc.Class

-- | Create a new @CVar@ containing a value.
newCVar :: MonadConc m => a -> m (CVar m a)
newCVar a = do
  cvar <- newEmptyCVar
  putCVar cvar a
  return cvar

-- | Swap the contents of a @CVar@, and return the value taken.
swapCVar :: MonadConc m => CVar m a -> a -> m a
swapCVar cvar a = do
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
-- finishing.
withCVar :: MonadConc m => CVar m a -> (a -> m b) -> m b
withCVar cvar f = do
  val <- takeCVar cvar
  out <- f val
  putCVar cvar val

  return out

-- | Apply a function to the value inside a @CVar@, and also return a
-- value.
modifyCVar :: MonadConc m => CVar m a -> (a -> m (a, b)) -> m b
modifyCVar cvar f = do
  val <- takeCVar cvar
  (val', out) <- f val
  putCVar cvar val'
  return out

-- | Modify the contents of a @CVar@.
modifyCVar_ :: MonadConc m => CVar m a -> (a -> m a) -> m ()
modifyCVar_ cvar f = modifyCVar cvar $ \a -> (\b -> (b, ())) `liftM` f a

-- | Put a @()@ into a @CVar@, claiming the lock. This is atomic.
lock :: MonadConc m => CVar m () -> m ()
lock = flip putCVar ()

-- | Empty a @CVar@, releasing the lock. This is atomic.
unlock :: MonadConc m => CVar m () -> m ()
unlock = takeCVar

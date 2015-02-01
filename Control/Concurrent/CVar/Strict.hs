-- | Strict alternatives to the functions in
-- Control.Monad.Conc.CVar. Specifically, values are evaluated to
-- normal form before being put into a @CVar@.
module Control.Concurrent.CVar.Strict
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

import Control.Concurrent.CVar (isEmptyCVar, withCVar, lock, unlock)
import Control.DeepSeq (NFData, force)
import Control.Monad (liftM)
import Control.Monad.Conc.Class hiding (newEmptyCVar, putCVar, tryPutCVar)

import qualified Control.Concurrent.CVar  as V
import qualified Control.Monad.Conc.Class as C

-- | Create a new empty @CVar@.
newEmptyCVar :: (MonadConc m, NFData a) => m (CVar m a)
newEmptyCVar = C.newEmptyCVar

-- | Create a new @CVar@ containing a value.
newCVar :: (MonadConc m, NFData a) => a -> m (CVar m a)
newCVar = V.newCVar . force

-- | Swap the contents of a @CVar@, and return the value taken.
swapCVar :: (MonadConc m, NFData a) => CVar m a -> a -> m a
swapCVar cvar = V.swapCVar cvar . force

-- | Apply a function to the value inside a @CVar@, and also return a
-- value.
modifyCVar :: (MonadConc m, NFData a) => CVar m a -> (a -> m (a, b)) -> m b
modifyCVar cvar f = do
  val <- takeCVar cvar
  (val', out) <- f val
  putCVar cvar val'
  return out

-- | Modify the contents of a @CVar@.
modifyCVar_ :: (MonadConc m, NFData a) => CVar m a -> (a -> m a) -> m ()
modifyCVar_ cvar f = modifyCVar cvar $ \a -> (\b -> (b, ())) `liftM` f a

-- | Put a value into a @CVar@. If there is already a value there,
-- this will block until that value has been taken, at which point the
-- value will be stored.
putCVar :: (MonadConc m, NFData a) => CVar m a -> a -> m ()
putCVar cvar = C.putCVar cvar . force

-- | Attempt to put a value in a @CVar@, returning 'True' (and filling
-- the @CVar@) if there was nothing there, otherwise returning
-- 'False'.
tryPutCVar :: (MonadConc m, NFData a) => CVar m a -> a -> m Bool
tryPutCVar cvar = C.tryPutCVar cvar . force

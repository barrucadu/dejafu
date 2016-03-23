-- | Strict alternatives to the functions in
-- Control.Concurrent.Classy.MVar. Specifically, values are evaluated
-- to normal form before being put into a @MVar@.
module Control.Concurrent.Classy.MVar.Strict
 ( -- *@MVar@s
  MVar
 , newEmptyMVar
 , newEmptyMVarN
 , newMVar
 , newMVarN
 , takeMVar
 , putMVar
 , readMVar
 , swapMVar
 , tryTakeMVar
 , tryPutMVar
 , isEmptyMVar
 , withMVar
 , withMVarMasked
 , modifyMVar_
 , modifyMVar
 , modifyMVarMasked_
 , modifyMVarMasked

 -- * Binary semaphores
 -- | A common use of @MVar@s is in making binary semaphores to
 -- control mutual exclusion over a resource, so a couple of helper
 -- functions are provided.
 , lock
 , unlock
 ) where

import Control.Concurrent.Classy.MVar (isEmptyMVar, withMVar, withMVarMasked, lock, unlock)
import Control.DeepSeq (NFData, force)
import Control.Monad.Catch (mask_, onException)
import Control.Monad.Conc.Class hiding (newEmptyMVar, newEmptyMVarN, newMVar, newMVarN, putMVar, tryPutMVar)

import qualified Control.Concurrent.Classy.MVar as V
import qualified Control.Monad.Conc.Class as C

-- | Create a new empty @MVar@.
newEmptyMVar :: (MonadConc m, NFData a) => m (MVar m a)
newEmptyMVar = C.newEmptyMVar

-- | Create a new empty @MVar@, but it is given a name which may be
-- used to present more useful debugging information.
--
-- If no name is given, a counter starting from 0 is used. If names
-- conflict, successive @MVar@s with the same name are given a numeric
-- suffix, counting up from 1.
newEmptyMVarN :: (MonadConc m, NFData a) => String -> m (MVar m a)
newEmptyMVarN n = C.newEmptyMVarN (force n)

-- | Create a new @MVar@ containing a value.
newMVar :: (MonadConc m, NFData a) => a -> m (MVar m a)
newMVar = C.newMVar . force

-- | Create a new @MVar@ containing a value, but it is given a name
-- which may be used to present more useful debugging information.
--
-- If no name is given, a counter starting from 0 is used. If names
-- conflict, successive @MVar@s with the same name are given a numeric
-- suffix, counting up from 1.
newMVarN :: (MonadConc m, NFData a) => String -> a -> m (MVar m a)
newMVarN n = C.newMVarN (force n) . force

-- | Swap the contents of a @MVar@, and return the value taken.
swapMVar :: (MonadConc m, NFData a) => MVar m a -> a -> m a
swapMVar cvar = V.swapMVar cvar . force

-- | Put a value into a @MVar@. If there is already a value there,
-- this will block until that value has been taken, at which point the
-- value will be stored.
putMVar :: (MonadConc m, NFData a) => MVar m a -> a -> m ()
putMVar cvar = C.putMVar cvar . force

-- | Attempt to put a value in a @MVar@, returning 'True' (and filling
-- the @MVar@) if there was nothing there, otherwise returning
-- 'False'.
tryPutMVar :: (MonadConc m, NFData a) => MVar m a -> a -> m Bool
tryPutMVar cvar = C.tryPutMVar cvar . force

-- | An exception-safe wrapper for modifying the contents of a @MVar@.
-- Like 'withMVar', 'modifyMVar' will replace the original contents of
-- the @MVar@ if an exception is raised during the operation. This
-- function is only atomic if there are no other producers for this
-- @MVar@.
{-# INLINE modifyMVar_ #-}
modifyMVar_ :: (MonadConc m, NFData a) => MVar m a -> (a -> m a) -> m ()
modifyMVar_ cvar f = modifyMVar cvar $ fmap (\a -> (a,())) . f

-- | A slight variation on 'modifyMVar_' that allows a value to be
-- returned (@b@) in addition to the modified value of the @MVar@.
{-# INLINE modifyMVar #-}
modifyMVar :: (MonadConc m, NFData a) => MVar m a -> (a -> m (a, b)) -> m b
modifyMVar cvar f = mask $ \restore -> do
  val <- takeMVar cvar
  (val', out) <- restore (f val) `onException` putMVar cvar val
  putMVar cvar val'
  return out

-- | Like 'modifyMVar_', but the @IO@ action in the second argument is
-- executed with asynchronous exceptions masked.
{-# INLINE modifyMVarMasked_ #-}
modifyMVarMasked_ :: (MonadConc m, NFData a) => MVar m a -> (a -> m a) -> m ()
modifyMVarMasked_ cvar f = modifyMVarMasked cvar $ fmap (\a -> (a,())) . f

-- | Like 'modifyMVar', but the @IO@ action in the second argument is
-- executed with asynchronous exceptions masked.
{-# INLINE modifyMVarMasked #-}
modifyMVarMasked :: (MonadConc m, NFData a) => MVar m a -> (a -> m (a, b)) -> m b
modifyMVarMasked cvar f = mask_ $ do
  val <- takeMVar cvar
  (val', out) <- f val `onException` putMVar cvar val
  putMVar cvar val'
  return out

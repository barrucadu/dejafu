{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- |The monad-conc package provides a class of monads which implement
-- concurrency with (from the perspective of the programmer)
-- nondeterministic interleaving.
--
-- The major difference between this and the Par family of monads (in
-- monad-par) is that results may be implicitly dependent on
-- interleaving, giving rise to data races between threads, whereas
-- Par was explicitly designed to give rise to deterministic
-- concurrency.
module Control.Monad.Conc.Class where

import Prelude hiding (take)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, readMVar, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Monad (void)
import Data.Maybe (maybe)

-- | @ConcFuture@ is the monad-conc alternative of 'ParFuture'. It
-- abstracts Conc monads which support futures. In itself, this is not
-- enough to implement nondeterminism, however the class is provided
-- to remove the 'NFData' constraints imposed by 'ParFuture'.
class Monad m => ConcFuture future m | m -> future where
  -- | Create a concurrent computation for the provided action, and
  -- return a future which can be used to query the result.
  spawn :: m a -> m (future a)

  -- | Block until a value is present in the future, and then return
  -- it. This does not \"remove\" the value from the future, multiple
  -- 'get's are possible, unlike 'takeMVar' for example.
  get :: future a -> m a

instance ConcFuture MVar IO where
  spawn ma = do
    mvar <- newEmptyMVar
    void . forkIO $ ma >>= putMVar mvar
    return mvar

  get = readMVar

-- | @ConcCVar@ builds on futures by allowing `CVar`s which threads
-- can read from and write to, possibly multiple times. This is the
-- key difference with the `Par` monads, where it is illegal to write
-- multiple times to the same `IVar`, which removes the possibility of
-- data races.
--
-- A minimal implementation consists of 'fork', 'new', 'put', and
-- 'tryTake'.
class ConcFuture cvar m => ConcCVar cvar m | m -> cvar where
  -- | Forks a computation to happen concurrently. Communication may
  -- happen over `CVar`s.
  fork :: m () -> m ()

  -- | Creates a new empty `CVar`.
  new :: m (cvar a)

  -- | Put a value into a `CVar`. If there is already a value there,
  -- this will block until that value has been 'take'n, at which point
  -- the value will be stored.
  put :: cvar a -> a -> m ()

  -- | Take a value from a `CVar`. This \"empties\" the `CVar`,
  -- allowing a new value to be 'put' in. This will block if there is
  -- no value in the `CVar` already, until one has been 'put'.
  --
  -- > take cvar = tryTake cvar >>= maybe (take cvar) return
  --
  -- The default implementation is very inefficient, and so should
  -- probably be always overridden to use some implementation-specific
  -- blocking functionality.
  take :: cvar a -> m a
  take cvar = tryTake cvar >>= maybe (take cvar) return

  -- | Attempt to take a value from a `CVar`, returning `Just value`
  -- (and emptying the `CVar`) if there was something there, otherwise
  -- returning `Nothing`.
  tryTake :: cvar a -> m (Maybe a)

instance ConcCVar MVar IO where
  fork    = void . forkIO
  new     = newEmptyMVar
  put     = putMVar
  take    = takeMVar
  tryTake = tryTakeMVar

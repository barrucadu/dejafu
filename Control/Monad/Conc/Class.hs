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

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, readMVar, newEmptyMVar, putMVar, tryPutMVar, takeMVar, tryTakeMVar)
import Control.Monad (void)

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
  readCVar :: future a -> m a

instance ConcFuture MVar IO where
  spawn ma = do
    cvar <- newEmptyCVar
    fork $ ma >>= putCVar cvar
    return cvar

  readCVar = readMVar

-- | @ConcCVar@ builds on futures by allowing `CVar`s which threads
-- can read from and write to, possibly multiple times. This is the
-- key difference with the `Par` monads, where it is illegal to write
-- multiple times to the same `IVar`, which removes the possibility of
-- data races.
--
-- A minimal implementation consists of 'fork', 'newCVar',
-- 'tryPutCVar', and 'tryTakeCVar'. The default implementations of
-- 'takeCVar' and 'putCVar', however, are very inefficient, and should
-- probably always be overridden to make use of
-- implementation-specific blocking functionality.
class ConcFuture cvar m => ConcCVar cvar m | m -> cvar where
  -- | Forks a computation to happen concurrently. Communication may
  -- happen over `CVar`s.
  fork :: m () -> m ()

  -- | Creates a new empty `CVar`.
  newEmptyCVar :: m (cvar a)

  -- | Put a value into a `CVar`. If there is already a value there,
  -- this will block until that value has been 'take'n, at which point
  -- the value will be stored.
  --
  -- > putCVar cvar a = tryPutCVar cvar a >>= \b -> if b then return () else putCVar cvar a
  putCVar :: cvar a -> a -> m ()
  putCVar cvar a = tryPutCVar cvar a >>= \b -> if b then return () else putCVar cvar a

  -- | Attempt to put a value in a `CVar`, returning `True` (and
  -- filling the `CVar`) if there was nothing there, otherwise
  -- returning `False`
  tryPutCVar :: cvar a -> a -> m Bool

  -- | Take a value from a `CVar`. This \"empties\" the `CVar`,
  -- allowing a new value to be 'put' in. This will block if there is
  -- no value in the `CVar` already, until one has been 'put'.
  --
  -- > takeCVar cvar = tryTakeCVar cvar >>= maybe (takeCVar cvar) return
  takeCVar :: cvar a -> m a
  takeCVar cvar = tryTakeCVar cvar >>= maybe (takeCVar cvar) return

  -- | Attempt to take a value from a `CVar`, returning `Just value`
  -- (and emptying the `CVar`) if there was something there, otherwise
  -- returning `Nothing`.
  tryTakeCVar :: cvar a -> m (Maybe a)

instance ConcCVar MVar IO where
  fork         = void . forkIO
  newEmptyCVar = newEmptyMVar
  putCVar      = putMVar
  tryPutCVar   = tryPutMVar
  takeCVar     = takeMVar
  tryTakeCVar  = tryTakeMVar

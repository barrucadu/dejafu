{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | This module captures the interface of @Conc@ monads in two
-- typeclasses, one merely providing the ability to spawn new threads,
-- and the other providing full @CVar@s. All @Conc@ monads implement
-- the former, and to provide for nondeterminism also need to
-- implement the latter.
module Control.Monad.Conc.Class where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, readMVar, newEmptyMVar, putMVar, tryPutMVar, takeMVar, tryTakeMVar)
import Control.Monad (unless, void)

-- | @ConcFuture@ is the monad-conc alternative of 'ParFuture'. It
-- abstracts Conc monads which support futures. In itself, this is not
-- enough to implement nondeterminism, however the class is provided
-- to remove the 'NFData' constraints imposed by 'ParFuture'.
class Monad m => ConcFuture future m | m -> future where
  -- | Create a concurrent computation for the provided action, and
  -- return a future which can be used to query the result.
  --
  -- For monads which also implement 'ConcCVar', it is expected to
  -- implement 'spawn' in terms of 'newEmptyCVar', 'fork', and
  -- 'putCVar'. The 'defaultSpawn' function provides this.
  --
  -- > spawn ma = do
  -- >   cvar <- newEmptyCVar
  -- >   fork $ ma >>= putCVar cvar
  -- >   return cvar
  spawn :: m a -> m (future a)

  -- | Block until a value is present in the future, and then return
  -- it. As with 'readMVar', this does not \"remove\" the value from
  -- the future, multiple reads are possible.
  readCVar :: future a -> m a

instance ConcFuture MVar IO where
  spawn    = defaultSpawn
  readCVar = readMVar

-- | @ConcCVar@ builds on futures by allowing @CVar@s which threads
-- can read from and write to, possibly multiple times. This is the
-- key difference with the @Par@ monads, where it is illegal to write
-- multiple times to the same @IVar@, which removes the possibility of
-- data races.
--
-- A minimal implementation consists of 'fork', 'newEmptyCVar',
-- 'tryPutCVar', and 'tryTakeCVar'. The default implementations of
-- 'takeCVar' and 'putCVar', however, are very inefficient, and should
-- probably always be overridden to make use of
-- implementation-specific blocking functionality.
class ConcFuture cvar m => ConcCVar cvar m | m -> cvar where
  -- | Fork a computation to happen concurrently. Communication may
  -- happen over @CVar@s.
  fork :: m () -> m ()

  -- | Create a new empty @CVar@.
  newEmptyCVar :: m (cvar a)

  -- | Put a value into a @CVar@. If there is already a value there,
  -- this will block until that value has been taken, at which point
  -- the value will be stored.
  --
  -- > putCVar cvar a = tryPutCVar cvar a >>= \b -> unless b $ putCVar cvar a
  putCVar :: cvar a -> a -> m ()
  putCVar cvar a = tryPutCVar cvar a >>= \b -> unless b $ putCVar cvar a

  -- | Attempt to put a value in a @CVar@, returning 'True' (and
  -- filling the @CVar@) if there was nothing there, otherwise
  -- returning 'False'.
  tryPutCVar :: cvar a -> a -> m Bool

  -- | Take a value from a @CVar@. This \"empties\" the @CVar@,
  -- allowing a new value to be put in. This will block if there is no
  -- value in the @CVar@ already, until one has been put.
  --
  -- > takeCVar cvar = tryTakeCVar cvar >>= maybe (takeCVar cvar) return
  takeCVar :: cvar a -> m a
  takeCVar cvar = tryTakeCVar cvar >>= maybe (takeCVar cvar) return

  -- | Attempt to take a value from a @CVar@, returning a 'Just' (and
  -- emptying the @CVar@) if there was something there, otherwise
  -- returning 'Nothing'.
  tryTakeCVar :: cvar a -> m (Maybe a)

instance ConcCVar MVar IO where
  fork         = void . forkIO
  newEmptyCVar = newEmptyMVar
  putCVar      = putMVar
  tryPutCVar   = tryPutMVar
  takeCVar     = takeMVar
  tryTakeCVar  = tryTakeMVar

-- | A default implementation of 'spawn' for 'ConcCVar' monads.
defaultSpawn :: ConcCVar cvar m => m a -> m (cvar a)
defaultSpawn ma = do
  cvar <- newEmptyCVar
  fork $ ma >>= putCVar cvar
  return cvar

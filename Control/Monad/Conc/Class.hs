{-# LANGUAGE TypeFamilies #-}

-- | This module captures in a typeclass the interface of concurrency
-- monads.
module Control.Monad.Conc.Class where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, readMVar, newEmptyMVar, putMVar, tryPutMVar, takeMVar, tryTakeMVar)
import Control.Monad (unless, void)

-- | @MonadConc@ is like a combination of 'ParFuture' and 'ParIVar'
-- from the abstract-par package. It captures the interface of
-- concurrency monads in terms of how they can operate on shared
-- state.
--
-- There are a few notable differences: firstly, @Par@ imposes
-- 'NFData' constraints on everything, as it achieves its speed-up by
-- forcing evaluation in separate threads. @MonadConc@ doesn't do
-- that, and so you need to be careful about where evaluation occurs,
-- just like with 'MVar's. Secondly, this builds on futures by
-- allowing @CVar@s which threads can read from and write to, possibly
-- multiple times, whereas with the @Par@ monads it is illegal to
-- write multiple times to the same @IVar@ (or to non-blockingly read
-- from it), which removes the possibility of data races.
--
-- A minimal implementation consists of 'fork', 'newEmptyCVar',
-- 'tryPutCVar', and 'tryTakeCVar'. The default implementations of
-- 'takeCVar' and 'putCVar', however, are very inefficient, and should
-- probably always be overridden to make use of
-- implementation-specific blocking functionality.
class Monad m => MonadConc m where
  -- | The mutable reference type. This may contain one value at a
  -- time, attempting to read or take from an \"empty\" @CVar@ will
  -- block until it is full, and attempting to put to a \"full\"
  -- @CVar@ will block until it is empty.
  type CVar m :: * -> *

  -- | Fork a computation to happen concurrently. Communication may
  -- happen over @CVar@s.
  fork :: m () -> m ()

  -- | Create a concurrent computation for the provided action, and
  -- return a @CVar@ which can be used to query the result.
  --
  -- > spawn ma = do
  -- >   cvar <- newEmptyCVar
  -- >   fork $ ma >>= putCVar cvar
  -- >   return cvar
  spawn :: m a -> m (CVar m a)
  spawn ma = do
    cvar <- newEmptyCVar
    fork $ ma >>= putCVar cvar
    return cvar

  -- | Create a new empty @CVar@.
  newEmptyCVar :: m (CVar m a)

  -- | Put a value into a @CVar@. If there is already a value there,
  -- this will block until that value has been taken, at which point
  -- the value will be stored.
  --
  -- > putCVar cvar a = tryPutCVar cvar a >>= \b -> unless b $ putCVar cvar a
  putCVar :: CVar m a -> a -> m ()
  putCVar cvar a = tryPutCVar cvar a >>= \b -> unless b $ putCVar cvar a

  -- | Attempt to put a value in a @CVar@, returning 'True' (and
  -- filling the @CVar@) if there was nothing there, otherwise
  -- returning 'False'.
  tryPutCVar :: CVar m a -> a -> m Bool

  -- | Block until a value is present in the @CVar@, and then return
  -- it. As with 'readMVar', this does not \"remove\" the value,
  -- multiple reads are possible.
  readCVar :: CVar m a -> m a

  -- | Take a value from a @CVar@. This \"empties\" the @CVar@,
  -- allowing a new value to be put in. This will block if there is no
  -- value in the @CVar@ already, until one has been put.
  --
  -- > takeCVar cvar = tryTakeCVar cvar >>= maybe (takeCVar cvar) return
  takeCVar :: CVar m a -> m a
  takeCVar cvar = tryTakeCVar cvar >>= maybe (takeCVar cvar) return

  -- | Attempt to take a value from a @CVar@, returning a 'Just' (and
  -- emptying the @CVar@) if there was something there, otherwise
  -- returning 'Nothing'.
  tryTakeCVar :: CVar m a -> m (Maybe a)

instance MonadConc IO where
  type CVar IO = MVar

  readCVar     = readMVar
  fork         = void . forkIO
  newEmptyCVar = newEmptyMVar
  putCVar      = putMVar
  tryPutCVar   = tryPutMVar
  takeCVar     = takeMVar
  tryTakeCVar  = tryTakeMVar

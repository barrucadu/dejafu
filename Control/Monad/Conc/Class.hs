{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | This module captures in a typeclass the interface of concurrency
-- monads.
module Control.Monad.Conc.Class where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, readMVar, newEmptyMVar, putMVar, tryPutMVar, takeMVar, tryTakeMVar)
import Control.Exception (Exception)
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.STM (STM)
import Control.Monad.STM.Class (MonadSTM)

import qualified Control.Monad.STM as S

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
-- Every @MonadConc@ has an associated 'MonadSTM', transactions of
-- which can be run atomically.
--
-- A minimal implementation consists of 'fork', 'newEmptyCVar',
-- 'tryPutCVar', and 'tryTakeCVar'. The default implementations of
-- 'takeCVar' and 'putCVar', however, are very inefficient, and should
-- probably always be overridden to make use of
-- implementation-specific blocking functionality.
class (Monad m, MonadCatch m, MonadSTM (STMLike m), MonadThrow m) => MonadConc m  where
  -- | The associated 'MonadSTM' for this class.
  type STMLike m :: * -> *

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

  -- | Perform a series of STM actions atomically.
  atomically :: STMLike m a -> m a

  -- | Throw an exception. This will \"bubble up\" looking for an
  -- exception handler capable of dealing with it and, if one is not
  -- found, the thread is killed.
  --
  -- > throw = throwM
  throw :: Exception e => e -> m a
  throw = throwM

  -- | Catch an exception. This is only required to be able to catch
  -- exceptions raised by 'throw', unlike the more general
  -- Control.Exception.catch function. If you need to be able to catch
  -- /all/ errors, you will have to use 'IO'.
  catch :: Exception e => m a -> (e -> m a) -> m a
  catch = Control.Monad.Catch.catch

  -- | Runs its argument, just as if the @_concNoTest@ weren't there.
  --
  -- > _concNoTest x = x
  --
  -- This function is purely for testing purposes, and indicates that
  -- it's not worth considering more than one schedule here. This is
  -- useful if you have some larger computation built up out of
  -- subcomputations which you have already got tests for: you only
  -- want to consider what's unique to the large component.
  --
  -- The test runner will report a failure if the argument fails.
  --
  -- Note that inappropriate use of @_concNoTest@ can actually
  -- /suppress/ bugs! For this reason it is recommended to use it only
  -- for things which don't make use of any state from a larger
  -- scope. As a rule-of-thumb: if you can't define it as a top-level
  -- function taking no @CVar@ arguments, you probably shouldn't
  -- @_concNoTest@ it.
  _concNoTest :: m a -> m a
  _concNoTest = id

instance MonadConc IO where
  type STMLike IO = STM
  type CVar    IO = MVar

  readCVar     = readMVar
  fork         = void . forkIO
  newEmptyCVar = newEmptyMVar
  putCVar      = putMVar
  tryPutCVar   = tryPutMVar
  takeCVar     = takeMVar
  tryTakeCVar  = tryTakeMVar
  atomically   = S.atomically

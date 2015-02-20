{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

-- | This module captures in a typeclass the interface of concurrency
-- monads.
module Control.Monad.Conc.Class
  ( MonadConc(..)
  -- * Utilities
  , spawn
  , forkFinally
  , killThread
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, readMVar, newEmptyMVar, putMVar, tryPutMVar, takeMVar, tryTakeMVar)
import Control.Exception (Exception, AsyncException(ThreadKilled), SomeException)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Control.Monad.STM (STM)
import Control.Monad.STM.Class (MonadSTM)
import Data.IORef (IORef, atomicModifyIORef, atomicWriteIORef, newIORef, readIORef)

import qualified Control.Monad.Catch as Ca
import qualified Control.Concurrent as C
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
class ( Monad m, MonadCatch m, MonadThrow m, MonadMask m
      , MonadSTM (STMLike m)
      , Eq (ThreadId m), Show (ThreadId m)) => MonadConc m  where
  -- | The associated 'MonadSTM' for this class.
  type STMLike m :: * -> *

  -- | The mutable reference type. This may contain one value at a
  -- time, attempting to read or take from an \"empty\" @CVar@ will
  -- block until it is full, and attempting to put to a \"full\"
  -- @CVar@ will block until it is empty.
  type CVar m :: * -> *

  -- | The mutable non-blocking reference type. These are like
  -- 'IORef's, but don't have the potential re-ordering problem
  -- mentioned in Data.IORef.
  type CRef m :: * -> *

  -- | An abstract handle to a thread
  type ThreadId m :: *

  -- | Fork a computation to happen concurrently. Communication may
  -- happen over @CVar@s.
  fork :: m () -> m (ThreadId m)

  -- | Like 'fork', but the child thread is passed a function that can
  -- be used to unmask asynchronous exceptions. This function should
  -- not be used within a 'mask' or 'uninterruptibleMask'.
  forkWithUnmask :: ((forall a. m a -> m a) -> m ()) -> m (ThreadId m)

  -- | Fork a computation to happen on a specific processor. The
  -- specified int is the /capability number/, typically capabilities
  -- correspond to physical processors but this is implementation
  -- dependent. The int is interpreted modulo to the total number of
  -- capabilities as returned by 'getNumCapabilities'.
  forkOn :: Int -> m () -> m (ThreadId m)

  -- | Get the number of Haskell threads that can run simultaneously.
  getNumCapabilities :: m Int

  -- | Get the @ThreadId@ of the current thread.
  myThreadId :: m (ThreadId m)

  -- | Create a new empty @CVar@.
  newEmptyCVar :: m (CVar m a)

  -- | Put a value into a @CVar@. If there is already a value there,
  -- this will block until that value has been taken, at which point
  -- the value will be stored. The default implementation is very bad,
  -- as it does not make use of any blocking functionality, and should
  -- probably b overridden.
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
  -- value in the @CVar@ already, until one has been put. The default
  -- implementation is very bad, as it does not make use of any
  -- blocking functionality, and should probably b overridden.
  --
  -- > takeCVar cvar = tryTakeCVar cvar >>= maybe (takeCVar cvar) return
  takeCVar :: CVar m a -> m a
  takeCVar cvar = tryTakeCVar cvar >>= maybe (takeCVar cvar) return

  -- | Attempt to take a value from a @CVar@, returning a 'Just' (and
  -- emptying the @CVar@) if there was something there, otherwise
  -- returning 'Nothing'.
  tryTakeCVar :: CVar m a -> m (Maybe a)

  -- | Create a new reference.
  newCRef :: a -> m (CRef m a)

  -- | Read the current value stored in a reference.
  readCRef :: CRef m a -> m a

  -- | Atomically modify the value stored in a reference.
  modifyCRef :: CRef m a -> (a -> (a, b)) -> m b

  -- | Replace the value stored in a reference.
  --
  -- > writeCRef r a = modifyCRef r $ const (a, ())
  writeCRef :: CRef m a -> a -> m ()
  writeCRef r a = modifyCRef r $ const (a, ())

  -- | Perform a series of STM actions atomically.
  atomically :: STMLike m a -> m a

  -- | Throw an exception. This will \"bubble up\" looking for an
  -- exception handler capable of dealing with it and, if one is not
  -- found, the thread is killed.
  --
  -- > throw = Control.Monad.Catch.throwM
  throw :: Exception e => e -> m a
  throw = Ca.throwM

  -- | Catch an exception. This is only required to be able to catch
  -- exceptions raised by 'throw', unlike the more general
  -- Control.Exception.catch function. If you need to be able to catch
  -- /all/ errors, you will have to use 'IO'.
  --
  -- > catch = Control.Monad.Catch.catch
  catch :: Exception e => m a -> (e -> m a) -> m a
  catch = Ca.catch

  -- | Throw an exception to the target thread. This blocks until the
  -- exception is delivered, and it is just as if the target thread
  -- had raised it with 'throw'. This can interrupt a blocked action.
  throwTo :: Exception e => ThreadId m -> e -> m ()

  -- | Executes a computation with asynchronous exceptions
  -- /masked/. That is, any thread which attempts to raise an
  -- exception in the current thread with 'throwTo' will be blocked
  -- until asynchronous exceptions are unmasked again.
  --
  -- The argument passed to mask is a function that takes as its
  -- argument another function, which can be used to restore the
  -- prevailing masking state within the context of the masked
  -- computation. This function should not be used within an
  -- 'uninterruptibleMask'.
  --
  -- > mask = Control.Monad.Catch.mask
  mask :: ((forall a. m a -> m a) -> m b) -> m b
  mask = Ca.mask

  -- | Like 'mask', but the masked computation is not
  -- interruptible. THIS SHOULD BE USED WITH GREAT CARE, because if a
  -- thread executing in 'uninterruptibleMask' blocks for any reason,
  -- then the thread (and possibly the program, if this is the main
  -- thread) will be unresponsive and unkillable. This function should
  -- only be necessary if you need to mask exceptions around an
  -- interruptible operation, and you can guarantee that the
  -- interruptible operation will only block for a short period of
  -- time. The supplied unmasking function should not be used within a
  -- 'mask'.
  --
  -- > uninterruptibleMask = Control.Monad.Catch.uninterruptibleMask
  uninterruptibleMask :: ((forall a. m a -> m a) -> m b) -> m b
  uninterruptibleMask = Ca.uninterruptibleMask

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
  type STMLike  IO = STM
  type CVar     IO = MVar
  type CRef     IO = IORef
  type ThreadId IO = C.ThreadId

  readCVar       = readMVar
  fork           = forkIO
  forkWithUnmask = C.forkIOWithUnmask
  forkOn         = C.forkOn
  getNumCapabilities = C.getNumCapabilities
  myThreadId     = C.myThreadId
  throwTo        = C.throwTo
  newEmptyCVar   = newEmptyMVar
  putCVar        = putMVar
  tryPutCVar     = tryPutMVar
  takeCVar       = takeMVar
  tryTakeCVar    = tryTakeMVar
  newCRef        = newIORef
  readCRef       = readIORef
  modifyCRef     = atomicModifyIORef
  writeCRef      = atomicWriteIORef
  atomically     = S.atomically

-- | Create a concurrent computation for the provided action, and
-- return a @CVar@ which can be used to query the result.
spawn :: MonadConc m => m a -> m (CVar m a)
spawn ma = do
  cvar <- newEmptyCVar
  _ <- fork $ ma >>= putCVar cvar
  return cvar

-- | Fork a thread and call the supplied function when the thread is
-- about to terminate, with an exception or a returned value. The
-- function is called with asynchronous exceptions masked.
--
-- This function is useful for informing the parent when a child
-- terminates, for example.
forkFinally :: MonadConc m => m a -> (Either SomeException a -> m ()) -> m (ThreadId m)
forkFinally action and_then =
  mask $ \restore ->
    fork $ Ca.try (restore action) >>= and_then

-- | Raise the 'ThreadKilled' exception in the target thread. Note
-- that if the thread is prepared to catch this exception, it won't
-- actually kill it.
killThread :: MonadConc m => ThreadId m -> m ()
killThread tid = throwTo tid ThreadKilled

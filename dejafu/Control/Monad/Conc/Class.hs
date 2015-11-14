{-# LANGUAGE CPP              #-}
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
  , cas

  -- * Bound Threads

  -- | @MonadConc@ does not support bound threads, if you need that
  -- sort of thing you will have to use regular @IO@.

  , rtsSupportsBoundThreads
  , isCurrentThreadBound
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, readMVar, newEmptyMVar, putMVar, tryPutMVar, takeMVar, tryTakeMVar)
import Control.Exception (Exception, AsyncException(ThreadKilled), SomeException)
import Control.Monad (liftM)
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.STM (STM)
import Control.Monad.STM.Class (MonadSTM, CTVar)
import Control.Monad.Trans (lift)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef, atomicWriteIORef)

import qualified Control.Concurrent as C
import qualified Control.Monad.Catch as Ca
import qualified Control.Monad.RWS.Lazy as RL
import qualified Control.Monad.RWS.Strict as RS
import qualified Control.Monad.STM as S
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS
import qualified Data.Atomics as A

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative)
import Data.Monoid (Monoid, mempty)
#endif

-- | @MonadConc@ is an abstraction over GHC's typical concurrency
-- abstraction. It captures the interface of concurrency monads in
-- terms of how they can operate on shared state and in the presence
-- of exceptions.
--
-- Every @MonadConc@ has an associated 'MonadSTM', transactions of
-- which can be run atomically.
class ( Applicative m, Monad m
      , MonadCatch m, MonadThrow m, MonadMask m
      , MonadSTM (STMLike m)
      , Eq (ThreadId m), Show (ThreadId m)) => MonadConc m  where
  -- | The associated 'MonadSTM' for this class.
  type STMLike m :: * -> *

  -- | The mutable reference type, like 'MVar's. This may contain one
  -- value at a time, attempting to read or take from an \"empty\"
  -- @CVar@ will block until it is full, and attempting to put to a
  -- \"full\" @CVar@ will block until it is empty.
  type CVar m :: * -> *

  -- | The mutable non-blocking reference type. These may suffer from
  -- relaxed memory effects if functions outside the set @newCRef@,
  -- @readCRef@, @modifyCRef@, and @atomicWriteCRef@ are used.
  type CRef m :: * -> *

  -- | When performing compare-and-swap operations on @CRef@s, a
  -- @Ticket@ is a proof that a thread observed a specific previous
  -- value.
  type Ticket m :: * -> *

  -- | An abstract handle to a thread.
  type ThreadId m :: *

  -- | Fork a computation to happen concurrently. Communication may
  -- happen over @CVar@s.
  --
  -- > fork ma = forkWithUnmask (\_ -> ma)
  fork :: m () -> m (ThreadId m)
  fork ma = forkWithUnmask (\_ -> ma)

  -- | Like 'fork', but the child thread is passed a function that can
  -- be used to unmask asynchronous exceptions. This function should
  -- not be used within a 'mask' or 'uninterruptibleMask'.
  forkWithUnmask :: ((forall a. m a -> m a) -> m ()) -> m (ThreadId m)

  -- | Fork a computation to happen on a specific processor. The
  -- specified int is the /capability number/, typically capabilities
  -- correspond to physical processors or cores but this is
  -- implementation dependent. The int is interpreted modulo to the
  -- total number of capabilities as returned by 'getNumCapabilities'.
  --
  -- > forkOn c ma = forkOnWithUnmask c (\_ -> ma)
  forkOn :: Int -> m () -> m (ThreadId m)
  forkOn c ma = forkOnWithUnmask c (\_ -> ma)

  -- | Like 'forkWithUnmask' but the child thread is pinned to the
  -- given CPU, as with 'forkOn'.
  forkOnWithUnmask :: Int -> ((forall a. m a -> m a) -> m ()) -> m (ThreadId m)

  -- | Get the number of Haskell threads that can run simultaneously.
  getNumCapabilities :: m Int

  -- | Get the @ThreadId@ of the current thread.
  myThreadId :: m (ThreadId m)

  -- | Allows a context-switch to any other currently runnable thread
  -- (if any).
  yield :: m ()

  -- | Create a new empty @CVar@.
  newEmptyCVar :: m (CVar m a)

  -- | Put a value into a @CVar@. If there is already a value there,
  -- this will block until that value has been taken, at which point
  -- the value will be stored.
  putCVar :: CVar m a -> a -> m ()

  -- | Attempt to put a value in a @CVar@ non-blockingly, returning
  -- 'True' (and filling the @CVar@) if there was nothing there,
  -- otherwise returning 'False'.
  tryPutCVar :: CVar m a -> a -> m Bool

  -- | Block until a value is present in the @CVar@, and then return
  -- it. As with 'readMVar', this does not \"remove\" the value,
  -- multiple reads are possible.
  readCVar :: CVar m a -> m a

  -- | Take a value from a @CVar@. This \"empties\" the @CVar@,
  -- allowing a new value to be put in. This will block if there is no
  -- value in the @CVar@ already, until one has been put.
  takeCVar :: CVar m a -> m a

  -- | Attempt to take a value from a @CVar@ non-blockingly, returning
  -- a 'Just' (and emptying the @CVar@) if there was something there,
  -- otherwise returning 'Nothing'.
  tryTakeCVar :: CVar m a -> m (Maybe a)

  -- | Create a new reference.
  newCRef :: a -> m (CRef m a)

  -- | Read the current value stored in a reference.
  --
  -- > readCRef cref = readForCAS cref >>= peekTicket
  readCRef :: CRef m a -> m a
  readCRef cref = readForCAS cref >>= peekTicket

  -- | Atomically modify the value stored in a reference. This imposes
  -- a full memory barrier.
  modifyCRef :: CRef m a -> (a -> (a, b)) -> m b

  -- | Write a new value into an @CRef@, without imposing a memory
  -- barrier. This means that relaxed memory effects can be observed.
  writeCRef :: CRef m a -> a -> m ()

  -- | Replace the value stored in a reference, with the
  -- barrier-to-reordering property that 'modifyCRef' has.
  --
  -- > atomicWriteCRef r a = modifyCRef r $ const (a, ())
  atomicWriteCRef :: CRef m a -> a -> m ()
  atomicWriteCRef r a = modifyCRef r $ const (a, ())

  -- | Read the current value stored in a reference, returning a
  -- @Ticket@, for use in future compare-and-swap operations.
  readForCAS :: CRef m a -> m (Ticket m a)

  -- | Extract the actual Haskell value from a @Ticket@.
  --
  -- This shouldn't need to do any monadic computation, the @m@
  -- appears in the result type because of the need for injectivity in
  -- the @Ticket@ type family, which can't be expressed currently.
  peekTicket :: Ticket m a -> m a

  -- | Perform a machine-level compare-and-swap (CAS) operation on a
  -- @CRef@. Returns an indication of success and a @Ticket@ for the
  -- most current value in the @CRef@.
  --
  -- This is strict in the \"new\" value argument.
  casCRef :: CRef m a -> Ticket m a -> a -> m (Bool, Ticket m a)

  -- | A replacement for 'modifyCRef' using a compare-and-swap.
  --
  -- This is strict in the \"new\" value argument.
  modifyCRefCAS :: CRef m a -> (a -> (a, b)) -> m b

  -- | A variant of 'modifyCRefCAS' which doesn't return a result.
  --
  -- > modifyCRefCAS_ cref f = modifyCRefCAS cref (\a -> (f a, ()))
  modifyCRefCAS_ :: CRef m a -> (a -> a) -> m ()
  modifyCRefCAS_ cref f = modifyCRefCAS cref (\a -> (f a, ()))

  -- | Perform an STM transaction atomically.
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

  -- | Does nothing.
  --
  -- This function is purely for testing purposes, and indicates that
  -- the thread has a reference to the provided @CVar@ or
  -- @CTVar@. This function may be called multiple times, to add new
  -- knowledge to the system. It does not need to be called when
  -- @CVar@s or @CTVar@s are created, these get recorded
  -- automatically.
  --
  -- Gathering this information allows detection of cases where the
  -- main thread is blocked on a variable no runnable thread has a
  -- reference to, which is a deadlock situation.
  --
  -- > _concKnowsAbout _ = return ()
  _concKnowsAbout :: Either (CVar m a) (CTVar (STMLike m) a) -> m ()
  _concKnowsAbout _ = return ()

  -- | Does nothing.
  --
  -- The counterpart to '_concKnowsAbout'. Indicates that the
  -- referenced variable will never be touched again by the current
  -- thread.
  --
  -- Note that inappropriate use of @_concForgets@ can result in false
  -- positives! Be very sure that the current thread will /never/
  -- refer to the variable again, for instance when leaving its scope.
  --
  -- > _concForgets _ = return ()
  _concForgets :: Either (CVar m a) (CTVar (STMLike m) a) -> m ()
  _concForgets _ = return ()

  -- | Does nothing.
  --
  -- Indicates to the test runner that all variables which have been
  -- passed in to this thread have been recorded by calls to
  -- '_concKnowsAbout'. If every thread has called '_concAllKnown',
  -- then detection of nonglobal deadlock is turned on.
  --
  -- If a thread receives references to @CVar@s or @CTVar@s in the
  -- future (for instance, if one was sent over a channel), then
  -- '_concKnowsAbout' should be called immediately, otherwise there
  -- is a risk of identifying false positives.
  --
  -- > _concAllKnown = return ()
  _concAllKnown :: m ()
  _concAllKnown = return ()

instance MonadConc IO where
  type STMLike  IO = STM
  type CVar     IO = MVar
  type CRef     IO = IORef
  type Ticket   IO = A.Ticket
  type ThreadId IO = C.ThreadId

  readCVar       = readMVar
  fork           = forkIO
  forkWithUnmask = C.forkIOWithUnmask
  forkOn         = C.forkOn
  forkOnWithUnmask = C.forkOnWithUnmask
  getNumCapabilities = C.getNumCapabilities
  myThreadId     = C.myThreadId
  yield          = C.yield
  throwTo        = C.throwTo
  newEmptyCVar   = newEmptyMVar
  putCVar        = putMVar
  tryPutCVar     = tryPutMVar
  takeCVar       = takeMVar
  tryTakeCVar    = tryTakeMVar
  newCRef        = newIORef
  readCRef       = readIORef
  modifyCRef     = atomicModifyIORef
  writeCRef      = writeIORef
  atomicWriteCRef = atomicWriteIORef
  readForCAS     = A.readForCAS
  peekTicket     = return . A.peekTicket
  casCRef        = A.casIORef
  modifyCRefCAS  = A.atomicModifyIORefCAS
  atomically     = S.atomically

-- | Create a concurrent computation for the provided action, and
-- return a @CVar@ which can be used to query the result.
spawn :: MonadConc m => m a -> m (CVar m a)
spawn ma = do
  cvar <- newEmptyCVar
  _ <- fork $ _concKnowsAbout (Left cvar) >> ma >>= putCVar cvar
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

-- | Provided for compatibility, always returns 'False'.
rtsSupportsBoundThreads :: Bool
rtsSupportsBoundThreads = False

-- | Provided for compatibility, always returns 'False'.
isCurrentThreadBound :: MonadConc m => m Bool
isCurrentThreadBound = return False

-- | Compare-and-swap a value in a @CRef@, returning an indication of
-- success and the new value.
cas :: MonadConc m => CRef m a -> a -> m (Bool, a)
cas cref a = do
  tick         <- readForCAS cref
  (suc, tick') <- casCRef cref tick a
  a'           <- peekTicket tick'

  return (suc, a')

-------------------------------------------------------------------------------
-- Transformer instances

instance MonadConc m => MonadConc (ReaderT r m) where
  type STMLike  (ReaderT r m) = STMLike m
  type CVar     (ReaderT r m) = CVar m
  type CRef     (ReaderT r m) = CRef m
  type Ticket   (ReaderT r m) = Ticket m
  type ThreadId (ReaderT r m) = ThreadId m

  fork              = reader fork
  forkOn i          = reader (forkOn i)
  forkWithUnmask ma = ReaderT $ \r -> forkWithUnmask (\f -> runReaderT (ma $ reader f) r)
  forkOnWithUnmask i ma = ReaderT $ \r -> forkOnWithUnmask i (\f -> runReaderT (ma $ reader f) r)

  getNumCapabilities = lift getNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  readCRef           = lift . readCRef
  modifyCRef r       = lift . modifyCRef r
  writeCRef r        = lift . writeCRef r
  atomicWriteCRef r  = lift . atomicWriteCRef r
  readForCAS         = lift . readForCAS
  peekTicket         = lift . peekTicket
  casCRef r t        = lift . casCRef r t
  modifyCRefCAS r    = lift . modifyCRefCAS r
  atomically         = lift . atomically
  _concKnowsAbout    = lift . _concKnowsAbout
  _concForgets       = lift . _concForgets
  _concAllKnown      = lift _concAllKnown

reader :: Monad m => (m a -> m b) -> ReaderT r m a -> ReaderT r m b
reader f ma = ReaderT $ \r -> f (runReaderT ma r)

instance (MonadConc m, Monoid w) => MonadConc (WL.WriterT w m) where
  type STMLike  (WL.WriterT w m) = STMLike m
  type CVar     (WL.WriterT w m) = CVar m
  type CRef     (WL.WriterT w m) = CRef m
  type Ticket   (WL.WriterT w m) = Ticket m
  type ThreadId (WL.WriterT w m) = ThreadId m

  fork              = writerlazy fork
  forkOn i          = writerlazy (forkOn i)
  forkWithUnmask ma = lift $ forkWithUnmask (\f -> fst `liftM` WL.runWriterT (ma $ writerlazy f))
  forkOnWithUnmask i ma = lift $ forkOnWithUnmask i (\f -> fst `liftM` WL.runWriterT (ma $ writerlazy f))

  getNumCapabilities = lift getNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  readCRef           = lift . readCRef
  modifyCRef r       = lift . modifyCRef r
  writeCRef r        = lift . writeCRef r
  atomicWriteCRef r  = lift . atomicWriteCRef r
  readForCAS         = lift . readForCAS
  peekTicket         = lift . peekTicket
  casCRef r t        = lift . casCRef r t
  modifyCRefCAS r    = lift . modifyCRefCAS r
  atomically         = lift . atomically
  _concKnowsAbout    = lift . _concKnowsAbout
  _concForgets       = lift . _concForgets
  _concAllKnown      = lift _concAllKnown

writerlazy :: (Monad m, Monoid w) => (m a -> m b) -> WL.WriterT w m a -> WL.WriterT w m b
writerlazy f ma = lift . f $ fst `liftM` WL.runWriterT ma

instance (MonadConc m, Monoid w) => MonadConc (WS.WriterT w m) where
  type STMLike  (WS.WriterT w m) = STMLike m
  type CVar     (WS.WriterT w m) = CVar m
  type CRef     (WS.WriterT w m) = CRef m
  type Ticket   (WS.WriterT w m) = Ticket m
  type ThreadId (WS.WriterT w m) = ThreadId m

  fork              = writerstrict fork
  forkOn i          = writerstrict (forkOn i)
  forkWithUnmask ma = lift $ forkWithUnmask (\f -> fst `liftM` WS.runWriterT (ma $ writerstrict f))
  forkOnWithUnmask i ma = lift $ forkOnWithUnmask i (\f -> fst `liftM` WS.runWriterT (ma $ writerstrict f))

  getNumCapabilities = lift getNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  readCRef           = lift . readCRef
  modifyCRef r       = lift . modifyCRef r
  writeCRef r        = lift . writeCRef r
  atomicWriteCRef r  = lift . atomicWriteCRef r
  readForCAS         = lift . readForCAS
  peekTicket         = lift . peekTicket
  casCRef r t        = lift . casCRef r t
  modifyCRefCAS r    = lift . modifyCRefCAS r
  atomically         = lift . atomically
  _concKnowsAbout    = lift . _concKnowsAbout
  _concForgets       = lift . _concForgets
  _concAllKnown      = lift _concAllKnown

writerstrict :: (Monad m, Monoid w) => (m a -> m b) -> WS.WriterT w m a -> WS.WriterT w m b
writerstrict f ma = lift . f $ fst `liftM` WS.runWriterT ma

instance MonadConc m => MonadConc (SL.StateT s m) where
  type STMLike  (SL.StateT s m) = STMLike m
  type CVar     (SL.StateT s m) = CVar m
  type CRef     (SL.StateT s m) = CRef m
  type Ticket   (SL.StateT s m) = Ticket m
  type ThreadId (SL.StateT s m) = ThreadId m

  fork              = statelazy fork
  forkOn i          = statelazy (forkOn i)
  forkWithUnmask ma = SL.StateT $ \s -> (\a -> (a,s)) `liftM` forkWithUnmask (\f -> SL.evalStateT (ma $ statelazy f) s)
  forkOnWithUnmask i ma = SL.StateT $ \s -> (\a -> (a,s)) `liftM` forkOnWithUnmask i (\f -> SL.evalStateT (ma $ statelazy f) s)

  getNumCapabilities = lift getNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  readCRef           = lift . readCRef
  modifyCRef r       = lift . modifyCRef r
  writeCRef r        = lift . writeCRef r
  atomicWriteCRef r  = lift . atomicWriteCRef r
  readForCAS         = lift . readForCAS
  peekTicket         = lift . peekTicket
  casCRef r t        = lift . casCRef r t
  modifyCRefCAS r    = lift . modifyCRefCAS r
  atomically         = lift . atomically
  _concKnowsAbout    = lift . _concKnowsAbout
  _concForgets       = lift . _concForgets
  _concAllKnown      = lift _concAllKnown

statelazy :: Monad m => (m a -> m b) -> SL.StateT s m a -> SL.StateT s m b
statelazy f ma = SL.StateT $ \s -> (\b -> (b,s)) `liftM` f (SL.evalStateT ma s)

instance MonadConc m => MonadConc (SS.StateT s m) where
  type STMLike  (SS.StateT s m) = STMLike m
  type CVar     (SS.StateT s m) = CVar m
  type CRef     (SS.StateT s m) = CRef m
  type Ticket   (SS.StateT s m) = Ticket m
  type ThreadId (SS.StateT s m) = ThreadId m

  fork              = statestrict fork
  forkOn i          = statestrict (forkOn i)
  forkWithUnmask ma = SS.StateT $ \s -> (\a -> (a,s)) `liftM` forkWithUnmask (\f -> SS.evalStateT (ma $ statestrict f) s)
  forkOnWithUnmask i ma = SS.StateT $ \s -> (\a -> (a,s)) `liftM` forkOnWithUnmask i (\f -> SS.evalStateT (ma $ statestrict f) s)

  getNumCapabilities = lift getNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  readCRef           = lift . readCRef
  modifyCRef r       = lift . modifyCRef r
  writeCRef r        = lift . writeCRef r
  atomicWriteCRef r  = lift . atomicWriteCRef r
  readForCAS         = lift . readForCAS
  peekTicket         = lift . peekTicket
  casCRef r t        = lift . casCRef r t
  modifyCRefCAS r    = lift . modifyCRefCAS r
  atomically         = lift . atomically
  _concKnowsAbout    = lift . _concKnowsAbout
  _concForgets       = lift . _concForgets
  _concAllKnown      = lift _concAllKnown

statestrict :: Monad m => (m a -> m b) -> SS.StateT s m a -> SS.StateT s m b
statestrict f ma = SS.StateT $ \s -> (\b -> (b,s)) `liftM` f (SS.evalStateT ma s)

instance (MonadConc m, Monoid w) => MonadConc (RL.RWST r w s m) where
  type STMLike  (RL.RWST r w s m) = STMLike m
  type CVar     (RL.RWST r w s m) = CVar m
  type CRef     (RL.RWST r w s m) = CRef m
  type Ticket   (RL.RWST r w s m) = Ticket m
  type ThreadId (RL.RWST r w s m) = ThreadId m

  fork              = rwslazy fork
  forkOn i          = rwslazy (forkOn i)
  forkWithUnmask ma = RL.RWST $ \r s -> (\a -> (a,s,mempty)) `liftM` forkWithUnmask (\f -> fst `liftM` RL.evalRWST (ma $ rwslazy f) r s)
  forkOnWithUnmask i ma = RL.RWST $ \r s -> (\a -> (a,s,mempty)) `liftM` forkOnWithUnmask i (\f -> fst `liftM` RL.evalRWST (ma $ rwslazy f) r s)

  getNumCapabilities = lift getNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  readCRef           = lift . readCRef
  modifyCRef r       = lift . modifyCRef r
  writeCRef r        = lift . writeCRef r
  atomicWriteCRef r  = lift . atomicWriteCRef r
  readForCAS         = lift . readForCAS
  peekTicket         = lift . peekTicket
  casCRef r t        = lift . casCRef r t
  modifyCRefCAS r    = lift . modifyCRefCAS r
  atomically         = lift . atomically
  _concKnowsAbout    = lift . _concKnowsAbout
  _concForgets       = lift . _concForgets
  _concAllKnown      = lift _concAllKnown

rwslazy :: (Monad m, Monoid w) => (m a -> m b) -> RL.RWST r w s m a -> RL.RWST r w s m b
rwslazy f ma = RL.RWST $ \r s -> (\b -> (b,s,mempty)) `liftM` f (fst `liftM` RL.evalRWST ma r s)

instance (MonadConc m, Monoid w) => MonadConc (RS.RWST r w s m) where
  type STMLike  (RS.RWST r w s m) = STMLike m
  type CVar     (RS.RWST r w s m) = CVar m
  type CRef     (RS.RWST r w s m) = CRef m
  type Ticket   (RS.RWST r w s m) = Ticket m
  type ThreadId (RS.RWST r w s m) = ThreadId m

  fork              = rwsstrict fork
  forkOn i          = rwsstrict (forkOn i)
  forkWithUnmask ma = RS.RWST $ \r s -> (\a -> (a,s,mempty)) `liftM` forkWithUnmask (\f -> fst `liftM` RS.evalRWST (ma $ rwsstrict f) r s)
  forkOnWithUnmask i ma = RS.RWST $ \r s -> (\a -> (a,s,mempty)) `liftM` forkOnWithUnmask i (\f -> fst `liftM` RS.evalRWST (ma $ rwsstrict f) r s)

  getNumCapabilities = lift getNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  readCRef           = lift . readCRef
  modifyCRef r       = lift . modifyCRef r
  writeCRef r        = lift . writeCRef r
  atomicWriteCRef r  = lift . atomicWriteCRef r
  readForCAS         = lift . readForCAS
  peekTicket         = lift . peekTicket
  casCRef r t        = lift . casCRef r t
  modifyCRefCAS r    = lift . modifyCRefCAS r
  atomically         = lift . atomically
  _concKnowsAbout    = lift . _concKnowsAbout
  _concForgets       = lift . _concForgets
  _concAllKnown      = lift _concAllKnown

rwsstrict :: (Monad m, Monoid w) => (m a -> m b) -> RS.RWST r w s m a -> RS.RWST r w s m b
rwsstrict f ma = RS.RWST $ \r s -> (\b -> (b,s,mempty)) `liftM` f (fst `liftM` RS.evalRWST ma r s)

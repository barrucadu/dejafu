{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}

-- | This module captures in a typeclass the interface of concurrency
-- monads.
module Control.Monad.Conc.Class
  ( MonadConc(..)

  -- * Threads
  , spawn
  , forkFinally
  , killThread

  -- ** Named Threads
  , forkN
  , forkOnN
  , lineNum

  -- ** Bound Threads

  -- | @MonadConc@ does not support bound threads, if you need that
  -- sort of thing you will have to use regular @IO@.

  , rtsSupportsBoundThreads
  , isCurrentThreadBound

  -- * Mutable State
  , newCVar
  , newCVarN
  , cas
  ) where

-- for the class and utilities
import Control.Exception (Exception, AsyncException(ThreadKilled), SomeException)
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import qualified Control.Monad.Catch as Ca
import Control.Monad.STM.Class (MonadSTM, CTVar)
import Language.Haskell.TH (Q, Exp, Loc(..), location)

-- for the 'IO' instance
import qualified Control.Concurrent as IO
import qualified Control.Monad.STM as IO
import qualified Data.Atomics as IO
import qualified Data.IORef as IO

-- for the transformer instances
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import qualified Control.Monad.RWS.Lazy as RL
import qualified Control.Monad.RWS.Strict as RS
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS

{-# ANN module ("HLint: ignore Use const" :: String) #-}

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

  {-# MINIMAL
        (forkWithUnmask | forkWithUnmaskN)
      , (forkOnWithUnmask | forkOnWithUnmaskN)
      , getNumCapabilities
      , setNumCapabilities
      , myThreadId
      , yield
      , (newEmptyCVar | newEmptyCVarN)
      , putCVar
      , tryPutCVar
      , readCVar
      , takeCVar
      , tryTakeCVar
      , (newCRef | newCRefN)
      , modifyCRef
      , writeCRef
      , readForCAS
      , peekTicket
      , casCRef
      , modifyCRefCAS
      , atomically
      , throwTo
    #-}

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
  --
  -- > forkWithUnmask = forkWithUnmaskN ""
  forkWithUnmask :: ((forall a. m a -> m a) -> m ()) -> m (ThreadId m)
  forkWithUnmask = forkWithUnmaskN ""

  -- | Like 'forkWithUnmask', but the thread is given a name which may
  -- be used to present more useful debugging information.
  --
  -- If an empty name is given, the @ThreadId@ is used. If names
  -- conflict, successive threads with the same name are given a
  -- numeric suffix, counting up from 1.
  --
  -- > forkWithUnmaskN _ = forkWithUnmask
  forkWithUnmaskN :: String -> ((forall a. m a -> m a) -> m ()) -> m (ThreadId m)
  forkWithUnmaskN _ = forkWithUnmask

  -- | Fork a computation to happen on a specific processor. The
  -- specified int is the /capability number/, typically capabilities
  -- correspond to physical processors or cores but this is
  -- implementation dependent. The int is interpreted modulo to the
  -- total number of capabilities as returned by 'getNumCapabilities'.
  --
  -- > forkOn c ma = forkOnWithUnmask c (\_ -> ma)
  forkOn :: Int -> m () -> m (ThreadId m)
  forkOn c ma = forkOnWithUnmask c (\_ -> ma)

  -- | Like 'forkWithUnmask', but the child thread is pinned to the
  -- given CPU, as with 'forkOn'.
  --
  -- > forkOnWithUnmask = forkOnWithUnmaskN ""
  forkOnWithUnmask :: Int -> ((forall a. m a -> m a) -> m ()) -> m (ThreadId m)
  forkOnWithUnmask = forkOnWithUnmaskN ""

  -- | Like 'forkWithUnmaskN', but the child thread is pinned to the
  -- given CPU, as with 'forkOn'.
  --
  -- > forkOnWithUnmaskN _ = forkOnWithUnmask
  forkOnWithUnmaskN :: String -> Int -> ((forall a. m a -> m a) -> m ()) -> m (ThreadId m)
  forkOnWithUnmaskN _ = forkOnWithUnmask

  -- | Get the number of Haskell threads that can run simultaneously.
  getNumCapabilities :: m Int

  -- | Set the number of Haskell threads that can run simultaneously.
  setNumCapabilities :: Int -> m ()

  -- | Get the @ThreadId@ of the current thread.
  myThreadId :: m (ThreadId m)

  -- | Allows a context-switch to any other currently runnable thread
  -- (if any).
  yield :: m ()

  -- | Create a new empty @CVar@.
  --
  -- > newEmptyCVar = newEmptyCVarN ""
  newEmptyCVar :: m (CVar m a)
  newEmptyCVar = newEmptyCVarN ""

  -- | Create a new empty @CVar@, but it is given a name which may be
  -- used to present more useful debugging information.
  --
  -- If an empty name is given, a counter starting from 0 is used. If
  -- names conflict, successive @CVar@s with the same name are given a
  -- numeric suffix, counting up from 1.
  --
  -- > newEmptyCVarN _ = newEmptyCVar
  newEmptyCVarN :: String -> m (CVar m a)
  newEmptyCVarN _ = newEmptyCVar

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
  --
  -- > newCRef = newCRefN ""
  newCRef :: a -> m (CRef m a)
  newCRef = newCRefN ""

  -- | Create a new reference, but it is given a name which may be
  -- used to present more useful debugging information.
  --
  -- If an empty name is given, a counter starting from 0 is used. If
  -- names conflict, successive @CRef@s with the same name are given a
  -- numeric suffix, counting up from 1.
  --
  -- > newCRefN _ = newCRef
  newCRefN :: String -> a -> m (CRef m a)
  newCRefN _ = newCRef

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
  -- > _concKnowsAbout _ = pure ()
  _concKnowsAbout :: Either (CVar m a) (CTVar (STMLike m) a) -> m ()
  _concKnowsAbout _ = pure ()

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
  -- > _concForgets _ = pure ()
  _concForgets :: Either (CVar m a) (CTVar (STMLike m) a) -> m ()
  _concForgets _ = pure ()

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
  -- > _concAllKnown = pure ()
  _concAllKnown :: m ()
  _concAllKnown = pure ()

-------------------------------------------------------------------------------
-- Utilities

-- | Get the current line number as a String. Useful for automatically
-- naming threads, @CVar@s, and @CRef@s.
--
-- Example usage:
--
-- > forkN $lineNum ...
--
-- Unfortunately this can't be packaged up into a
-- @forkL@/@forkOnL@/etc set of functions, because this imposes a
-- 'Lift' constraint on the monad, which 'IO' does not have.
lineNum :: Q Exp
lineNum = do
  line <- show . fst . loc_start <$> location
  [| line |]

-- Threads

-- | Create a concurrent computation for the provided action, and
-- return a @CVar@ which can be used to query the result.
spawn :: MonadConc m => m a -> m (CVar m a)
spawn ma = do
  cvar <- newEmptyCVar
  _ <- fork $ _concKnowsAbout (Left cvar) >> ma >>= putCVar cvar
  pure cvar

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

-- | Like 'fork', but the thread is given a name which may be used to
-- present more useful debugging information.
--
-- If no name is given, the @ThreadId@ is used. If names conflict,
-- successive threads with the same name are given a numeric suffix,
-- counting up from 1.
forkN :: MonadConc m => String -> m () -> m (ThreadId m)
forkN name ma = forkWithUnmaskN name (\_ -> ma)

-- | Like 'forkOn', but the thread is given a name which may be used
-- to present more useful debugging information.
--
-- If no name is given, the @ThreadId@ is used. If names conflict,
-- successive threads with the same name are given a numeric suffix,
-- counting up from 1.
forkOnN :: MonadConc m => String -> Int -> m () -> m (ThreadId m)
forkOnN name i ma = forkOnWithUnmaskN name i (\_ -> ma)

-- Bound Threads

-- | Provided for compatibility, always returns 'False'.
rtsSupportsBoundThreads :: Bool
rtsSupportsBoundThreads = False

-- | Provided for compatibility, always returns 'False'.
isCurrentThreadBound :: MonadConc m => m Bool
isCurrentThreadBound = pure False

-- Mutable Variables

-- | Create a new @CVar@ containing a value.
newCVar :: MonadConc m => a -> m (CVar m a)
newCVar a = do
  cvar <- newEmptyCVar
  putCVar cvar a
  pure cvar

-- | Create a new @CVar@ containing a value, but it is given a name
-- which may be used to present more useful debugging information.
--
-- If no name is given, a counter starting from 0 is used. If names
-- conflict, successive @CVar@s with the same name are given a numeric
-- suffix, counting up from 1.
newCVarN :: MonadConc m => String -> a -> m (CVar m a)
newCVarN n a = do
  cvar <- newEmptyCVarN n
  putCVar cvar a
  pure cvar

-- | Compare-and-swap a value in a @CRef@, returning an indication of
-- success and the new value.
cas :: MonadConc m => CRef m a -> a -> m (Bool, a)
cas cref a = do
  tick         <- readForCAS cref
  (suc, tick') <- casCRef cref tick a
  a'           <- peekTicket tick'

  pure (suc, a')

-------------------------------------------------------------------------------
-- Concrete instances

instance MonadConc IO where
  type STMLike  IO = IO.STM
  type CVar     IO = IO.MVar
  type CRef     IO = IO.IORef
  type Ticket   IO = IO.Ticket
  type ThreadId IO = IO.ThreadId

  fork   = IO.forkIO
  forkOn = IO.forkOn

  forkWithUnmask   = IO.forkIOWithUnmask
  forkOnWithUnmask = IO.forkOnWithUnmask

  getNumCapabilities = IO.getNumCapabilities
  setNumCapabilities = IO.setNumCapabilities
  readCVar           = IO.readMVar
  myThreadId         = IO.myThreadId
  yield              = IO.yield
  throwTo            = IO.throwTo
  newEmptyCVar       = IO.newEmptyMVar
  putCVar            = IO.putMVar
  tryPutCVar         = IO.tryPutMVar
  takeCVar           = IO.takeMVar
  tryTakeCVar        = IO.tryTakeMVar
  newCRef            = IO.newIORef
  readCRef           = IO.readIORef
  modifyCRef         = IO.atomicModifyIORef
  writeCRef          = IO.writeIORef
  atomicWriteCRef    = IO.atomicWriteIORef
  readForCAS         = IO.readForCAS
  peekTicket         = pure . IO.peekTicket
  casCRef            = IO.casIORef
  modifyCRefCAS      = IO.atomicModifyIORefCAS
  atomically         = IO.atomically

-------------------------------------------------------------------------------
-- Transformer instances

instance MonadConc m => MonadConc (ReaderT r m) where
  type STMLike  (ReaderT r m) = STMLike m
  type CVar     (ReaderT r m) = CVar m
  type CRef     (ReaderT r m) = CRef m
  type Ticket   (ReaderT r m) = Ticket m
  type ThreadId (ReaderT r m) = ThreadId m

  fork   = reader fork
  forkOn = reader . forkOn

  forkWithUnmask        = reader' forkWithUnmask
  forkWithUnmaskN   n   = reader' (forkWithUnmaskN   n  )
  forkOnWithUnmask    i = reader' (forkOnWithUnmask    i)
  forkOnWithUnmaskN n i = reader' (forkOnWithUnmaskN n i)

  getNumCapabilities = lift getNumCapabilities
  setNumCapabilities = lift . setNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  newEmptyCVarN      = lift . newEmptyCVarN
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  newCRefN n         = lift . newCRefN n
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

reader :: Monad m
       => (m a -> m b)
       -> ReaderT r m a
       -> ReaderT r m b
reader f ma = ReaderT $ \r -> f (runReaderT ma r)

reader' :: Monad m
        => (((forall x. m x -> m x) -> m a) -> m b)
        -> ((forall x. ReaderT r m x -> ReaderT r m x)
          -> ReaderT r m a)
        -> ReaderT r m b
reader' f ma = ReaderT $ \r -> f (\g -> runReaderT (ma $ reader g) r)

instance (MonadConc m, Monoid w) => MonadConc (WL.WriterT w m) where
  type STMLike  (WL.WriterT w m) = STMLike m
  type CVar     (WL.WriterT w m) = CVar m
  type CRef     (WL.WriterT w m) = CRef m
  type Ticket   (WL.WriterT w m) = Ticket m
  type ThreadId (WL.WriterT w m) = ThreadId m

  fork   = writerlazy fork
  forkOn = writerlazy . forkOn

  forkWithUnmask        = writerlazy' forkWithUnmask
  forkWithUnmaskN   n   = writerlazy' (forkWithUnmaskN   n  )
  forkOnWithUnmask    i = writerlazy' (forkOnWithUnmask    i)
  forkOnWithUnmaskN n i = writerlazy' (forkOnWithUnmaskN n i)

  getNumCapabilities = lift getNumCapabilities
  setNumCapabilities = lift . setNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  newEmptyCVarN      = lift . newEmptyCVarN
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  newCRefN n         = lift . newCRefN n
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

writerlazy :: (Monad m, Monoid w)
           => (m a -> m b)
           -> WL.WriterT w m a
           -> WL.WriterT w m b
writerlazy f ma = lift $ f (fst <$> WL.runWriterT ma)

writerlazy' :: (Monad m, Monoid w)
            => (((forall x. m x -> m x) -> m a) -> m b)
            -> ((forall x. WL.WriterT w m x -> WL.WriterT w m x)
               -> WL.WriterT w m a)
            -> WL.WriterT w m b
writerlazy' f ma = lift $ f (\g -> fst <$> WL.runWriterT (ma $ writerlazy g))

instance (MonadConc m, Monoid w) => MonadConc (WS.WriterT w m) where
  type STMLike  (WS.WriterT w m) = STMLike m
  type CVar     (WS.WriterT w m) = CVar m
  type CRef     (WS.WriterT w m) = CRef m
  type Ticket   (WS.WriterT w m) = Ticket m
  type ThreadId (WS.WriterT w m) = ThreadId m

  fork   = writerstrict fork
  forkOn = writerstrict . forkOn

  forkWithUnmask        = writerstrict' forkWithUnmask
  forkWithUnmaskN   n   = writerstrict' (forkWithUnmaskN   n  )
  forkOnWithUnmask    i = writerstrict' (forkOnWithUnmask    i)
  forkOnWithUnmaskN n i = writerstrict' (forkOnWithUnmaskN n i)

  getNumCapabilities = lift getNumCapabilities
  setNumCapabilities = lift . setNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  newEmptyCVarN      = lift . newEmptyCVarN
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  newCRefN n         = lift . newCRefN n
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

writerstrict :: (Monad m, Monoid w)
             => (m a -> m b)
             -> WS.WriterT w m a
             -> WS.WriterT w m b
writerstrict f ma = lift . f $ fst <$> WS.runWriterT ma

writerstrict' :: (Monad m, Monoid w)
              => (((forall x. m x -> m x) -> m a) -> m b)
              -> ((forall x. WS.WriterT w m x -> WS.WriterT w m x)
                 -> WS.WriterT w m a)
              -> WS.WriterT w m b
writerstrict' f ma = lift $ f (\g -> fst <$> WS.runWriterT (ma $ writerstrict g))

instance MonadConc m => MonadConc (SL.StateT s m) where
  type STMLike  (SL.StateT s m) = STMLike m
  type CVar     (SL.StateT s m) = CVar m
  type CRef     (SL.StateT s m) = CRef m
  type Ticket   (SL.StateT s m) = Ticket m
  type ThreadId (SL.StateT s m) = ThreadId m

  fork   = statelazy fork
  forkOn = statelazy . forkOn

  forkWithUnmask        = statelazy' forkWithUnmask
  forkWithUnmaskN   n   = statelazy' (forkWithUnmaskN   n  )
  forkOnWithUnmask    i = statelazy' (forkOnWithUnmask    i)
  forkOnWithUnmaskN n i = statelazy' (forkOnWithUnmaskN n i)

  getNumCapabilities = lift getNumCapabilities
  setNumCapabilities = lift . setNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  newEmptyCVarN      = lift . newEmptyCVarN
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  newCRefN n         = lift . newCRefN n
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

statelazy :: Monad m
          => (m a -> m b)
          -> SL.StateT s m a
          -> SL.StateT s m b
statelazy f ma = SL.StateT $ \s -> (\b -> (b,s)) <$> f (SL.evalStateT ma s)

statelazy' :: Monad m
           => (((forall x. m x -> m x) -> m a) -> m b)
           -> ((forall x. SL.StateT s m x -> SL.StateT s m x)
              -> SL.StateT s m a)
           -> SL.StateT s m b
statelazy' f ma = SL.StateT $ \s -> (\a -> (a,s)) <$> f (\g -> SL.evalStateT (ma $ statelazy g) s)

instance MonadConc m => MonadConc (SS.StateT s m) where
  type STMLike  (SS.StateT s m) = STMLike m
  type CVar     (SS.StateT s m) = CVar m
  type CRef     (SS.StateT s m) = CRef m
  type Ticket   (SS.StateT s m) = Ticket m
  type ThreadId (SS.StateT s m) = ThreadId m

  fork   = statestrict fork
  forkOn = statestrict . forkOn

  forkWithUnmask        = statestrict' forkWithUnmask
  forkWithUnmaskN   n   = statestrict' (forkWithUnmaskN   n  )
  forkOnWithUnmask    i = statestrict' (forkOnWithUnmask    i)
  forkOnWithUnmaskN n i = statestrict' (forkOnWithUnmaskN n i)

  getNumCapabilities = lift getNumCapabilities
  setNumCapabilities = lift . setNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  newEmptyCVarN      = lift . newEmptyCVarN
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  newCRefN n         = lift . newCRefN n
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

statestrict :: Monad m
            => (m a -> m b)
            -> SS.StateT s m a
            -> SS.StateT s m b
statestrict f ma = SS.StateT $ \s -> (\b -> (b,s)) <$> f (SS.evalStateT ma s)

statestrict' :: Monad m
             => (((forall x. m x -> m x) -> m a) -> m b)
             -> ((forall x. SS.StateT s m x -> SS.StateT s m x)
                -> SS.StateT s m a)
             -> SS.StateT s m b
statestrict' f ma = SS.StateT $ \s -> (\a -> (a,s)) <$> f (\g -> SS.evalStateT (ma $ statestrict g) s)

instance (MonadConc m, Monoid w) => MonadConc (RL.RWST r w s m) where
  type STMLike  (RL.RWST r w s m) = STMLike m
  type CVar     (RL.RWST r w s m) = CVar m
  type CRef     (RL.RWST r w s m) = CRef m
  type Ticket   (RL.RWST r w s m) = Ticket m
  type ThreadId (RL.RWST r w s m) = ThreadId m

  fork   = rwslazy fork
  forkOn = rwslazy . forkOn

  forkWithUnmask        = rwslazy' forkWithUnmask
  forkWithUnmaskN   n   = rwslazy' (forkWithUnmaskN   n  )
  forkOnWithUnmask    i = rwslazy' (forkOnWithUnmask    i)
  forkOnWithUnmaskN n i = rwslazy' (forkOnWithUnmaskN n i)

  getNumCapabilities = lift getNumCapabilities
  setNumCapabilities = lift . setNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  newEmptyCVarN      = lift . newEmptyCVarN
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  newCRefN n         = lift . newCRefN n
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

rwslazy :: (Monad m, Monoid w)
        => (m a -> m b)
        -> RL.RWST r w s m a
        -> RL.RWST r w s m b
rwslazy f ma = RL.RWST $ \r s -> (\b -> (b,s,mempty)) <$> f (fst <$> RL.evalRWST ma r s)

rwslazy' :: (Monad m, Monoid w)
         => (((forall x. m x -> m x) -> m a) -> m b)
         -> ((forall x. RL.RWST r w s m x -> RL.RWST r w s m x)
            -> RL.RWST r w s m a)
         -> RL.RWST r w s m b
rwslazy' f ma = RL.RWST $ \r s -> (\a -> (a,s,mempty)) <$> f (\g -> fst <$> RL.evalRWST (ma $ rwslazy g) r s)

instance (MonadConc m, Monoid w) => MonadConc (RS.RWST r w s m) where
  type STMLike  (RS.RWST r w s m) = STMLike m
  type CVar     (RS.RWST r w s m) = CVar m
  type CRef     (RS.RWST r w s m) = CRef m
  type Ticket   (RS.RWST r w s m) = Ticket m
  type ThreadId (RS.RWST r w s m) = ThreadId m

  fork   = rwsstrict fork
  forkOn = rwsstrict . forkOn

  forkWithUnmask        = rwsstrict' forkWithUnmask
  forkWithUnmaskN   n   = rwsstrict' (forkWithUnmaskN   n  )
  forkOnWithUnmask    i = rwsstrict' (forkOnWithUnmask    i)
  forkOnWithUnmaskN n i = rwsstrict' (forkOnWithUnmaskN n i)

  getNumCapabilities = lift getNumCapabilities
  setNumCapabilities = lift . setNumCapabilities
  myThreadId         = lift myThreadId
  yield              = lift yield
  throwTo t          = lift . throwTo t
  newEmptyCVar       = lift newEmptyCVar
  newEmptyCVarN      = lift . newEmptyCVarN
  readCVar           = lift . readCVar
  putCVar v          = lift . putCVar v
  tryPutCVar v       = lift . tryPutCVar v
  takeCVar           = lift . takeCVar
  tryTakeCVar        = lift . tryTakeCVar
  newCRef            = lift . newCRef
  newCRefN n         = lift . newCRefN n
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

rwsstrict :: (Monad m, Monoid w)
          => (m a -> m b)
          -> RS.RWST r w s m a
          -> RS.RWST r w s m b
rwsstrict f ma = RS.RWST $ \r s -> (\b -> (b,s,mempty)) <$> f (fst <$> RS.evalRWST ma r s)

rwsstrict' :: (Monad m, Monoid w)
           => (((forall x. m x -> m x) -> m a) -> m b)
           -> ((forall x. RS.RWST r w s m x -> RS.RWST r w s m x)
              -> RS.RWST r w s m a)
           -> RS.RWST r w s m b
rwsstrict' f ma = RS.RWST $ \r s -> (\a -> (a,s,mempty)) <$> f (\g -> fst <$> RS.evalRWST (ma $ rwsstrict g) r s)

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Deterministic traced execution of concurrent computations which
-- may do @IO@.
--
-- __Caution!__ Blocking on the action of another thread in 'liftIO'
-- cannot be detected! So if you perform some potentially blocking
-- action in a 'liftIO' the entire collection of threads may deadlock!
-- You should therefore keep @IO@ blocks small, and only perform
-- blocking operations with the supplied primitives, insofar as
-- possible.
module Test.DejaFu.Deterministic.IO
  ( -- * The @ConcIO@ Monad
    ConcIO
  , Failure(..)
  , runConcIO
  , liftIO
  , fork
  , forkFinally
  , forkWithUnmask
  , myThreadId
  , spawn
  , atomically
  , throw
  , throwTo
  , killThread
  , catch
  , mask
  , uninterruptibleMask

  -- * Communication: CVars
  , CVar
  , newEmptyCVar
  , putCVar
  , tryPutCVar
  , readCVar
  , takeCVar
  , tryTakeCVar

  -- * Testing
  , _concNoTest

  -- * Execution traces
  , Trace
  , Decision(..)
  , ThreadAction(..)
  , CVarId
  , showTrace

  -- * Scheduling
  , module Test.DejaFu.Deterministic.Schedule
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Exception (Exception, SomeException(..))
import Control.Monad.Cont (cont, runCont)
import Control.State (Wrapper(..), refIO)
import Data.IORef (IORef, newIORef)
import Test.DejaFu.Deterministic.Internal
import Test.DejaFu.Deterministic.Schedule
import Test.DejaFu.STM (STMLike, runTransactionIO)

import qualified Control.Monad.Catch as Ca
import qualified Control.Monad.Conc.Class as C
import qualified Control.Monad.IO.Class as IO

-- | The 'IO' variant of Test.DejaFu.Deterministic's @Conc@ monad.
newtype ConcIO t a = C { unC :: M IO IORef (STMLike t) a } deriving (Functor, Applicative, Monad)

instance Ca.MonadCatch (ConcIO t) where
  catch = catch

instance Ca.MonadThrow (ConcIO t) where
  throwM = throw

instance Ca.MonadMask (ConcIO t) where
  mask = mask
  uninterruptibleMask = uninterruptibleMask

instance IO.MonadIO (ConcIO t) where
  liftIO = liftIO

instance C.MonadConc (ConcIO t) where
  type CVar     (ConcIO t) = CVar t
  type STMLike  (ConcIO t) = STMLike t IO IORef
  type ThreadId (ConcIO t) = Int

  fork           = fork
  forkWithUnmask = forkWithUnmask
  myThreadId     = myThreadId
  throwTo        = throwTo
  newEmptyCVar   = newEmptyCVar
  putCVar        = putCVar
  tryPutCVar     = tryPutCVar
  readCVar       = readCVar
  takeCVar       = takeCVar
  tryTakeCVar    = tryTakeCVar
  atomically     = atomically
  _concNoTest    = _concNoTest

fixed :: Fixed IO IORef (STMLike t)
fixed = Wrapper refIO $ unC . liftIO

-- | The concurrent variable type used with the 'ConcIO' monad. These
-- behave the same as @Conc@'s @CVar@s
newtype CVar t a = V { unV :: R IORef a } deriving Eq

-- | Lift an 'IO' action into the 'ConcIO' monad.
liftIO :: IO a -> ConcIO t a
liftIO ma = C $ cont lifted where
  lifted c = ALift $ c <$> ma

-- | Run the provided computation concurrently, returning the result.
spawn :: ConcIO t a -> ConcIO t (CVar t a)
spawn = C.spawn

-- | Block on a 'CVar' until it is full, then read from it (without
-- emptying).
readCVar :: CVar t a -> ConcIO t a
readCVar cvar = C $ cont $ AGet $ unV cvar

-- | Run the provided computation concurrently.
fork :: ConcIO t () -> ConcIO t ThreadId
fork (C ma) = C $ cont $ AFork (runCont ma $ const AStop)

-- | Get the 'ThreadId' of the current thread.
myThreadId :: ConcIO t ThreadId
myThreadId = C $ cont AMyTId

-- | Run the provided 'MonadSTM' transaction atomically. If 'retry' is
-- called, it will be blocked until any of the touched 'CTVar's have
-- been written to.
atomically :: STMLike t IO IORef a -> ConcIO t a
atomically stm = C $ cont $ AAtom stm

-- | Create a new empty 'CVar'.
newEmptyCVar :: ConcIO t (CVar t a)
newEmptyCVar = C $ cont lifted where
  lifted c = ANew $ \cvid -> c <$> newEmptyCVar' cvid
  newEmptyCVar' cvid = (\ref -> V (cvid, ref)) <$> newIORef Nothing

-- | Block on a 'CVar' until it is empty, then write to it.
putCVar :: CVar t a -> a -> ConcIO t ()
putCVar cvar a = C $ cont $ \c -> APut (unV cvar) a $ c ()

-- | Put a value into a 'CVar' if there isn't one, without blocking.
tryPutCVar :: CVar t a -> a -> ConcIO t Bool
tryPutCVar cvar a = C $ cont $ ATryPut (unV cvar) a

-- | Block on a 'CVar' until it is full, then read from it (with
-- emptying).
takeCVar :: CVar t a -> ConcIO t a
takeCVar cvar = C $ cont $ ATake $ unV cvar

-- | Read a value from a 'CVar' if there is one, without blocking.
tryTakeCVar :: CVar t a -> ConcIO t (Maybe a)
tryTakeCVar cvar = C $ cont $ ATryTake $ unV cvar

-- | Raise an exception in the 'ConcIO' monad. The exception is raised
-- when the action is run, not when it is applied. It short-citcuits
-- the rest of the computation:
--
-- > throw e >> x == throw e
throw :: Exception e => e -> ConcIO t a
throw e = C $ cont $ \_ -> AThrow (SomeException e)

-- | Throw an exception to the target thread. This blocks until the
-- exception is delivered, and it is just as if the target thread had
-- raised it with 'throw'. This can interrupt a blocked action.
throwTo :: Exception e => ThreadId -> e -> ConcIO t ()
throwTo tid e = C $ cont $ \c -> AThrowTo tid (SomeException e) $ c ()

-- | Raise the 'ThreadKilled' exception in the target thread. Note
-- that if the thread is prepared to catch this exception, it won't
-- actually kill it.
killThread :: ThreadId => ConcIO t ()
killThread = C.killThread

-- | Catch an exception raised by 'throw'. This __cannot__ catch
-- errors, such as evaluating 'undefined', or division by zero. If you
-- need that, use Control.Exception.catch and 'liftIO'.
catch :: Exception e => ConcIO t a -> (e -> ConcIO t a) -> ConcIO t a
catch ma h = C $ cont $ ACatching (unC . h) (unC ma)

-- | Fork a thread and call the supplied function when the thread is
-- about to terminate, with an exception or a returned value. The
-- function is called with asynchronous exceptions masked.
--
-- This function is useful for informing the parent when a child
-- terminates, for example.
forkFinally :: ConcIO t a -> (Either SomeException a -> ConcIO t ()) -> ConcIO t ThreadId
forkFinally action and_then =
  mask $ \restore ->
    fork $ Ca.try (restore action) >>= and_then

-- | Like 'fork', but the child thread is passed a function that can
-- be used to unmask asynchronous exceptions.
forkWithUnmask :: ((forall a. ConcIO t a -> ConcIO t a) -> ConcIO t ()) -> ConcIO t ThreadId
forkWithUnmask = error "'forkWithUnmask' not yet implemented for 'ConcIO'"

-- | Executes a computation with asynchronous exceptions
-- /masked/. That is, any thread which attempts to raise an exception
-- in the current thread with 'throwTo' will be blocked until
-- asynchronous exceptions are unmasked again.
--
-- The argument passed to mask is a function that takes as its
-- argument another function, which can be used to restore the
-- prevailing masking state within the context of the masked
-- computation.
mask :: ((forall a. ConcIO t a -> ConcIO t a) -> ConcIO t b) -> ConcIO t b
mask = error "'mask' not yet implemented for 'ConcIO'"

-- | Like 'mask', but the masked computation is not
-- interruptible. THIS SHOULD BE USED WITH GREAT CARE, because if a
-- thread executing in 'uninterruptibleMask' blocks for any reason,
-- then the thread (and possibly the program, if this is the main
-- thread) will be unresponsive and unkillable. This function should
-- only be necessary if you need to mask exceptions around an
-- interruptible operation, and you can guarantee that the
-- interruptible operation will only block for a short period of time.
uninterruptibleMask :: ((forall a. ConcIO t a -> ConcIO t a) -> ConcIO t b) -> ConcIO t b
uninterruptibleMask = error "'uninterruptibleMask' not yet implemented for 'ConcIO'"

-- | Run the argument in one step. If the argument fails, the whole
-- computation will fail.
_concNoTest :: ConcIO t a -> ConcIO t a
_concNoTest ma = C $ cont $ \c -> ANoTest (unC ma) c

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning an failure reason on error. Also returned is the
-- final state of the scheduler, and an execution trace.
runConcIO :: Scheduler s -> s -> (forall t. ConcIO t a) -> IO (Either Failure a, s, Trace)
runConcIO sched s ma = runFixed fixed runTransactionIO sched s $ unC ma

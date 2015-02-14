{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Deterministic traced execution of concurrent computations which
-- don't do @IO@.
--
-- This works by executing the computation on a single thread, calling
-- out to the supplied scheduler after each step to determine which
-- thread runs next.
module Test.DejaFu.Deterministic
  ( -- * The @Conc@ Monad
    Conc
  , Failure(..)
  , runConc
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
import Control.Exception (Exception, MaskingState(..), SomeException(..))
import Control.Monad.Cont (cont, runCont)
import Control.Monad.ST (ST, runST)
import Control.State (Wrapper(..), refST)
import Data.STRef (STRef, newSTRef)
import Test.DejaFu.Deterministic.Internal
import Test.DejaFu.Deterministic.Schedule
import Test.DejaFu.STM (STMLike, runTransactionST)

import qualified Control.Monad.Catch as Ca
import qualified Control.Monad.Conc.Class as C

-- | The @Conc@ monad itself. This uses the same
-- universally-quantified indexing state trick as used by 'ST' and
-- 'STRef's to prevent mutable references from leaking out of the
-- monad.
newtype Conc t a = C { unC :: M (ST t) (STRef t) (STMLike t) a } deriving (Functor, Applicative, Monad)

wrap :: (M (ST t) (STRef t) (STMLike t) a -> M (ST t) (STRef t) (STMLike t) a) -> (Conc t a -> Conc t a)
wrap f = C . f . unC

instance Ca.MonadCatch (Conc t) where
  catch = catch

instance Ca.MonadThrow (Conc t) where
  throwM = throw

instance Ca.MonadMask (Conc t) where
  mask = mask
  uninterruptibleMask = uninterruptibleMask

instance C.MonadConc (Conc t) where
  type CVar     (Conc t) = CVar t
  type STMLike  (Conc t) = STMLike t (ST t) (STRef t)
  type ThreadId (Conc t) = Int

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

fixed :: Fixed (ST t) (STRef t) (STMLike t)
fixed = Wrapper refST $ \ma -> cont (\c -> ALift $ c <$> ma)

-- | The concurrent variable type used with the 'Conc' monad. One
-- notable difference between these and 'MVar's is that 'MVar's are
-- single-wakeup, and wake up in a FIFO order. Writing to a @CVar@
-- wakes up all threads blocked on reading it, and it is up to the
-- scheduler which one runs next. Taking from a @CVar@ behaves
-- analogously.
newtype CVar t a = V { unV :: R (STRef t) a } deriving Eq

-- | Run the provided computation concurrently, returning the result.
spawn :: Conc t a -> Conc t (CVar t a)
spawn = C.spawn

-- | Block on a 'CVar' until it is full, then read from it (without
-- emptying).
readCVar :: CVar t a -> Conc t a
readCVar cvar = C $ cont $ AGet $ unV cvar

-- | Run the provided computation concurrently.
fork :: Conc t () -> Conc t ThreadId
fork (C ma) = C $ cont $ AFork (const $ runCont ma $ const AStop)

-- | Get the 'ThreadId' of the current thread.
myThreadId :: Conc t ThreadId
myThreadId = C $ cont AMyTId

-- | Run the provided 'MonadSTM' transaction atomically. If 'retry' is
-- called, it will be blocked until any of the touched 'CTVar's have
-- been written to.
atomically :: STMLike t (ST t) (STRef t) a -> Conc t a
atomically stm = C $ cont $ AAtom stm

-- | Create a new empty 'CVar'.
newEmptyCVar :: Conc t (CVar t a)
newEmptyCVar = C $ cont lifted where
  lifted c = ANew $ \cvid -> c <$> newEmptyCVar' cvid
  newEmptyCVar' cvid = (\ref -> V (cvid, ref)) <$> newSTRef Nothing

-- | Block on a 'CVar' until it is empty, then write to it.
putCVar :: CVar t a -> a -> Conc t ()
putCVar cvar a = C $ cont $ \c -> APut (unV cvar) a $ c ()

-- | Put a value into a 'CVar' if there isn't one, without blocking.
tryPutCVar :: CVar t a -> a -> Conc t Bool
tryPutCVar cvar a = C $ cont $ ATryPut (unV cvar) a

-- | Block on a 'CVar' until it is full, then read from it (with
-- emptying).
takeCVar :: CVar t a -> Conc t a
takeCVar cvar = C $ cont $ ATake $ unV cvar

-- | Read a value from a 'CVar' if there is one, without blocking.
tryTakeCVar :: CVar t a -> Conc t (Maybe a)
tryTakeCVar cvar = C $ cont $ ATryTake $ unV cvar

-- | Raise an exception in the 'Conc' monad. The exception is raised
-- when the action is run, not when it is applied. It short-citcuits
-- the rest of the computation:
--
-- > throw e >> x == throw e
throw :: Exception e => e -> Conc t a
throw e = C $ cont $ \_ -> AThrow (SomeException e)

-- | Throw an exception to the target thread. This blocks until the
-- exception is delivered, and it is just as if the target thread had
-- raised it with 'throw'. This can interrupt a blocked action.
throwTo :: Exception e => ThreadId -> e -> Conc t ()
throwTo tid e = C $ cont $ \c -> AThrowTo tid (SomeException e) $ c ()

-- | Raise the 'ThreadKilled' exception in the target thread. Note
-- that if the thread is prepared to catch this exception, it won't
-- actually kill it.
killThread :: ThreadId => Conc t ()
killThread = C.killThread

-- | Catch an exception raised by 'throw'. This __cannot__ catch
-- errors, such as evaluating 'undefined', or division by zero. If you
-- need that, use Control.Exception.catch and 'ConcIO'.
catch :: Exception e => Conc t a -> (e -> Conc t a) -> Conc t a
catch ma h = C $ cont $ ACatching (unC . h) (unC ma)

-- | Fork a thread and call the supplied function when the thread is
-- about to terminate, with an exception or a returned value. The
-- function is called with asynchronous exceptions masked.
--
-- This function is useful for informing the parent when a child
-- terminates, for example.
forkFinally :: Conc t a -> (Either SomeException a -> Conc t ()) -> Conc t ThreadId
forkFinally action and_then =
  mask $ \restore ->
    fork $ Ca.try (restore action) >>= and_then

-- | Like 'fork', but the child thread is passed a function that can
-- be used to unmask asynchronous exceptions. This function should not
-- be used within a 'mask' or 'uninterruptibleMask'.
forkWithUnmask :: ((forall a. Conc t a -> Conc t a) -> Conc t ()) -> Conc t ThreadId
forkWithUnmask ma = C $ cont $ AFork (\umask -> runCont (unC $ ma $ wrap umask) $ const AStop)

-- | Executes a computation with asynchronous exceptions
-- /masked/. That is, any thread which attempts to raise an exception
-- in the current thread with 'throwTo' will be blocked until
-- asynchronous exceptions are unmasked again.
--
-- The argument passed to mask is a function that takes as its
-- argument another function, which can be used to restore the
-- prevailing masking state within the context of the masked
-- computation. This function should not be used within an
-- 'uninterruptibleMask'.
mask :: ((forall a. Conc t a -> Conc t a) -> Conc t b) -> Conc t b
-- Can't avoid the lambda here (and in uninterruptibleMask and in
-- ConcIO) because RankNTypes inference is scary.
mask mb = C $ cont $ AMasking MaskedInterruptible (\f -> unC $ mb $ wrap f)

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
uninterruptibleMask :: ((forall a. Conc t a -> Conc t a) -> Conc t b) -> Conc t b
uninterruptibleMask mb = C $ cont $ AMasking MaskedUninterruptible (\f -> unC $ mb $ wrap f)

-- | Run the argument in one step. If the argument fails, the whole
-- computation will fail.
_concNoTest :: Conc t a -> Conc t a
_concNoTest ma = C $ cont $ \c -> ANoTest (unC ma) c

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning a failure reason on error. Also returned is the
-- final state of the scheduler, and an execution trace.
--
-- Note how the @t@ in 'Conc' is universally quantified, what this
-- means in practice is that you can't do something like this:
--
-- > runConc (\s _ (x:_) -> (x, s)) () newEmptyCVar
--
-- So 'CVar's cannot leak out of the 'Conc' computation. If this is
-- making your head hurt, check out the \"How @runST@ works\" section
-- of <https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html>
runConc :: Scheduler s -> s -> (forall t. Conc t a) -> (Either Failure a, s, Trace)
runConc sched s ma = runST $ runFixed fixed runTransactionST sched s $ unC ma

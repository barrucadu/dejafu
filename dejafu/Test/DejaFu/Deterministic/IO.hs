{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Deterministic traced execution of concurrent computations which
-- may do @IO@.
--
-- __Warning:__ Blocking on the action of another thread in 'liftIO'
-- cannot be detected! So if you perform some potentially blocking
-- action in a 'liftIO' the entire collection of threads may deadlock!
-- You should therefore keep @IO@ blocks small, and only perform
-- blocking operations with the supplied primitives, insofar as
-- possible.
module Test.DejaFu.Deterministic.IO
  ( -- * The @ConcIO@ Monad
    ConcIO
  , Failure(..)
  , MemType(..)
  , runConcIO
  , runConcIO'

  -- * Execution traces
  , Trace
  , Trace'
  , Decision(..)
  , ThreadAction(..)
  , Lookahead(..)
  , CVarId
  , CRefId
  , MaskingState(..)
  , toTrace
  , showTrace
  , showFail

  -- * Scheduling
  , module Test.DejaFu.Deterministic.Schedule
  ) where

import Control.Exception (Exception, MaskingState(..))
import Control.Monad.Cont (cont, runCont)
import Data.IORef (IORef, newIORef)
import Test.DejaFu.Deterministic.Internal
import Test.DejaFu.Deterministic.Schedule
import Test.DejaFu.Internal (refIO)
import Test.DejaFu.STM (STMLike, runTransactionIO)
import Test.DejaFu.STM.Internal (CTVar(..))

import qualified Control.Monad.Catch as Ca
import qualified Control.Monad.Conc.Class as C
import qualified Control.Monad.IO.Class as IO
import qualified Data.IntMap.Strict as I

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..), (<$>))
#endif

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use const"    :: String) #-}

-- | The 'IO' variant of Test.DejaFu.Deterministic's
-- 'Test.DejaFu.Deterministic.Conc' monad.
newtype ConcIO t a = C { unC :: M IO IORef (STMLike t) a } deriving (Functor, Applicative, Monad)

wrap :: (M IO IORef (STMLike t) a -> M IO IORef (STMLike t) a) -> ConcIO t a -> ConcIO t a
wrap f = C . f . unC

instance Ca.MonadCatch (ConcIO t) where
  catch = Test.DejaFu.Deterministic.IO.catch

instance Ca.MonadThrow (ConcIO t) where
  throwM = throw

instance Ca.MonadMask (ConcIO t) where
  mask = mask
  uninterruptibleMask = uninterruptibleMask

instance IO.MonadIO (ConcIO t) where
  liftIO = liftIO

instance C.MonadConc (ConcIO t) where
  type CVar     (ConcIO t) = CVar t
  type CRef     (ConcIO t) = CRef t
  type STMLike  (ConcIO t) = STMLike t IO IORef
  type ThreadId (ConcIO t) = Int

  fork           = fork
  forkWithUnmask = forkWithUnmask
  forkOn         = forkOn
  getNumCapabilities = getNumCapabilities
  myThreadId     = myThreadId
  yield          = yield
  throwTo        = throwTo
  newEmptyCVar   = newEmptyCVar
  putCVar        = putCVar
  tryPutCVar     = tryPutCVar
  readCVar       = readCVar
  takeCVar       = takeCVar
  tryTakeCVar    = tryTakeCVar
  newCRef        = newCRef
  readCRef       = readCRef
  writeCRef      = writeCRef
  atomicWriteCRef = atomicWriteCRef
  modifyCRef     = modifyCRef
  atomically     = atomically
  _concKnowsAbout = _concKnowsAbout
  _concForgets   = _concForgets
  _concAllKnown  = _concAllKnown

fixed :: Fixed IO IORef (STMLike t)
fixed = refIO $ unC . liftIO

-- | The concurrent variable type used with the 'ConcIO' monad. These
-- behave the same as @Conc@'s @CVar@s
newtype CVar t a = Var { unV :: V IORef a } deriving Eq

-- | The mutable non-blocking reference type. These behave the same as
-- @Conc@'s @CRef@s
newtype CRef t a = Ref { unR :: R IORef a } deriving Eq

-- | Lift an 'IO' action into the 'ConcIO' monad.
liftIO :: IO a -> ConcIO t a
liftIO ma = C $ cont lifted where
  lifted c = ALift $ c <$> ma

-- | Block on a 'CVar' until it is full, then read from it (without
-- emptying).
readCVar :: CVar t a -> ConcIO t a
readCVar cvar = C $ cont $ AGet $ unV cvar

-- | Run the provided computation concurrently.
fork :: ConcIO t () -> ConcIO t ThreadId
fork (C ma) = C $ cont $ AFork ((\a _ -> a) $ runCont ma $ const AStop)

-- | Get the 'ThreadId' of the current thread.
myThreadId :: ConcIO t ThreadId
myThreadId = C $ cont AMyTId

-- | Allows a context-switch to any other currently runnable thread
-- (if any).
yield :: ConcIO t ()
yield = C $ cont $ \c -> AYield $ c ()

-- | Run the provided 'MonadSTM' transaction atomically. If 'retry' is
-- called, it will be blocked until any of the touched 'CTVar's have
-- been written to.
atomically :: STMLike t IO IORef a -> ConcIO t a
atomically stm = C $ cont $ AAtom stm

-- | Create a new empty 'CVar'.
newEmptyCVar :: ConcIO t (CVar t a)
newEmptyCVar = C $ cont lifted where
  lifted c = ANew $ \cvid -> c <$> newEmptyCVar' cvid
  newEmptyCVar' cvid = (\ref -> Var (cvid, ref)) <$> newIORef Nothing

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

-- | Create a new 'CRef'.
newCRef :: a -> ConcIO t (CRef t a)
newCRef a = C $ cont lifted where
  lifted c = ANewRef $ \crid -> c <$> newCRef' crid
  newCRef' crid = (\ref -> Ref (crid, ref)) <$> newIORef (I.empty, a)

-- | Read the value from a 'CRef'.
readCRef :: CRef t a -> ConcIO t a
readCRef ref = C $ cont $ AReadRef $ unR ref

-- | Atomically modify the value inside a 'CRef'.
modifyCRef :: CRef t a -> (a -> (a, b)) -> ConcIO t b
modifyCRef ref f = C $ cont $ AModRef (unR ref) f

-- | Replace the value stored inside a 'CRef'.
writeCRef :: CRef t a -> a -> ConcIO t ()
writeCRef ref a = C $ cont $ \c -> AWriteRef (unR ref) a $ c ()

-- | Replace the value stored inside a 'CRef' with a barrier to
-- re-ordering.
atomicWriteCRef :: CRef t a -> a -> ConcIO t ()
atomicWriteCRef ref a = modifyCRef ref $ const (a, ())

-- | Raise an exception in the 'ConcIO' monad. The exception is raised
-- when the action is run, not when it is applied. It short-citcuits
-- the rest of the computation:
--
-- > throw e >> x == throw e
throw :: Exception e => e -> ConcIO t a
throw e = C $ cont $ \_ -> AThrow e

-- | Throw an exception to the target thread. This blocks until the
-- exception is delivered, and it is just as if the target thread had
-- raised it with 'throw'. This can interrupt a blocked action.
throwTo :: Exception e => ThreadId -> e -> ConcIO t ()
throwTo tid e = C $ cont $ \c -> AThrowTo tid e $ c ()

-- | Catch an exception raised by 'throw'. This __cannot__ catch
-- errors, such as evaluating 'undefined', or division by zero. If you
-- need that, use Control.Exception.catch and 'liftIO'.
catch :: Exception e => ConcIO t a -> (e -> ConcIO t a) -> ConcIO t a
catch ma h = C $ cont $ ACatching (unC . h) (unC ma)

-- | Like 'fork', but the child thread is passed a function that can
-- be used to unmask asynchronous exceptions. This function should not
-- be used within a 'mask' or 'uninterruptibleMask'.
forkWithUnmask :: ((forall a. ConcIO t a -> ConcIO t a) -> ConcIO t ()) -> ConcIO t ThreadId
forkWithUnmask ma = C $ cont $
  AFork (\umask -> runCont (unC $ ma $ wrap umask) $ const AStop)

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
mask :: ((forall a. ConcIO t a -> ConcIO t a) -> ConcIO t b) -> ConcIO t b
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
uninterruptibleMask :: ((forall a. ConcIO t a -> ConcIO t a) -> ConcIO t b) -> ConcIO t b
uninterruptibleMask mb = C $ cont $
  AMasking MaskedUninterruptible (\f -> unC $ mb $ wrap f)

-- | Fork a computation to happen on a specific processor. This
-- implementation only has a single processor.
forkOn :: Int -> ConcIO t () -> ConcIO t ThreadId
forkOn _ = fork

-- | Get the number of Haskell threads that can run
-- simultaneously. This implementation lies and always returns
-- 2. There is no way to verify in the computation that this is a lie,
-- and will potentially avoid special-case behaviour for 1 capability,
-- so it seems a sane choice.
getNumCapabilities :: ConcIO t Int
getNumCapabilities = return 2

-- | Record that the referenced variable is known by the current thread.
_concKnowsAbout :: Either (CVar t a) (CTVar t IORef a) -> ConcIO t ()
_concKnowsAbout (Left  (Var (cvarid,  _))) = C $ cont $ \c -> AKnowsAbout (Left  cvarid)  (c ())
_concKnowsAbout (Right (V   (ctvarid, _))) = C $ cont $ \c -> AKnowsAbout (Right ctvarid) (c ())

-- | Record that the referenced variable will never be touched by the
-- current thread.
_concForgets :: Either (CVar t a) (CTVar t IORef a) -> ConcIO t ()
_concForgets (Left  (Var (cvarid,  _))) = C $ cont $ \c -> AForgets (Left  cvarid)  (c ())
_concForgets (Right (V   (ctvarid, _))) = C $ cont $ \c -> AForgets (Right ctvarid) (c ())

-- | Record that all 'CVar's and 'CTVar's known by the current thread
-- have been passed to '_concKnowsAbout'.
_concAllKnown :: ConcIO t ()
_concAllKnown = C $ cont $ \c -> AAllKnown (c ())

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning an failure reason on error. Also returned is the
-- final state of the scheduler, and an execution trace.
--
-- This uses the 'SequentialConsistency' memory model.
runConcIO :: Scheduler s -> s -> (forall t. ConcIO t a) -> IO (Either Failure a, s, Trace)
runConcIO sched s ma = do
  (r, s', t') <- runConcIO' sched SequentialConsistency s ma
  return (r, s', toTrace t')

-- | Variant of 'runConcIO' which produces a 'Trace''.
runConcIO' :: Scheduler s -> MemType -> s -> (forall t. ConcIO t a) -> IO (Either Failure a, s, Trace')
runConcIO' sched memtype s ma = runFixed fixed runTransactionIO sched memtype s $ unC ma

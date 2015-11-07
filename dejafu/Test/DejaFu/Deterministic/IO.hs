{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Control.Exception (MaskingState(..))
import Data.IORef (IORef)
import Test.DejaFu.Deterministic.Internal
import Test.DejaFu.Deterministic.Schedule
import Test.DejaFu.Internal (refIO)
import Test.DejaFu.STM (STMIO, runTransactionIO)
import Test.DejaFu.STM.Internal (CTVar(..))

import qualified Control.Monad.Catch as Ca
import qualified Control.Monad.Conc.Class as C
import qualified Control.Monad.IO.Class as IO

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..), (<$>))
#endif

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use const"    :: String) #-}

-- | The 'IO' variant of Test.DejaFu.Deterministic's
-- 'Test.DejaFu.Deterministic.Conc' monad.
newtype ConcIO a = C { unC :: M IO IORef STMIO a } deriving (Functor, Applicative, Monad)

toConcIO :: ((a -> Action IO IORef STMIO) -> Action IO IORef STMIO) -> ConcIO a
toConcIO = C . cont

wrap :: (M IO IORef STMIO a -> M IO IORef STMIO a) -> ConcIO a -> ConcIO a
wrap f = C . f . unC

fixed :: Fixed IO IORef STMIO
fixed = refIO $ \ma -> cont (\c -> ALift $ c <$> ma)

-- | The concurrent variable type used with the 'ConcIO' monad. These
-- behave the same as @Conc@'s @CVar@s
newtype CVar a = Var (V IORef a) deriving Eq

-- | The mutable non-blocking reference type. These behave the same as
-- @Conc@'s @CRef@s
newtype CRef a = Ref (R IORef a) deriving Eq

instance IO.MonadIO ConcIO where
  liftIO ma = toConcIO (\c -> ALift (fmap c ma))

instance Ca.MonadCatch ConcIO where
  catch ma h = toConcIO (ACatching (unC . h) (unC ma))

instance Ca.MonadThrow ConcIO where
  throwM e = toConcIO (\_ -> AThrow e)

instance Ca.MonadMask ConcIO where
  mask                mb = toConcIO (AMasking MaskedInterruptible   (\f -> unC $ mb $ wrap f))
  uninterruptibleMask mb = toConcIO (AMasking MaskedUninterruptible (\f -> unC $ mb $ wrap f))

instance C.MonadConc ConcIO where
  type CVar     ConcIO = CVar
  type CRef     ConcIO = CRef
  type STMLike  ConcIO = STMIO
  type ThreadId ConcIO = Int

  -- ----------

  forkWithUnmask  ma = toConcIO (AFork (\umask -> runCont (unC $ ma $ wrap umask) (\_ -> AStop)))
  forkOnWithUnmask _ = C.forkWithUnmask

  -- This implementation lies and always returns 2. There is no way to
  -- verify in the computation that this is a lie, and will
  -- potentially avoid special-case behaviour for 1 capability, so it
  -- seems a sane choice.
  getNumCapabilities = return 2

  myThreadId = toConcIO AMyTId

  yield = toConcIO (\c -> AYield (c ()))

  -- ----------

  newCRef a = toConcIO (\c -> ANewRef a (c . Ref))

  readCRef   (Ref ref)   = toConcIO (AReadRef ref)
  writeCRef  (Ref ref) a = toConcIO (\c -> AWriteRef ref a (c ()))
  modifyCRef (Ref ref) f = toConcIO (AModRef ref f)

  -- ----------

  newEmptyCVar = toConcIO (\c -> ANewVar (c . Var))

  putCVar  (Var var) a = toConcIO (\c -> APutVar var a (c ()))
  readCVar (Var var)   = toConcIO (AReadVar var)
  takeCVar (Var var)   = toConcIO (ATakeVar var)

  tryPutCVar  (Var var) a = toConcIO (ATryPutVar  var a)
  tryTakeCVar (Var var)   = toConcIO (ATryTakeVar var)

  -- ----------

  throwTo tid e = toConcIO (\c -> AThrowTo tid e (c ()))

  -- ----------

  atomically = toConcIO . AAtom

  -- ----------

  _concKnowsAbout (Left  (Var (cvarid,  _))) = toConcIO (\c -> AKnowsAbout (Left  cvarid)  (c ()))
  _concKnowsAbout (Right (V   (ctvarid, _))) = toConcIO (\c -> AKnowsAbout (Right ctvarid) (c ()))

  _concForgets (Left  (Var (cvarid,  _))) = toConcIO (\c -> AForgets (Left  cvarid)  (c ()))
  _concForgets (Right (V   (ctvarid, _))) = toConcIO (\c -> AForgets (Right ctvarid) (c ()))

  _concAllKnown = toConcIO (\c -> AAllKnown (c ()))

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning an failure reason on error. Also returned is the
-- final state of the scheduler, and an execution trace.
--
-- This uses the 'SequentialConsistency' memory model.
runConcIO :: Scheduler s -> s -> ConcIO a -> IO (Either Failure a, s, Trace)
runConcIO sched s ma = do
  (r, s', t') <- runConcIO' sched SequentialConsistency s ma
  return (r, s', toTrace t')

-- | Variant of 'runConcIO' which produces a 'Trace''.
runConcIO' :: Scheduler s -> MemType -> s -> ConcIO a -> IO (Either Failure a, s, Trace')
runConcIO' sched memtype s ma = runFixed fixed runTransactionIO sched memtype s $ unC ma

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Test.DejaFu.Conc
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeFamilies
--
-- Deterministic traced execution of concurrent computations.
--
-- This works by executing the computation on a single thread, calling
-- out to the supplied scheduler after each step to determine which
-- thread runs next.
module Test.DejaFu.Conc
  ( -- * The @ConcT@ monad transformer
    ConcT
  , ConcIO

  -- * Executing computations
  , Failure(..)
  , MemType(..)
  , runConcurrent
  , subconcurrency

  -- * Execution traces
  , Trace
  , Decision(..)
  , ThreadId(..)
  , ThreadAction(..)
  , Lookahead(..)
  , MVarId
  , CRefId
  , MaskingState(..)
  , showTrace
  , showFail

  -- * Scheduling
  , module Test.DejaFu.Schedule
  ) where

import           Control.Exception                (MaskingState(..))
import qualified Control.Monad.Catch              as Ca
import qualified Control.Monad.IO.Class           as IO
import           Control.Monad.Ref                (MonadRef)
import qualified Control.Monad.Ref                as Re
import           Control.Monad.Trans.Class        (MonadTrans(..))
import qualified Data.Foldable                    as F
import           Data.IORef                       (IORef)
import           Test.DejaFu.Schedule

import qualified Control.Monad.Conc.Class         as C
import           Test.DejaFu.Common
import           Test.DejaFu.Conc.Internal
import           Test.DejaFu.Conc.Internal.Common
import           Test.DejaFu.STM

-- | @since 0.6.0.0
newtype ConcT r n a = C { unC :: M n r a } deriving (Functor, Applicative, Monad)

-- | A 'MonadConc' implementation using @IO@.
--
-- @since 0.4.0.0
type ConcIO = ConcT IORef IO

toConc :: ((a -> Action n r) -> Action n r) -> ConcT r n a
toConc = C . cont

wrap :: (M n r a -> M n r a) -> ConcT r n a -> ConcT r n a
wrap f = C . f . unC

-- | @since unreleased
instance IO.MonadIO n => IO.MonadIO (ConcT r n) where
  liftIO ma = toConc (\c -> ALift (fmap c (IO.liftIO ma)))

instance Re.MonadRef (CRef r) (ConcT r n) where
  newRef a = toConc (ANewCRef "" a)

  readRef ref = toConc (AReadCRef ref)

  writeRef ref a = toConc (\c -> AWriteCRef ref a (c ()))

  modifyRef ref f = toConc (AModCRef ref (\a -> (f a, ())))

instance Re.MonadAtomicRef (CRef r) (ConcT r n) where
  atomicModifyRef ref f = toConc (AModCRef ref f)

instance MonadTrans (ConcT r) where
  lift ma = toConc (\c -> ALift (fmap c ma))

instance Ca.MonadCatch (ConcT r n) where
  catch ma h = toConc (ACatching (unC . h) (unC ma))

instance Ca.MonadThrow (ConcT r n) where
  throwM e = toConc (\_ -> AThrow e)

instance Ca.MonadMask (ConcT r n) where
  mask                mb = toConc (AMasking MaskedInterruptible   (\f -> unC $ mb $ wrap f))
  uninterruptibleMask mb = toConc (AMasking MaskedUninterruptible (\f -> unC $ mb $ wrap f))

instance Monad n => C.MonadConc (ConcT r n) where
  type MVar     (ConcT r n) = MVar r
  type CRef     (ConcT r n) = CRef r
  type Ticket   (ConcT r n) = Ticket
  type STM      (ConcT r n) = STMLike n r
  type ThreadId (ConcT r n) = ThreadId

  -- ----------

  forkWithUnmaskN   n ma = toConc (AFork   n (\umask -> runCont (unC $ ma $ wrap umask) (\_ -> AStop (pure ()))))
  forkOnWithUnmaskN n _  = C.forkWithUnmaskN n
  forkOSN n ma = forkOSWithUnmaskN n (const ma)

  isCurrentThreadBound = toConc AIsBound

  -- This implementation lies and returns 2 until a value is set. This
  -- will potentially avoid special-case behaviour for 1 capability,
  -- so it seems a sane choice.
  getNumCapabilities      = toConc AGetNumCapabilities
  setNumCapabilities caps = toConc (\c -> ASetNumCapabilities caps (c ()))

  myThreadId = toConc AMyTId

  yield = toConc (\c -> AYield (c ()))
  threadDelay n = toConc (\c -> ADelay n (c ()))

  -- ----------

  newCRefN n a = toConc (ANewCRef n a)

  readCRef   ref = toConc (AReadCRef    ref)
  readForCAS ref = toConc (AReadCRefCas ref)

  peekTicket' _ = _ticketVal

  writeCRef ref      a = toConc (\c -> AWriteCRef ref a (c ()))
  casCRef   ref tick a = toConc (ACasCRef ref tick a)

  atomicModifyCRef ref f = toConc (AModCRef    ref f)
  modifyCRefCAS    ref f = toConc (AModCRefCas ref f)

  -- ----------

  newEmptyMVarN n = toConc (ANewMVar n)

  putMVar  var a = toConc (\c -> APutMVar var a (c ()))
  readMVar var   = toConc (AReadMVar var)
  takeMVar var   = toConc (ATakeMVar var)

  tryPutMVar  var a = toConc (ATryPutMVar  var a)
  tryReadMVar var   = toConc (ATryReadMVar var)
  tryTakeMVar var   = toConc (ATryTakeMVar var)

  -- ----------

  throwTo tid e = toConc (\c -> AThrowTo tid e (c ()))

  -- ----------

  atomically = toConc . AAtom

-- move this into the instance defn when forkOSWithUnmaskN is added to MonadConc in 2018
forkOSWithUnmaskN :: Applicative n => String -> ((forall a. ConcT r n a -> ConcT r n a) -> ConcT r n ()) -> ConcT r n ThreadId
forkOSWithUnmaskN n ma
  | C.rtsSupportsBoundThreads = toConc (AForkOS n (\umask -> runCont (unC $ ma $ wrap umask) (\_ -> AStop (pure ()))))
  | otherwise = fail "RTS doesn't support multiple OS threads (use ghc -threaded when linking)"

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning a failure reason on error. Also returned is the
-- final state of the scheduler, and an execution trace.
--
-- __Warning:__ Blocking on the action of another thread in 'liftIO'
-- cannot be detected! So if you perform some potentially blocking
-- action in a 'liftIO' the entire collection of threads may deadlock!
-- You should therefore keep @IO@ blocks small, and only perform
-- blocking operations with the supplied primitives, insofar as
-- possible.
--
-- __Note:__ In order to prevent computation from hanging, the runtime
-- will assume that a deadlock situation has arisen if the scheduler
-- attempts to (a) schedule a blocked thread, or (b) schedule a
-- nonexistent thread. In either of those cases, the computation will
-- be halted.
--
-- @since unreleased
runConcurrent :: (C.MonadConc n, MonadRef r n)
  => Scheduler s
  -> MemType
  -> s
  -> ConcT r n a
  -> n (Either Failure a, s, Trace)
runConcurrent sched memtype s ma = do
  (res, ctx, trace, _) <- runConcurrency sched memtype s initialIdSource 2 (unC ma)
  pure (res, cSchedState ctx, F.toList trace)

-- | Run a concurrent computation and return its result.
--
-- This can only be called in the main thread, when no other threads
-- exist. Calls to 'subconcurrency' cannot be nested. Violating either
-- of these conditions will result in the computation failing with
-- @IllegalSubconcurrency@.
--
-- @since 0.6.0.0
subconcurrency :: ConcT r n a -> ConcT r n (Either Failure a)
subconcurrency ma = toConc (ASub (unC ma))

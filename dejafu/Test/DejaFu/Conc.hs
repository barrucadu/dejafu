{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Test.DejaFu.Conc
-- Copyright   : (c) 2016--2019 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies
--
-- Deterministic traced execution of concurrent computations.
--
-- This works by executing the computation on a single thread, calling
-- out to the supplied scheduler after each step to determine which
-- thread runs next.
module Test.DejaFu.Conc
  ( -- * Expressing concurrent programs
    ConcT
  , ConcIO
  , basic

  -- ** Setup and teardown
  , WithSetup
  , WithSetupAndTeardown
  , withSetup
  , withTeardown
  , withSetupAndTeardown

  -- * Executing concurrent programs
  , Program(..)
  , Snapshot
  , MemType(..)

  -- ** Scheduling
  , module Test.DejaFu.Schedule

  -- * Results
  , Condition(..)
  , Trace
  , Decision(..)
  , ThreadId(..)
  , ThreadAction(..)
  , Lookahead(..)
  , MVarId
  , IORefId
  , MaskingState(..)
  , showTrace
  , showCondition
  ) where

import           Control.Exception                 (MaskingState(..))
import qualified Control.Monad.Catch               as Ca
import           Control.Monad.Fail                (MonadFail)
import qualified Control.Monad.IO.Class            as IO
import           Control.Monad.Trans.Class         (MonadTrans(..))
import qualified Data.Foldable                     as F
import           Test.DejaFu.Schedule

import qualified Control.Monad.Conc.Class          as C
import           Test.DejaFu.Conc.Internal
import           Test.DejaFu.Conc.Internal.Common
import           Test.DejaFu.Conc.Internal.Program
import           Test.DejaFu.Conc.Internal.STM     (ModelSTM)
import           Test.DejaFu.Internal
import           Test.DejaFu.Types
import           Test.DejaFu.Utils

-- | @since 1.4.0.0
newtype ConcT n a = C { unC :: ModelConc n a }
  deriving (Functor, Applicative, Monad, MonadFail)

-- | A 'MonadConc' implementation using @IO@.
--
-- @since 0.4.0.0
type ConcIO = ConcT IO

toConc :: ((a -> Action n) -> Action n) -> ConcT n a
toConc = C . ModelConc

wrap :: (ModelConc n a -> ModelConc n a) -> ConcT n a -> ConcT n a
wrap f = C . f . unC

-- | @since 1.0.0.0
instance IO.MonadIO n => IO.MonadIO (ConcT n) where
  liftIO ma = toConc (\c -> ALift (fmap c (IO.liftIO ma)))

instance MonadTrans ConcT where
  lift ma = toConc (\c -> ALift (fmap c ma))

instance Ca.MonadCatch (ConcT n) where
  catch ma h = toConc (ACatching (unC . h) (unC ma))

instance Ca.MonadThrow (ConcT n) where
  throwM e = toConc (\_ -> AThrow e)

instance Ca.MonadMask (ConcT n) where
  mask                mb = toConc (AMasking MaskedInterruptible   (\f -> unC $ mb $ wrap f))
  uninterruptibleMask mb = toConc (AMasking MaskedUninterruptible (\f -> unC $ mb $ wrap f))

#if MIN_VERSION_exceptions(0,10,0)
  generalBracket acquire release use = Ca.mask $ \unmasked -> do
    resource <- acquire
    b <- unmasked (use resource) `Ca.catch` (\e -> release resource (Ca.ExitCaseException e) >> Ca.throwM e)
    c <- release resource (Ca.ExitCaseSuccess b)
    pure (b, c)
#elif MIN_VERSION_exceptions(0,9,0)
  -- from https://github.com/fpco/stackage/issues/3315#issuecomment-368583481
  generalBracket acquire release cleanup use = Ca.mask $ \unmasked -> do
    resource <- acquire
    result <- unmasked (use resource) `Ca.catch` (\e -> cleanup resource e >> Ca.throwM e)
    _ <- release resource
    pure result
#endif

instance Monad n => C.MonadConc (ConcT n) where
  type MVar     (ConcT n) = ModelMVar n
  type IORef    (ConcT n) = ModelIORef n
  type Ticket   (ConcT n) = ModelTicket
  type STM      (ConcT n) = ModelSTM n
  type ThreadId (ConcT n) = ThreadId

  -- ----------

  forkWithUnmaskN   n ma = toConc (AFork n (\umask -> runModelConc (unC $ ma $ wrap umask) (\_ -> AStop (pure ()))))
  forkOnWithUnmaskN n _  = C.forkWithUnmaskN n
  forkOSWithUnmaskN n ma
    | C.rtsSupportsBoundThreads =
      toConc (AForkOS n (\umask -> runModelConc (unC $ ma $ wrap umask) (\_ -> AStop (pure ()))))
    | otherwise = fail "RTS doesn't support multiple OS threads (use ghc -threaded when linking)"

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

  newIORefN n a = toConc (ANewIORef n a)

  readIORef   ref = toConc (AReadIORef    ref)
  readForCAS ref = toConc (AReadIORefCas ref)

  peekTicket' _ = ticketVal

  writeIORef ref      a = toConc (\c -> AWriteIORef ref a (c ()))
  casIORef   ref tick a = toConc (ACasIORef ref tick a)

  atomicModifyIORef ref f = toConc (AModIORef    ref f)
  modifyIORefCAS    ref f = toConc (AModIORefCas ref f)

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

-------------------------------------------------------------------------------
-- Setup & teardown

-- | A basic program with no set-up or tear-down.  This is just to
-- resolve type ambiguity if needed.
--
-- @since unreleased
basic :: ConcT n a -> ConcT n a
basic = id

-- | A concurrent program with some set-up action.
--
-- In terms of results, this is the same as @setup >>= program@.
-- However, the setup action will be __snapshotted__ (see
-- 'recordSnapshot' and 'runSnapshot') by the testing functions.  This
-- means that even if dejafu runs this program many many times, the
-- setup action will only be run the first time, and its effects
-- remembered for subsequent executions.
--
-- @since unreleased
withSetup
  :: ConcT n x
  -- ^ Setup action
  -> (x -> ConcT n a)
  -- ^ Main program
  -> WithSetup x n a
withSetup (C setup) p = WithSetup
  { wsSetup   = setup
  , wsProgram = \x -> unC (p x)
  }

-- | A concurrent program with some set-up and teardown actions.
--
-- This is similar to
--
-- @
-- do
--   x <- setup
--   y <- program x
--   teardown x y
-- @
--
-- But with two differences:
--
--   * The setup action can be __snapshotted__, as described for
--     'withSetup'
--
--   * The teardown action will be executed even if the main action
--     fails to produce a value.
--
-- @since unreleased
withTeardown
  :: (x -> Either Condition a -> ConcT n y)
  -- ^ Teardown action
  -> WithSetup x n a
  -- ^ Main program
  -> WithSetupAndTeardown x a n y
withTeardown teardown ws = WithSetupAndTeardown
  { wstSetup    = wsSetup ws
  , wstProgram  = wsProgram ws
  , wstTeardown = \x efa -> unC (teardown x efa)
  }

-- | A combination of 'withSetup' and 'withTeardown' for convenience.
--
-- @
-- withSetupAndTeardown setup teardown =
--   withTeardown teardown . withSetup setup
-- @
--
-- @since unreleased
withSetupAndTeardown
  :: ConcT n x
  -- ^ Setup action
  -> (x -> Either Condition a -> ConcT n y)
  -- ^ Teardown action
  -> (x -> ConcT n a)
  -- ^ Main program
  -> WithSetupAndTeardown x a n y
withSetupAndTeardown setup teardown =
  withTeardown teardown . withSetup setup

-------------------------------------------------------------------------------
-- Programs

-- | Does not support snapshotting at all.
instance Program ConcT where
  runConcurrent sched memtype s ma = do
    res <- runConcurrency False sched memtype s initialIdSource 2 (unC ma)
    out <- efromJust <$> C.readIORef (finalRef res)
    pure ( out
         , cSchedState (finalContext res)
         , F.toList (finalTrace res)
         )

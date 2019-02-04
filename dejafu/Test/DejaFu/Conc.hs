-- |
-- Module      : Test.DejaFu.Conc
-- Copyright   : (c) 2016--2019 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Deterministic traced execution of concurrent computations.
--
-- This works by executing the computation on a single thread, calling
-- out to the supplied scheduler after each step to determine which
-- thread runs next.
module Test.DejaFu.Conc
  ( -- * Expressing concurrent programs
    Program
  , Basic
  , ConcT
  , ConcIO

  -- ** Setup and teardown
  , WithSetup
  , WithSetupAndTeardown
  , withSetup
  , withTeardown
  , withSetupAndTeardown

  -- ** Invariants
  , Invariant
  , registerInvariant
  , inspectIORef
  , inspectMVar
  , inspectTVar

  -- * Executing concurrent programs
  , Snapshot
  , MemType(..)
  , runConcurrent
  , recordSnapshot
  , runSnapshot

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

import           Test.DejaFu.Conc.Internal.Common
import           Test.DejaFu.Conc.Internal.Program
import           Test.DejaFu.Conc.Internal.STM     (ModelTVar)
import           Test.DejaFu.Schedule
import           Test.DejaFu.Types
import           Test.DejaFu.Utils

-------------------------------------------------------------------------------
-- Expressing concurrent programs

-- | @since 1.4.0.0
type ConcT = Program Basic

-- | A 'MonadConc' implementation using @IO@.
--
-- @since 0.4.0.0
type ConcIO = ConcT IO

-------------------------------------------------------------------------------
-- Setup & teardown

-- | A concurrent program with some set-up action.
--
-- In terms of results, this is the same as @setup >>= program@.
-- However, the setup action will be __snapshotted__ (see
-- 'recordSnapshot' and 'runSnapshot') by the testing functions.  This
-- means that even if dejafu runs this program many many times, the
-- setup action will only be run the first time, and its effects
-- remembered for subsequent executions.
--
-- @since 2.0.0.0
withSetup
  :: Program Basic n x
  -- ^ Setup action
  -> (x -> Program Basic n a)
  -- ^ Main program
  -> Program (WithSetup x) n a
withSetup setup p = WithSetup
  { wsSetup   = setup
  , wsProgram = p
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
-- @since 2.0.0.0
withTeardown
  :: (x -> Either Condition y -> Program Basic n a)
  -- ^ Teardown action
  -> Program (WithSetup x) n y
  -- ^ Main program
  -> Program (WithSetupAndTeardown x y) n a
withTeardown teardown ws = WithSetupAndTeardown
  { wstSetup    = wsSetup ws
  , wstProgram  = wsProgram ws
  , wstTeardown = teardown
  }

-- | A combination of 'withSetup' and 'withTeardown' for convenience.
--
-- @
-- withSetupAndTeardown setup teardown =
--   withTeardown teardown . withSetup setup
-- @
--
-- @since 2.0.0.0
withSetupAndTeardown
  :: Program Basic n x
  -- ^ Setup action
  -> (x -> Either Condition y -> Program Basic n a)
  -- ^ Teardown action
  -> (x -> Program Basic n y)
  -- ^ Main program
  -> Program (WithSetupAndTeardown x y) n a
withSetupAndTeardown setup teardown =
  withTeardown teardown . withSetup setup

-------------------------------------------------------------------------------
-- Invariants

-- | Call this in the setup phase to register new invariant which will
-- be checked after every scheduling point in the main phase.
-- Invariants are atomic actions which can inspect the shared state of
-- your computation.
--
-- If the invariant throws an exception, the execution will be aborted
-- with n @InvariantFailure@.  Any teardown action will still be run.
--
-- @since 2.0.0.0
registerInvariant :: Invariant n a -> Program Basic n ()
registerInvariant inv = ModelConc (\c -> ANewInvariant (() <$ inv) (c ()))

-- | Read the content of an @IORef@.
--
-- This returns the globally visible value, which may not be the same
-- as the value visible to any particular thread when using a memory
-- model other than 'SequentialConsistency'.
--
-- @since 2.0.0.0
inspectIORef :: ModelIORef n a -> Invariant n a
inspectIORef = Invariant . IInspectIORef

-- | Read the content of an @MVar@.
--
-- This is essentially @tryReadMVar@.
--
-- @since 2.0.0.0
inspectMVar :: ModelMVar n a -> Invariant n (Maybe a)
inspectMVar = Invariant . IInspectMVar

-- | Read the content of a @TVar@.
--
-- This is essentially @readTVar@.
--
-- @since 2.0.0.0
inspectTVar :: ModelTVar n a -> Invariant n a
inspectTVar = Invariant . IInspectTVar

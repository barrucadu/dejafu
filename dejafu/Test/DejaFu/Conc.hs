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
-- @since unreleased
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
-- @since unreleased
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
-- @since unreleased
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

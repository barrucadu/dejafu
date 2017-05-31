-- |
-- Module      : Test.DejaFu.Defaults
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Default parameters for test execution.
module Test.DejaFu.Defaults where

import           Test.DejaFu.Common
import           Test.DejaFu.SCT

-- | A default way to execute concurrent programs: systematically
-- using 'defaultBounds'.
--
-- @since 0.6.0.0
defaultWay :: Way
defaultWay = Systematically defaultBounds

-- | The default memory model: @TotalStoreOrder@
--
-- @since 0.2.0.0
defaultMemType :: MemType
defaultMemType = TotalStoreOrder

-- | All bounds enabled, using their default values.
--
-- @since 0.2.0.0
defaultBounds :: Bounds
defaultBounds = Bounds
  { boundPreemp = Just defaultPreemptionBound
  , boundFair   = Just defaultFairBound
  , boundLength = Just defaultLengthBound
  }

-- | A sensible default preemption bound: 2.
--
-- See /Concurrency Testing Using Schedule Bounding: an Empirical Study/,
-- P. Thomson, A. F. Donaldson, A. Betts for justification.
--
-- @since 0.2.0.0
defaultPreemptionBound :: PreemptionBound
defaultPreemptionBound = 2

-- | A sensible default fair bound: 5.
--
-- This comes from playing around myself, but there is probably a
-- better default.
--
-- @since 0.2.0.0
defaultFairBound :: FairBound
defaultFairBound = 5

-- | A sensible default length bound: 250.
--
-- Based on the assumption that anything which executes for much
-- longer (or even this long) will take ages to test.
--
-- @since 0.2.0.0
defaultLengthBound :: LengthBound
defaultLengthBound = 250

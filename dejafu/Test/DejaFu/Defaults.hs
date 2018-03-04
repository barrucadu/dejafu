{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.DejaFu.Defaults
-- Copyright   : (c) 2017--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : ScopedTypeVariables
--
-- Default parameters for test execution.
module Test.DejaFu.Defaults where

import           Test.DejaFu.SCT
import           Test.DejaFu.Types

-- | Default SCT settings: just combine all the other defaults.
--
-- @since unreleased
defaultSettings :: Applicative n => Settings n a
defaultSettings = fromWayAndMemType defaultWay defaultMemType

-- | A default way to execute concurrent programs: systematically
-- using 'defaultBounds'.
--
-- @since 0.6.0.0
defaultWay :: Way
defaultWay = systematically defaultBounds

-- | Do not discard any results.
--
-- @since 0.7.1.0
defaultDiscarder :: Either Failure a -> Maybe Discard
defaultDiscarder = const Nothing

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

-- | Show a value for debugging purposes: @const "_"@.
--
-- If you want debugging output, you will probably want to change
-- this.
--
-- @since unreleased
defaultDebugShow :: forall a. a -> String
defaultDebugShow = get ldebugShow (defaultSettings :: Settings IO a)

-- | Print a message for debugging purposes: @Nothing@.
--
-- If you want debugging output, you must change this.
--
-- @since unreleased
defaultDebugPrint :: Applicative n => Maybe (String -> n ())
defaultDebugPrint = get ldebugPrint defaultSettings

-- | Terminate SCT early, as soon as a result matching the predicate
-- is found: @const False@.
--
-- @since unreleased
defaultEarlyExit :: forall a. Either Failure a -> Bool
defaultEarlyExit = get learlyExit (defaultSettings :: Settings IO a)

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.DejaFu.Settings
-- Copyright   : (c) 2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : RankNTypes, ScopedTypeVariables
--
-- Configuration for the SCT functions.
module Test.DejaFu.Settings
  ( -- * SCT configuration
    Settings
  , defaultSettings
  , fromWayAndMemType

  -- ** The @Way@
  , Way
  , defaultWay
  , lway
  , systematically
  , randomly
  , uniformly
  , swarmy

  -- *** Schedule bounds
  , Bounds(..)
  , PreemptionBound(..)
  , FairBound(..)
  , LengthBound(..)
  , defaultBounds
  , defaultPreemptionBound
  , defaultFairBound
  , defaultLengthBound
  , noBounds

  -- ** The @MemType@
  , MemType(..)
  , defaultMemType
  , lmemtype

  -- ** Discard functions
  , Discard(..)
  , defaultDiscarder
  , ldiscard

  -- ** Early exit
  , defaultEarlyExit
  , learlyExit

  -- ** Debug output
  , defaultDebugShow
  , defaultDebugPrint
  , ldebugShow
  , ldebugPrint

  -- * Lens helpers
  , get
  , set
  ) where

import           Control.Applicative   (Const(..))
import           Data.Functor.Identity (Identity(..))
import           System.Random         (RandomGen)

import           Test.DejaFu.Internal  (Settings(..), Way(..))
import           Test.DejaFu.Types

-------------------------------------------------------------------------------
-- SCT configuration

-- | Default SCT settings: just combine all the other defaults.
--
-- @since unreleased
defaultSettings :: Applicative n => Settings n a
defaultSettings = fromWayAndMemType defaultWay defaultMemType

-- | Construct a 'Settings' record from a 'Way' and a 'MemType'.
--
-- All other settings take on their default values.
--
-- @since unreleased
fromWayAndMemType :: Applicative n => Way -> MemType -> Settings n a
fromWayAndMemType way memtype = Settings
  { _way = way
  , _memtype = memtype
  , _discard = const Nothing
  , _debugShow = const "_"
  , _debugPrint = Nothing
  , _earlyExit = const False
  }

-------------------------------------------------------------------------------
-- The @Way@

-- | A default way to execute concurrent programs: systematically
-- using 'defaultBounds'.
--
-- @since 0.6.0.0
defaultWay :: Way
defaultWay = systematically defaultBounds

-- | A lens into the 'Way'.
--
-- @since unreleased
lway :: Lens' (Settings n a) Way
lway afb s = (\b -> s {_way = b}) <$> afb (_way s)

-- | Systematically execute a program, trying all distinct executions
-- within the bounds.
--
-- @since 0.7.0.0
systematically
  :: Bounds
  -- ^ The bounds to constrain the exploration.
  -> Way
systematically = Systematic

-- | Randomly execute a program, exploring a fixed number of
-- executions.
--
-- Threads are scheduled by a weighted random selection, where weights
-- are assigned randomly on thread creation.
--
-- This is not guaranteed to find all distinct results (unlike
-- 'systematically').
--
-- @since 0.7.0.0
randomly :: RandomGen g
  => g
  -- ^ The random generator to drive the scheduling.
  -> Int
  -- ^ The number of executions to try.
  -> Way
randomly g lim = swarmy g lim 1

-- | Randomly execute a program, exploring a fixed number of
-- executions.
--
-- Threads are scheduled by a uniform random selection.
--
-- This is not guaranteed to find all distinct results (unlike
-- 'systematically').
--
-- @since 0.7.0.0
uniformly :: RandomGen g
  => g
  -- ^ The random generator to drive the scheduling.
  -> Int
  -- ^ The number of executions to try.
  -> Way
uniformly = Uniform

-- | Randomly execute a program, exploring a fixed number of
-- executions.
--
-- Threads are scheduled by a weighted random selection, where weights
-- are assigned randomly on thread creation.
--
-- This is not guaranteed to find all distinct results (unlike
-- 'systematically').
--
-- @since 0.7.0.0
swarmy :: RandomGen g
  => g
  -- ^ The random generator to drive the scheduling.
  -> Int
  -- ^ The number of executions to try.
  -> Int
  -- ^ The number of executions to use the thread weights for.
  -> Way
swarmy = Weighted

-------------------------------------------------------------------------------
-- Schedule bounds

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

-- | No bounds enabled. This forces the scheduler to just use
-- partial-order reduction and sleep sets to prune the search
-- space. This will /ONLY/ work if your computation always terminates!
--
-- @since 0.3.0.0
noBounds :: Bounds
noBounds = Bounds
  { boundPreemp = Nothing
  , boundFair   = Nothing
  , boundLength = Nothing
  }

-------------------------------------------------------------------------------
-- The @MemType@

-- | The default memory model: @TotalStoreOrder@
--
-- @since 0.2.0.0
defaultMemType :: MemType
defaultMemType = TotalStoreOrder

-- | A lens into the 'MemType'.
--
-- @since unreleased
lmemtype :: Lens' (Settings n a) MemType
lmemtype afb s = (\b -> s {_memtype = b}) <$> afb (_memtype s)

-------------------------------------------------------------------------------
-- Discard functions

-- | Do not discard any results.
--
-- @since 0.7.1.0
defaultDiscarder :: Either Failure a -> Maybe Discard
defaultDiscarder = const Nothing

-- | A lens into the discard function.
--
-- @since unreleased
ldiscard :: Lens' (Settings n a) (Either Failure a -> Maybe Discard)
ldiscard afb s = (\b -> s {_discard = b}) <$> afb (_discard s)

-------------------------------------------------------------------------------
-- Early exit

-- | Terminate SCT early, as soon as a result matching the predicate
-- is found: @const False@.
--
-- @since unreleased
defaultEarlyExit :: forall a. Either Failure a -> Bool
defaultEarlyExit = get learlyExit (defaultSettings :: Settings IO a)

-- | A lens into the early-exit predicate.
--
-- @since unreleased
learlyExit :: Lens' (Settings n a) (Either Failure a -> Bool)
learlyExit afb s = (\b -> s {_earlyExit = b}) <$> afb (_earlyExit s)

-------------------------------------------------------------------------------
-- Debug output

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

-- | A lens into the debug 'show' function.
--
-- @since unreleased
ldebugShow :: Lens' (Settings n a) (a -> String)
ldebugShow afb s = (\b -> s {_debugShow = b}) <$> afb (_debugShow s)

-- | A lens into the debug 'print' function.
--
-- @since unreleased
ldebugPrint :: Lens' (Settings n a) (Maybe (String -> n ()))
ldebugPrint afb s = (\b -> s {_debugPrint = b}) <$> afb (_debugPrint s)

-------------------------------------------------------------------------------
-- Lens helpers

-- lens type synonyms, unexported
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- | Get a value from a lens.
--
-- @since unreleased
get :: Lens' s a -> s -> a
get lens = getConst . lens Const

-- | Set a value in a lens.
--
-- @since unreleased
set :: Lens' s a -> a -> s -> s
set lens a = runIdentity . lens (\_ -> Identity a)

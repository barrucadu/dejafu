{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unit where

import           Data.Proxy          (Proxy(..))
import           Test.Tasty          (askOption, localOption)
import           Test.Tasty.Hedgehog (HedgehogDiscardLimit(..),
                                      HedgehogShrinkLimit(..),
                                      HedgehogShrinkRetries(..),
                                      HedgehogTestLimit)
import           Test.Tasty.Options  (IsOption(..), OptionDescription(..))
import           Text.Read           (readMaybe)

import qualified Unit.Properties     as P

import           Common

-- | Run all the unit tests.
tests :: [TestTree]
tests = map applyHedgehogOptions
  [ testGroup "Properties" P.tests
  ]

-- | Tasty options
options :: [OptionDescription]
options =
  [ Option (Proxy :: Proxy UnitHedgehogTestLimit)
  , Option (Proxy :: Proxy UnitHedgehogDiscardLimit)
  , Option (Proxy :: Proxy UnitHedgehogShrinkLimit)
  , Option (Proxy :: Proxy UnitHedgehogShrinkRetries)
  ]


-------------------------------------------------------------------------------
-- Hedgehog options

-- | The number of successful test cases required before Hedgehog will pass a test
newtype UnitHedgehogTestLimit = UnitHedgehogTestLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance IsOption UnitHedgehogTestLimit where
  defaultValue = 1500
  parseValue = fmap UnitHedgehogTestLimit . readMaybe
  optionName = pure "unit-hedgehog-tests"
  optionHelp = pure "hedgehog-tests for the unit tests"

-- | The number of discarded cases allowed before Hedgehog will fail a test
newtype UnitHedgehogDiscardLimit = UnitHedgehogDiscardLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance IsOption UnitHedgehogDiscardLimit where
  defaultValue = 1000
  parseValue = fmap UnitHedgehogDiscardLimit . readMaybe
  optionName = pure "unit-hedgehog-discards"
  optionHelp = pure "hedgehog-discards for the unit tests"

-- | The number of shrinks allowed before Hedgehog will fail a test
newtype UnitHedgehogShrinkLimit = UnitHedgehogShrinkLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance IsOption UnitHedgehogShrinkLimit where
  defaultValue =
    let HedgehogShrinkLimit d = defaultValue
    in fromIntegral d
  parseValue = fmap UnitHedgehogShrinkLimit . readMaybe
  optionName = pure "unit-hedgehog-shrinks"
  optionHelp = pure "hedgehog-shrinks for the unit tests"

-- | The number of times to re-run a test during shrinking
newtype UnitHedgehogShrinkRetries = UnitHedgehogShrinkRetries Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance IsOption UnitHedgehogShrinkRetries where
  defaultValue =
    let HedgehogShrinkRetries d = defaultValue
    in fromIntegral d
  parseValue = fmap UnitHedgehogShrinkRetries . readMaybe
  optionName = pure "unit-hedgehog-retries"
  optionHelp = pure "hedgehog-retries for the unit tests"

-- | Apply the Hedgehog options.
applyHedgehogOptions :: TestTree -> TestTree
applyHedgehogOptions tt0 =
  askOption $ \(UnitHedgehogTestLimit tl) ->
  askOption $ \(UnitHedgehogDiscardLimit dl) ->
  askOption $ \(UnitHedgehogShrinkLimit sl) ->
  askOption $ \(UnitHedgehogShrinkRetries sr) ->
  localOption (fromIntegral tl :: HedgehogTestLimit) $
  localOption (fromIntegral dl :: HedgehogDiscardLimit) $
  localOption (fromIntegral sl :: HedgehogShrinkLimit) $
  localOption (fromIntegral sr :: HedgehogShrinkRetries) tt0

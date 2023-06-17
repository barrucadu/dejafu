module Unit where

import           Data.Maybe          (fromJust)
import           Data.Proxy          (Proxy(..))
import           Test.Tasty          (askOption, localOption)
import           Test.Tasty.Hedgehog (HedgehogDiscardLimit(..),
                                      HedgehogShrinkLimit(..),
                                      HedgehogShrinkRetries(..),
                                      HedgehogTestLimit)
import           Test.Tasty.Options  (IsOption(..), OptionDescription(..))

import qualified Unit.Predicates     as PE
import qualified Unit.Properties     as PO

import           Common

-- | Run all the unit tests.
tests :: [TestTree]
tests = map applyHedgehogOptions
  [ testGroup "Predicates" PE.tests
  , testGroup "Properties" PO.tests
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
newtype UnitHedgehogTestLimit = UnitHedgehogTestLimit HedgehogTestLimit
  deriving (Eq, Ord, Show)

instance IsOption UnitHedgehogTestLimit where
  defaultValue = UnitHedgehogTestLimit . fromJust $ parseValue "1500"
  parseValue = fmap UnitHedgehogTestLimit . parseValue
  optionName = pure "unit-hedgehog-tests"
  optionHelp = pure "hedgehog-tests for the unit tests"

-- | The number of discarded cases allowed before Hedgehog will fail a test
newtype UnitHedgehogDiscardLimit = UnitHedgehogDiscardLimit HedgehogDiscardLimit
  deriving (Eq, Ord, Show)

instance IsOption UnitHedgehogDiscardLimit where
  defaultValue = UnitHedgehogDiscardLimit . fromJust $ parseValue "1000"
  parseValue = fmap UnitHedgehogDiscardLimit . parseValue
  optionName = pure "unit-hedgehog-discards"
  optionHelp = pure "hedgehog-discards for the unit tests"

-- | The number of shrinks allowed before Hedgehog will fail a test
newtype UnitHedgehogShrinkLimit = UnitHedgehogShrinkLimit HedgehogShrinkLimit
  deriving (Eq, Ord, Show)

instance IsOption UnitHedgehogShrinkLimit where
  defaultValue = UnitHedgehogShrinkLimit defaultValue
  parseValue = fmap UnitHedgehogShrinkLimit . parseValue
  optionName = pure "unit-hedgehog-shrinks"
  optionHelp = pure "hedgehog-shrinks for the unit tests"

-- | The number of times to re-run a test during shrinking
newtype UnitHedgehogShrinkRetries = UnitHedgehogShrinkRetries HedgehogShrinkRetries
  deriving (Eq, Ord, Show)

instance IsOption UnitHedgehogShrinkRetries where
  defaultValue = UnitHedgehogShrinkRetries defaultValue
  parseValue = fmap UnitHedgehogShrinkRetries . parseValue
  optionName = pure "unit-hedgehog-retries"
  optionHelp = pure "hedgehog-retries for the unit tests"

-- | Apply the Hedgehog options.
applyHedgehogOptions :: TestTree -> TestTree
applyHedgehogOptions tt0 =
  askOption $ \(UnitHedgehogTestLimit tl) ->
  askOption $ \(UnitHedgehogDiscardLimit dl) ->
  askOption $ \(UnitHedgehogShrinkLimit sl) ->
  askOption $ \(UnitHedgehogShrinkRetries sr) ->
  localOption tl $
  localOption dl $
  localOption sl $
  localOption sr tt0

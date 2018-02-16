{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Examples where

import           Data.Proxy            (Proxy(..))
import           Test.Tasty            (askOption, localOption)
import           Test.Tasty.Hedgehog   (HedgehogDiscardLimit(..),
                                        HedgehogShrinkLimit(..),
                                        HedgehogShrinkRetries(..),
                                        HedgehogTestLimit)
import           Test.Tasty.Options    (IsOption(..), OptionDescription(..))
import           Text.Read             (readMaybe)

import qualified Examples.AutoUpdate   as A
import qualified Examples.ClassLaws    as C
import qualified Examples.Logger       as L
import qualified Examples.ParMonad     as PM
import qualified Examples.Philosophers as P
import qualified Examples.SearchParty  as S

import           Common

-- | Run all the example tests.
tests :: [TestTree]
tests = map applyHedgehogOptions
  [ testGroup "AutoUpdate"   A.tests
  , testGroup "ClassLaws"    C.tests
  , testGroup "Logger"       L.tests
  , testGroup "ParMonad"     PM.tests
  , testGroup "Philosophers" P.tests
  , testGroup "SearchParty"  S.tests
  ]

-- | Tasty options
options :: [OptionDescription]
options =
  [ Option (Proxy :: Proxy ExampleHedgehogTestLimit)
  , Option (Proxy :: Proxy ExampleHedgehogDiscardLimit)
  , Option (Proxy :: Proxy ExampleHedgehogShrinkLimit)
  , Option (Proxy :: Proxy ExampleHedgehogShrinkRetries)
  ]


-------------------------------------------------------------------------------
-- Hedgehog options

-- | The number of successful test cases required before Hedgehog will pass a test
newtype ExampleHedgehogTestLimit = ExampleHedgehogTestLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance IsOption ExampleHedgehogTestLimit where
  defaultValue = 25
  parseValue = fmap ExampleHedgehogTestLimit . readMaybe
  optionName = pure "example-hedgehog-tests"
  optionHelp = pure "hedgehog-tests for the example tests"

-- | The number of discarded cases allowed before Hedgehog will fail a test
newtype ExampleHedgehogDiscardLimit = ExampleHedgehogDiscardLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance IsOption ExampleHedgehogDiscardLimit where
  defaultValue =
    let HedgehogDiscardLimit d = defaultValue
    in fromIntegral d
  parseValue = fmap ExampleHedgehogDiscardLimit . readMaybe
  optionName = pure "example-hedgehog-discards"
  optionHelp = pure "hedgehog-discards for the example tests"

-- | The number of shrinks allowed before Hedgehog will fail a test
newtype ExampleHedgehogShrinkLimit = ExampleHedgehogShrinkLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance IsOption ExampleHedgehogShrinkLimit where
  defaultValue =
    let HedgehogShrinkLimit d = defaultValue
    in fromIntegral d
  parseValue = fmap ExampleHedgehogShrinkLimit . readMaybe
  optionName = pure "example-hedgehog-shrinks"
  optionHelp = pure "hedgehog-shrinks for the example tests"

-- | The number of times to re-run a test during shrinking
newtype ExampleHedgehogShrinkRetries = ExampleHedgehogShrinkRetries Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance IsOption ExampleHedgehogShrinkRetries where
  defaultValue =
    let HedgehogShrinkRetries d = defaultValue
    in fromIntegral d
  parseValue = fmap ExampleHedgehogShrinkRetries . readMaybe
  optionName = pure "example-hedgehog-retries"
  optionHelp = pure "hedgehog-retries for the example tests"

-- | Apply the Hedgehog options.
applyHedgehogOptions :: TestTree -> TestTree
applyHedgehogOptions tt0 =
  askOption $ \(ExampleHedgehogTestLimit tl) ->
  askOption $ \(ExampleHedgehogDiscardLimit dl) ->
  askOption $ \(ExampleHedgehogShrinkLimit sl) ->
  askOption $ \(ExampleHedgehogShrinkRetries sr) ->
  localOption (fromIntegral tl :: HedgehogTestLimit) $
  localOption (fromIntegral dl :: HedgehogDiscardLimit) $
  localOption (fromIntegral sl :: HedgehogShrinkLimit) $
  localOption (fromIntegral sr :: HedgehogShrinkRetries) tt0

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Tasty.LeanCheck where

import           Data.Proxy           (Proxy(..))
import qualified Test.LeanCheck       as LeanCheck
import           Test.Tasty.Options
import           Test.Tasty.Providers
import           Text.Printf          (printf)
import           Text.Read            (readMaybe)

-- | Create a 'Test' for a LeanCheck 'LeanCheck.Testable' property.
testProperty :: LeanCheck.Testable p => TestName -> p -> TestTree
testProperty name = singleTest name . LeanCheckTest

-- | The number of tests for LeanCheck to try.
newtype LeanCheckTests = LeanCheckTests Int
  deriving (Num, Ord, Eq, Real, Enum, Integral)

-- | A LeanCheck test.
data LeanCheckTest where
  LeanCheckTest :: LeanCheck.Testable p => p -> LeanCheckTest

instance IsOption LeanCheckTests where
  defaultValue = 2500
  parseValue = fmap LeanCheckTests . readMaybe
  optionName = pure "leancheck-tests"
  optionHelp = pure "Tests to use for leancheck tests"

instance IsTest LeanCheckTest where
  testOptions = pure [Option (Proxy :: Proxy LeanCheckTests)]

  run opts (LeanCheckTest prop) _ = pure $
    let LeanCheckTests tests = lookupOption opts
    in case LeanCheck.counterExample tests prop of
      Just ce -> testFailed (printf "*** Failed! Counter example:\n%s" (unlines ce))
      Nothing -> testPassed (printf "+++ OK, passed %d tests." tests)

module Unit where

import           Test.Tasty.Options (OptionDescription)

import qualified Unit.Properties    as P

import           Common

-- | Run all the unit tests.
tests :: [TestTree]
tests =
  [ testGroup "Properties" P.tests
  ]

-- | Tasty options
options :: [OptionDescription]
options = []

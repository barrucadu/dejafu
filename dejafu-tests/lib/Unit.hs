module Unit where

import qualified Unit.Properties as P

import Common

-- | Run all the unit tests.
tests :: [TestTree]
tests =
  [ testGroup "Properties" P.tests
  ]

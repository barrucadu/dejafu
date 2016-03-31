module Examples where

import Test.Framework (Test, testGroup)

import qualified Examples.AutoUpdate   as A
import qualified Examples.ClassLaws    as C
import qualified Examples.Logger       as L
import qualified Examples.Philosophers as P

-- | Run all the example tests.
testExamples :: [Test]
testExamples = map (uncurry testGroup)
  [ ("auto-update",         A.tests)
  , ("Class Laws",          C.tests)
  , ("Dining Philosophers", P.tests)
  , ("Message Logger",      L.tests)
  ]

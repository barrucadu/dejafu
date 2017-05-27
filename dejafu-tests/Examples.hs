module Examples where

import Test.Framework (Test, testGroup)

import qualified Examples.AutoUpdate   as A
import qualified Examples.ClassLaws    as C
import qualified Examples.Logger       as L
import qualified Examples.Philosophers as P
import qualified Examples.SearchParty  as S

import Utils (tg)

-- | Run all the example tests.
testExamples :: [Test]
testExamples =
  [ tg        "auto-update"         A.tests
  , testGroup "Class Laws"          C.tests
  , testGroup "Dining Philosophers" P.tests
  , tg        "Message Logger"      L.tests
  , testGroup "Search Party"        S.tests
  ]

module Examples where

import qualified Examples.AutoUpdate   as A
import qualified Examples.ClassLaws    as C
import qualified Examples.Logger       as L
import qualified Examples.Philosophers as P
import qualified Examples.SearchParty  as S
import qualified Examples.ParMonad     as PM

import Common

-- | Run all the example tests.
testExamples :: [Test]
testExamples =
  [ testGroup "auto-update"         A.tests
  , testGroup "Class Laws"          C.tests
  , testGroup "Dining Philosophers" P.tests
  , testGroup "Message Logger"      L.tests
  , testGroup "Search Party"        S.tests
  , testGroup "Par Monad (Direct Scheduler)" PM.tests
  ]

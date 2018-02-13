module Examples where

import qualified Examples.AutoUpdate   as A
import qualified Examples.ClassLaws    as C
import qualified Examples.Logger       as L
import qualified Examples.ParMonad     as PM
import qualified Examples.Philosophers as P
import qualified Examples.SearchParty  as S

import           Common

-- | Run all the example tests.
tests :: [TestTree]
tests =
  [ testGroup "AutoUpdate"   A.tests
  , testGroup "ClassLaws"    C.tests
  , testGroup "Logger"       L.tests
  , testGroup "ParMonad"     PM.tests
  , testGroup "Philosophers" P.tests
  , testGroup "SearchParty"  S.tests
  ]

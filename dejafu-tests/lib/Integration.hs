module Integration where

import qualified Integration.Async          as A
import qualified Integration.Discard        as D
import qualified Integration.Litmus         as L
import qualified Integration.MultiThreaded  as M
import qualified Integration.Refinement     as R
import qualified Integration.Regressions    as G
import qualified Integration.SingleThreaded as S

import           Common

-- | Run all the integration tests.
tests :: [TestTree]
tests =
  [ testGroup "Async"          A.tests
  , testGroup "Discard"        D.tests
  , testGroup "Litmus"         L.tests
  , testGroup "MultiThreaded"  M.tests
  , testGroup "Refinement"     R.tests
  , testGroup "Regressions"    G.tests
  , testGroup "SingleThreaded" S.tests
  ]

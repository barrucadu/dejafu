module Integration where

import           Test.Tasty.Options         (OptionDescription)

import qualified Integration.Async          as A
import qualified Integration.Litmus         as L
import qualified Integration.MonadDejaFu    as MD
import qualified Integration.MultiThreaded  as M
import qualified Integration.Names          as N
import qualified Integration.Refinement     as R
import qualified Integration.Regressions    as G
import qualified Integration.SCT            as SC
import qualified Integration.SingleThreaded as S

import           Common

-- | Run all the integration tests.
tests :: [TestTree]
tests =
  [ testGroup "Async"          A.tests
  , testGroup "Litmus"         L.tests
  , testGroup "MultiThreaded"  M.tests
  , testGroup "MonadDejaFu"    MD.tests
  , testGroup "Names"          N.tests
  , testGroup "Refinement"     R.tests
  , testGroup "Regressions"    G.tests
  , testGroup "SingleThreaded" S.tests
  , testGroup "SCT"            SC.tests
  ]

-- | Tasty options
options :: [OptionDescription]
options = []

module Cases where

import qualified Cases.SingleThreaded as S
import qualified Cases.MultiThreaded  as M
import qualified Cases.Refinement     as R
import qualified Cases.Litmus         as L
import qualified Cases.Async          as A
import qualified Cases.Discard        as D

import Common

-- | Run all the test cases.
testCases :: [Test]
testCases =
  [ testGroup "Single Threaded" S.tests
  , testGroup "Multi Threaded"  M.tests
  , testGroup "Refinement"      R.tests
  , testGroup "Litmus"          L.tests
  , testGroup "Async"           A.tests
  , testGroup "Discard"         D.tests
  ]

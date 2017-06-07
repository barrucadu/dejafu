module Cases where

import Test.Framework (Test, testGroup)

import qualified Cases.SingleThreaded as S
import qualified Cases.MultiThreaded  as M
import qualified Cases.Refinement     as R
import qualified Cases.Litmus         as L
import qualified Cases.Async          as A

-- | Run all the test cases.
testCases :: [Test]
testCases = map (uncurry testGroup)
  [ ("Single Threaded", S.tests)
  , ("Multi Threaded",  M.tests)
  , ("Refinement",      R.tests)
  , ("Litmus",          L.tests)
  , ("Async",           A.tests)
  ]

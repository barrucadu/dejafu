module Unit.Predicates where

import qualified Test.DejaFu      as D
import           Test.Tasty.HUnit

import           Common

tests :: [TestTree]
tests =
  [ testGroup "alwaysSameBy"     alwaysSameBy
  , testGroup "notAlwaysSameBy"  notAlwaysSameBy
  , testGroup "alwaysNothing"    alwaysNothing
  , testGroup "somewhereNothing" somewhereNothing
  , testGroup "gives"            gives
  ]

-------------------------------------------------------------------------------

alwaysSameBy :: [TestTree]
alwaysSameBy = toTestList
  [ passes "Equal successes"   (D.alwaysSameBy (==)) [Right 1, Right 1, Right 1]
  , fails  "Unequal successes" (D.alwaysSameBy (==)) [Right 1, Right 2, Right 3]
  , fails  "Equal conditions"   (D.alwaysSameBy (==)) [Left D.Deadlock, Left D.Deadlock, Left D.Deadlock]
  , fails  "Unequal conditions" (D.alwaysSameBy (==)) [Left D.Deadlock, Left D.Abort, Left D.Abort]
  , fails  "Mixed conditions and successes" (D.alwaysSameBy (==)) [Left D.Deadlock, Right 1, Right 1]
  ]

-------------------------------------------------------------------------------

notAlwaysSameBy :: [TestTree]
notAlwaysSameBy = toTestList
  [ fails  "Equal successes"   (D.notAlwaysSameBy (==)) [Right 1, Right 1, Right 1]
  , passes "Unequal successes" (D.notAlwaysSameBy (==)) [Right 1, Right 2, Right 3]
  , fails  "Equal conditions"   (D.notAlwaysSameBy (==)) [Left D.Deadlock, Left D.Deadlock, Left D.Deadlock]
  , fails  "Unequal conditions" (D.notAlwaysSameBy (==)) [Left D.Deadlock, Left D.Abort, Left D.Abort]
  , fails  "Mixed conditions and successes" (D.notAlwaysSameBy (==)) [Left D.Deadlock, Right 1, Right 1]
  ]

-------------------------------------------------------------------------------

alwaysNothing :: [TestTree]
alwaysNothing = toTestList
  [ passes "Always"    (D.alwaysNothing (const Nothing)) [Right 1, Right 2, Left D.Deadlock]
  , fails  "Somewhere" (D.alwaysNothing (either (Just . Left) (const Nothing))) [Right 1, Right 2, Left D.Deadlock]
  , fails  "Never"     (D.alwaysNothing Just) [Right 1, Right 2, Left D.Deadlock]
  ]

-------------------------------------------------------------------------------

somewhereNothing :: [TestTree]
somewhereNothing = toTestList
  [ passes "Always"    (D.somewhereNothing (const Nothing)) [Right 1, Right 2, Left D.Deadlock]
  , passes "Somewhere" (D.somewhereNothing (either (Just . Left) (const Nothing))) [Right 1, Right 2, Left D.Deadlock]
  , fails  "Never"     (D.somewhereNothing Just) [Right 1, Right 2, Left D.Deadlock]
  ]

-------------------------------------------------------------------------------

gives :: [TestTree]
gives = toTestList
  [ passes "Exact match"     (D.gives [Right 1, Right 2]) [Right 1, Right 2]
  , fails  "Extra results"   (D.gives [Right 1, Right 2]) [Right 1, Right 2, Right 3]
  , fails  "Missing results" (D.gives [Right 1, Right 2]) [Right 1]
  ]

-------------------------------------------------------------------------------

-- | Check a predicate passes
passes :: String -> D.Predicate Int -> [Either D.Condition Int] -> TestTree
passes = checkPredicate D._pass

-- | Check a predicate fails
fails :: String -> D.Predicate Int -> [Either D.Condition Int] -> TestTree
fails = checkPredicate (not . D._pass)

-- | Check a predicate
checkPredicate :: (D.Result Int -> Bool) -> String -> D.Predicate Int -> [Either D.Condition Int] -> TestTree
checkPredicate f msg p = testCase msg . assertBool "" . f . D.peval p . map (\efa -> (efa, []))

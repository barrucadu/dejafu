module Main where

import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as T

import Cases
import Examples

main :: IO ()
main = T.defaultMain $ T.adjustOption reduceQCTests allTests

allTests :: T.TestTree
allTests = T.testGroup "Tests"
  [ T.testGroup "Cases"    testCases
  , T.testGroup "Examples" testExamples
  ]

-- | Reduce the default number of quickcheck runs.
reduceQCTests :: T.QuickCheckTests -> T.QuickCheckTests
reduceQCTests (T.QuickCheckTests n) = T.QuickCheckTests (min 25 n)

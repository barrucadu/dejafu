module Main where

import Test.Framework (Test, defaultMain, testGroup)

import Cases
import Examples

main :: IO ()
main = defaultMain allTests

allTests :: [Test]
allTests = map (uncurry testGroup)
  [ ("Test Cases", testCases)
  , ("Examples",   testExamples)
  ]

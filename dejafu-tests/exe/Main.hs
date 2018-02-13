module Main where

import qualified Test.Tasty            as T
import qualified Test.Tasty.QuickCheck as T

import qualified Examples              as E
import qualified Integration           as I
import qualified Unit                  as U

main :: IO ()
main = T.defaultMain $ T.adjustOption reduceQCTests tests

tests :: T.TestTree
tests = T.testGroup "Tests"
  [ T.testGroup "Unit" U.tests
  , T.testGroup "Integration" I.tests
  , T.testGroup "Examples" E.tests
  ]

-- | Reduce the default number of quickcheck runs.
reduceQCTests :: T.QuickCheckTests -> T.QuickCheckTests
reduceQCTests (T.QuickCheckTests n) = T.QuickCheckTests (min 25 n)

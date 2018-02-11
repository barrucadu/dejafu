module Main where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import qualified Test.Framework as T

import Cases
import Examples

main :: IO ()
main = do
  opts <- setDefaults <$> (T.interpretArgsOrExit =<< getArgs)
  T.defaultMainWithOpts allTests opts

allTests :: [T.Test]
allTests = map (uncurry T.testGroup)
  [ ("Test Cases", testCases)
  , ("Examples",   testExamples)
  ]

-- | Reduce the default number of quickcheck runs.
setDefaults :: T.RunnerOptions -> T.RunnerOptions
setDefaults opts = opts { T.ropt_test_options = set (T.ropt_test_options opts) } where
  set (Just opts) = Just (set' opts)
  set Nothing = Just (set' (T.TestOptions Nothing Nothing Nothing Nothing Nothing Nothing))

  set' opts = opts
    { T.topt_maximum_generated_tests = Just . fromMaybe 25 $
        T.topt_maximum_generated_tests opts
    }

module Main (main) where

import Control.Monad (when)
import Tests.Cases
import Tests.Utils
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  results <- mapM (runTest True) testCases
  if and results then exitSuccess else exitFailure

runTest :: Bool -> Test -> IO Bool
runTest verbose (Test {name = name, result = result}) = do
  res <- result
  case res of
    Pass      -> when verbose (putStrLn $ "\27[32m[pass]\27[0m " ++ name)  >> return True
    Fail str  -> putStrLn ("\27[31m[fail]\27[0m "  ++ name ++ ": " ++ str) >> return False
    Error str -> putStrLn ("\27[35m[error]\27[0m " ++ name ++ ": " ++ str) >> return False

module Main (main) where

import Control.Monad (when)
import Control.Monad.Conc.SCT.Tests (Result(..))
import Tests.Cases
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  results <- mapM (runTest True) testCases
  if and results then exitSuccess else exitFailure

runTest :: Bool -> Test -> IO Bool
runTest verbose (Test {name = name, result = result}) = do
  if _pass result
  then when verbose (putStrLn $ "\27[32m[pass]\27[0m " ++ name ++ " (checked: " ++ show (_casesChecked result) ++ ")")
  else putStrLn ("\27[31m[fail]\27[0m " ++ name ++ " (checked: " ++ show (_casesChecked result) ++ ")")
  return $ _pass result

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
  then
    -- If verbose, display a pass message.
    when verbose $
      putStrLn $ "\27[32m[pass]\27[0m " ++ name ++ " (checked: " ++ show (_casesChecked result) ++ ")"
  else do
    -- Display a failure message, and the first 3 failed traces
    putStrLn ("\27[31m[fail]\27[0m " ++ name ++ " (checked: " ++ show (_casesChecked result) ++ ")")
    mapM_ (\fail -> putStrLn $ "\t" ++ show fail) . take 3 $ _failures result
    when (length (_failures result) > 3) $
      putStrLn "\t..."

  return $ _pass result

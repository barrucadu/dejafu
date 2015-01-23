module Main (main) where

import Control.Monad.Conc.SCT.Tests (doTests)
import Tests.Cases
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  success <- doTests True testCases
  if success then exitSuccess else exitFailure

module Main (main) where

import Control.Monad.Conc.SCT.Tests (doTests')
import Tests.Cases
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  success <- doTests' id True testCases
  if success then exitSuccess else exitFailure

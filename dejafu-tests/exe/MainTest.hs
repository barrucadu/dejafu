module Main where

import           Test.Tasty (defaultMainWithIngredients)

import           Util

main :: IO ()
main = defaultMainWithIngredients ingredients tests

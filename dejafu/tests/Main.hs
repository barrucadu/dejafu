module Main (main) where

import Data.Functor (void)
import Test.HUnit (Test(..), runTestTT)

import qualified Cases.SingleThreaded as ST
import qualified Cases.MultiThreaded  as MT
import qualified Cases.Litmus         as L
import qualified Examples.Logger      as EL

main :: IO ()
main = void . runTestTT $ TestList
  [ TestLabel "Single Threaded" ST.tests
  , TestLabel "Multi Threaded"  MT.tests
  , TestLabel "Litmus Tests"    L.tests
  , TestLabel "Logger Example"  EL.tests
  ]

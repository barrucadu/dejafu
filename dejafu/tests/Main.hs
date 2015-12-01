module Main (main) where

import Test.Framework (defaultMain, testGroup)

import qualified Cases.SingleThreaded as ST
import qualified Cases.MultiThreaded  as MT
import qualified Cases.Litmus         as L
import qualified Examples.Logger      as EL

main :: IO ()
main = defaultMain
  [ testGroup "Test Cases"
    [ testGroup "Single Threaded" ST.tests
    , testGroup "Multi Threaded"  MT.tests
    , testGroup "Litmus"          L.tests
    ]
  , testGroup "Examples"
    [ testGroup "Message Logger" EL.tests
    ]
  ]

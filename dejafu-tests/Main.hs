module Main (main) where

import Test.Framework (defaultMain, testGroup)

import qualified Cases.SingleThreaded as ST
import qualified Cases.MultiThreaded  as MT
import qualified Cases.Litmus         as L
import qualified Examples.AutoUpdate  as EA
import qualified Examples.ClassLaws   as EC
import qualified Examples.Logger      as EL
import qualified Examples.Philosophers as EP

main :: IO ()
main = defaultMain
  [ testGroup "Test Cases"
    [ testGroup "Single Threaded" ST.tests
    , testGroup "Multi Threaded"  MT.tests
    , testGroup "Litmus"          L.tests
    ]
  , testGroup "Examples"
    [ testGroup "auto-update"    EA.tests
    , testGroup "Class Laws"     EC.tests
    , testGroup "Dining Philosophers" EP.tests
    , testGroup "Message Logger" EL.tests
    ]
  ]

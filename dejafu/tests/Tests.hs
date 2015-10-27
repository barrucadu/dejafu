module Main (main) where

import Data.Functor (void)
import Test.DejaFu hiding (MemType(..))
import Test.HUnit (Test(..), runTestTT, test)
import Test.HUnit.DejaFu

import qualified Tests.Cases  as C
import qualified Tests.Logger as L

tests :: Test
tests = TestList
  [ TestLabel "Simple" $ test
    [ testDejafu  C.simple2Deadlock "Simple 2-Deadlock"    deadlocksSometimes
    , testDejafu (C.philosophers 2) "2 Philosophers"       deadlocksSometimes
    , testDejafu (C.philosophers 3) "3 Philosophers"       deadlocksSometimes
    , testDejafu (C.philosophers 4) "4 Philosophers"       deadlocksSometimes
    , testDejafu  C.thresholdValue  "Threshold Value"      notAlwaysSame
    , testDejafu  C.forgottenUnlock "Forgotten Unlock"     deadlocksAlways
    , testDejafu  C.simple2Race     "Simple 2-Race"        notAlwaysSame
    , testDejafu  C.raceyStack      "Racey Stack"          notAlwaysSame
    , testDejafu  C.threadKill      "Kill Thread"          deadlocksSometimes
    , testDejafu  C.threadKillMask  "Kill Thread w/ Mask"  deadlocksNever
    , testDejafu  C.threadKillUmask "Kill Thread w/ Umask" deadlocksSometimes
    , testDejafu  C.stmAtomic       "STM Atomicity"      $ gives' [0,2]
    , testDejafu  C.stmRetry        "STM Retry"            alwaysSame
    , testDejafu  C.stmOrElse       "STM orElse"           alwaysSame
    , testDejafu  C.stmExc          "STM Exceptions"       alwaysSame
    , testDejafu  C.excNest         "Nested Exceptions"    alwaysSame
    ]

  , TestLabel "CRef Relaxed Memory" $ test
    [ testDejafu' SequentialConsistency 2 5 C.crefRelaxed "SQ" $ gives' [(True, True), (True, False), (False, True)]
    , testDejafu' TotalStoreOrder   2 5 C.crefRelaxed "TSO" $ gives' [(True, True), (True, False), (False, True), (False, False)]
    , testDejafu' PartialStoreOrder 2 5 C.crefRelaxed "PSO" $ gives' [(True, True), (True, False), (False, True), (False, False)]
    ]
  ]

main :: IO ()
main = void $ runTestTT tests

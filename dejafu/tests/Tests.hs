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
    , testDejafu  C.stmAtomic       "STM Atomicity"      $ gives [0,2]
    , testDejafu  C.stmRetry        "STM Retry"            alwaysSame
    , testDejafu  C.stmOrElse       "STM orElse"           alwaysSame
    , testDejafu  C.stmExc          "STM Exceptions"       alwaysSame
    , testDejafu  C.excNest         "Nested Exceptions"    alwaysSame
    ]

  , TestLabel "CRef Relaxed Memory" $ test
    [ testDejafu' SequentialConsistency 2 5 C.crefRelaxed "SQ" $ gives [(True, True), (True, False), (False, True)]
    , testDejafu' TotalStoreOrder   2 5 C.crefRelaxed "TSO" $ gives [(True, True), (True, False), (False, True), (False, False)]
    , testDejafu' PartialStoreOrder 2 5 C.crefRelaxed "PSO" $ gives [(True, True), (True, False), (False, True), (False, False)]
    ]
  ]

  where
    -- Predicate for when there is a known set of results where every
    -- result must be exhibited at least once.
    gives :: Eq a => [a] -> Predicate a
    gives expected results = go expected [] results Result { _pass = False, _casesChecked = 0, _failures = failures } where
      go waitingFor alreadySeen ((Right x, _):xs) res
        | x `elem` waitingFor  = go (filter (/=x) waitingFor) (x:alreadySeen) xs res { _casesChecked = _casesChecked res + 1 }
        | x `elem` alreadySeen = go waitingFor alreadySeen xs res { _casesChecked = _casesChecked res + 1 }
        | otherwise = res { _casesChecked = _casesChecked res + 1 }
      go waitingFor alreadySeen (_:xs) res = go waitingFor alreadySeen xs res
      go [] _ [] res = res { _pass = True }
      go _ _ _ res = res

      failures = filter (\(r, _) -> either (const True) (`notElem` expected) r) results

main :: IO ()
main = void $ runTestTT tests

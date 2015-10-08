module Main (main) where

import Data.Functor (void)
import Test.DejaFu hiding (MemType(..))
import Test.HUnit (Test(..), runTestTT, test)
import Test.HUnit.DejaFu

import qualified Tests.Cases  as C
import qualified Tests.Logger as L

tests :: Test
tests = TestList
  [ TestLabel "Simple" $ TestList
    [ TestLabel "Simple 2-Deadlock"    . test $ testDejafu  C.simple2Deadlock   deadlocksSometimes
    , TestLabel "2 Philosophers"       . test $ testDejafu (C.philosophers 2)   deadlocksSometimes
    , TestLabel "3 Philosophers"       . test $ testDejafu (C.philosophers 3)   deadlocksSometimes
    , TestLabel "4 Philosophers"       . test $ testDejafu (C.philosophers 4)   deadlocksSometimes
    , TestLabel "Threshold Value"      . test $ testDejafu  C.thresholdValue    notAlwaysSame
    , TestLabel "Forgotten Unlock"     . test $ testDejafu  C.forgottenUnlock   deadlocksAlways
    , TestLabel "Simple 2-Race"        . test $ testDejafu  C.simple2Race       notAlwaysSame
    , TestLabel "Racey Stack"          . test $ testDejafu  C.raceyStack        notAlwaysSame
    , TestLabel "Kill Thread"          . test $ testDejafu  C.threadKill        deadlocksSometimes
    , TestLabel "Kill Thread w/ Mask"  . test $ testDejafu  C.threadKillMask    deadlocksNever
    , TestLabel "Kill Thread w/ Umask" . test $ testDejafu  C.threadKillUmask   deadlocksSometimes
    , TestLabel "STM Atomicity"        . test $ testDejafu  C.stmAtomic       $ gives [0,2]
    , TestLabel "STM Retry"            . test $ testDejafu  C.stmRetry          alwaysSame
    , TestLabel "STM orElse"           . test $ testDejafu  C.stmOrElse         alwaysSame
    , TestLabel "STM Exceptions"       . test $ testDejafu  C.stmExc            alwaysSame
    , TestLabel "Nested Exceptions"    . test $ testDejafu  C.excNest           alwaysSame
    ]

  , TestLabel "CRef Relaxed Memory" $ TestList
    [ TestLabel "SQ"  . test $ testDejafus' SequentialConsistency 2 C.crefRelaxed [gives [(True, True), (True, False), (False, True)]]
    , TestLabel "TSO" . test $ testDejafus' TotalStoreOrder   2 C.crefRelaxed [gives [(True, True), (True, False), (False, True), (False, False)]]
    , TestLabel "PSO" . test $ testDejafus' PartialStoreOrder 2 C.crefRelaxed [gives [(True, True), (True, False), (False, True), (False, False)]]
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

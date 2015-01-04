{-# LANGUAGE RankNTypes #-}
module Tests.Utils where

import Control.Monad.Conc.Fixed (Conc)
import Control.Monad.Conc.SCT (sctPreBound)
import Data.List (group, sort)
import Data.Maybe (isJust, isNothing)
import System.Random (mkStdGen)

-- Couldn't get Cabal's detailed tests to work, hence this approach.
data Test   = Test { name :: String, result :: Result }
data Result = Pass | Fail String | Error String

-- | Test that a predicate holds over the results of a concurrent
-- computation.
testPred :: ([Maybe a] -> Result) -> Int -> (forall t. Conc t a) -> Result
testPred predicate num conc = predicate . map fst $ sctPreBound num conc

-- | Test that a concurrent computation is free of deadlocks.
testDeadlockFree :: Int -> (forall t. Conc t a) -> Result
testDeadlockFree = testPred predicate where
  predicate xs = case filter isNothing xs of
    [] -> Pass
    ds -> Fail $ "Found " ++ show (length ds) ++ "/" ++ show (length xs) ++ " deadlocking schedules."

-- | Test that a concurrent computation always deadlocks.
testDeadlocks :: Int -> (forall t. Conc t a) -> Result
testDeadlocks = testPred predicate where
  predicate xs = case filter isJust xs of
    [] -> Pass
    ds -> Fail $ "Found " ++ show (length ds) ++ "/" ++ show (length xs) ++ " productive schedules."

-- | Test that a concurrent computation always returns the same
-- result.
testAlwaysSame :: (Eq a, Ord a) => Int -> (forall t. Conc t a) -> Result
testAlwaysSame = testPred predicate where
  predicate xs = case group $ sort xs of
    []    -> Pass
    [[_]] -> Pass
    gs    -> Fail $ "Found " ++ show (length gs) ++ " distinct results."

-- | Invert the result of a test.
testNot :: String -> Result -> Result
testNot err  Pass    = Fail err
testNot _   (Fail _) = Pass
testNot _    err     = err

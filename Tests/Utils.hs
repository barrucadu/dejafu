{-# LANGUAGE RankNTypes #-}
module Tests.Utils where

import Control.Applicative ((<$>))
import Control.Monad.Conc.Fixed (Conc)
import Control.Monad.Conc.SCT (runSCT, sctRandom)
import Data.List (group, sort)
import Data.Maybe (isNothing)
import System.Random (mkStdGen)

-- Couldn't get Cabal's detailed tests to work, hence this approach.
data Test   = Test { name :: String, result :: IO Result }
data Result = Pass | Fail String | Error String

-- | Test that a predicate holds over the results of a concurrent
-- computation.
testPred :: ([Maybe a] -> Result) -> Int -> (forall t. Conc t a) -> IO Result
testPred predicate num conc = predicate . map fst <$> runSCT sctRandom (mkStdGen 0) num conc

-- | Test that a concurrent computation is free of deadlocks.
testDeadlockFree :: Int -> (forall t. Conc t a) -> IO Result
testDeadlockFree = testPred predicate where
  predicate xs = case filter isNothing xs of
    [] -> Pass
    ds -> Fail $ "Found " ++ show (length ds) ++ "/" ++ show (length xs) ++ " deadlocking schedules."

-- | Test that a concurrent computation always returns the same
-- result.
testAlwaysSame :: (Eq a, Ord a) => Int -> (forall t. Conc t a) -> IO Result
testAlwaysSame = testPred predicate where
  predicate xs = case group $ sort xs of
    []    -> Pass
    [[_]] -> Pass
    [gs]  -> Fail $ "Found " ++ show (length gs) ++ " distinct results."

-- | Invert the result of a test.
testNot :: String -> IO Result -> IO Result
testNot err old = do
  res <- old
  return $
    case res of
      Pass   -> Fail err
      Fail _ -> Pass
      e -> e

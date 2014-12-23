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

-- | Run a concurrent computation, aggregating the results.
doTest :: ([Maybe a] -> Result) -> Int -> (forall t. Conc t a) -> IO Result
doTest predicate num conc = predicate <$> map fst <$> runSCT sctRandom (mkStdGen 0) num conc

-- | Test that a concurrent computation is free of deadlocks.
testDeadlockFree :: Int -> (forall t. Conc t a) -> IO Result
testDeadlockFree = doTest predicate where
  predicate xs = case filter isNothing xs of
    [] -> Pass
    ds -> Fail $ "Found " ++ show (length ds) ++ "/" ++ show (length xs) ++ " deadlocking schedules."

-- | Test that a concurrent computation always returns the same
-- result.
testAlwaysSame :: (Eq a, Ord a) => Int -> (forall t. Conc t a) -> IO Result
testAlwaysSame = doTest predicate where
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

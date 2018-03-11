{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Test.DejaFu.SCT.Internal.Weighted
-- Copyright   : (c) 2015--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Internal types and functions for SCT via weighted random
-- scheduling.  This module is NOT considered to form part of the
-- public interface of this library.
module Test.DejaFu.SCT.Internal.Weighted where

import           Control.DeepSeq      (NFData)
import           Data.List.NonEmpty   (toList)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe)
import           GHC.Generics         (Generic)
import           System.Random        (RandomGen, randomR)

import           Test.DejaFu.Schedule (Scheduler(..))
import           Test.DejaFu.Types

-------------------------------------------------------------------------------
-- * Weighted random scheduler

-- | The scheduler state
data RandSchedState g = RandSchedState
  { schedWeights :: Map ThreadId Int
  -- ^ The thread weights: used in determining which to run.
  , schedGen     :: g
  -- ^ The random number generator.
  } deriving (Eq, Show, Generic, NFData)

-- | Initial weighted random scheduler state.
initialRandSchedState :: Maybe (Map ThreadId Int) -> g -> RandSchedState g
initialRandSchedState = RandSchedState . fromMaybe M.empty

-- | Weighted random scheduler: assigns to each new thread a weight,
-- and makes a weighted random choice out of the runnable threads at
-- every step.
randSched :: RandomGen g => (g -> (Int, g)) -> Scheduler (RandSchedState g)
randSched weightf = Scheduler $ \_ threads s ->
  let
    -- Select a thread
    pick idx ((x, f):xs)
      | idx < f = Just x
      | otherwise = pick (idx - f) xs
    pick _ [] = Nothing
    (choice, g'') = randomR (0, sum (map snd enabled) - 1) g'
    enabled = M.toList $ M.filterWithKey (\tid _ -> tid `elem` tids) weights'

    -- The weights, with any new threads added.
    (weights', g') = foldr assignWeight (M.empty, schedGen s) tids
    assignWeight tid ~(ws, g0) =
      let (w, g) = maybe (weightf g0) (\w0 -> (w0, g0)) (M.lookup tid (schedWeights s))
      in (M.insert tid w ws, g)

    -- The runnable threads.
    tids = map fst (toList threads)
  in (pick choice enabled, RandSchedState weights' g'')

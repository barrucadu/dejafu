-- | Deterministic scheduling for concurrent computations.
module Test.DejaFu.Deterministic.Schedule
  ( Scheduler
  , ThreadId
  , NonEmpty(..)
  -- * Pre-emptive
  , randomSched
  , roundRobinSched
  -- * Non pre-emptive
  , randomSchedNP
  , roundRobinSchedNP
  -- * Utilities
  , makeNP
  , toList
  ) where

import Data.List.Extra
import System.Random (RandomGen, randomR)
import Test.DejaFu.Deterministic.Internal

-- | A simple random scheduler which, at every step, picks a random
-- thread to run.
randomSched :: RandomGen g => Scheduler tid action lookahead g
randomSched _ _ threads g = (Just $ threads' !! choice, g') where
  (choice, g') = randomR (0, length threads' - 1) g
  threads' = map fst $ toList threads

-- | A random scheduler which doesn't pre-empt the running
-- thread. That is, if the last thread scheduled is still runnable,
-- run that, otherwise schedule randomly.
randomSchedNP :: (RandomGen g, Eq tid) => Scheduler tid action lookahead g
randomSchedNP = makeNP randomSched

-- | A round-robin scheduler which, at every step, schedules the
-- thread with the next 'ThreadId'.
roundRobinSched :: Scheduler ThreadId action lookahead ()
roundRobinSched _ Nothing _ _ = (Just initialThread, ())
roundRobinSched _ (Just (prior, _)) threads _
  | prior >= maximum threads' = (Just $ minimum threads', ())
  | otherwise = (Just . minimum $ filter (>prior) threads', ())

  where
    threads' = map fst $ toList threads

-- | A round-robin scheduler which doesn't pre-empt the running
-- thread.
roundRobinSchedNP :: Scheduler ThreadId action lookahead ()
roundRobinSchedNP = makeNP roundRobinSched

-- | Turn a potentially pre-emptive scheduler into a non-preemptive
-- one.
makeNP :: Eq tid
  => Scheduler tid action lookahead s
  -> Scheduler tid action lookahead s
makeNP sched = newsched where
  newsched trc p@(Just (prior, _)) threads s
    | prior `elem` map fst (toList threads) = (Just prior, s)
    | otherwise = sched trc p threads s
  newsched trc Nothing threads s = sched trc Nothing threads s

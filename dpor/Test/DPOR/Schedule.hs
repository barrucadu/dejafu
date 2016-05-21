-- |
-- Module      : Test.DPOR.Schedule
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Scheduling for concurrent computations.
module Test.DPOR.Schedule
  ( -- * Scheduling
    Scheduler

  , Decision(..)
  , tidOf
  , decisionOf

  , NonEmpty(..)

  -- ** Preemptive
  , randomSched
  , roundRobinSched

  -- ** Non-preemptive
  , randomSchedNP
  , roundRobinSchedNP

  -- * Utilities
  , makeNonPreemptive
  ) where

import Control.DeepSeq (NFData(..))
import Data.List.NonEmpty (NonEmpty(..), toList)
import System.Random (RandomGen, randomR)

-- | A @Scheduler@ drives the execution of a concurrent program. The
-- parameters it takes are:
--
-- 1. The trace so far.
--
-- 2. The last thread executed (if this is the first invocation, this
--    is @Nothing@).
--
-- 3. The runnable threads at this point.
--
-- 4. The state.
--
-- It returns a thread to execute, or @Nothing@ if execution should
-- abort here, and also a new state.
type Scheduler tid action lookahead s
  = [(Decision tid, action)]
  -> Maybe (tid, action)
  -> NonEmpty (tid, lookahead)
  -> s
  -> (Maybe tid, s)

-------------------------------------------------------------------------------
-- Scheduling decisions

-- | Scheduling decisions are based on the state of the running
-- program, and so we can capture some of that state in recording what
-- specific decision we made.
data Decision tid =
    Start tid
  -- ^ Start a new thread, because the last was blocked (or it's the
  -- start of computation).
  | Continue
  -- ^ Continue running the last thread for another step.
  | SwitchTo tid
  -- ^ Pre-empt the running thread, and switch to another.
  deriving (Eq, Show)

instance NFData tid => NFData (Decision tid) where
  rnf (Start    tid) = rnf tid
  rnf (SwitchTo tid) = rnf tid
  rnf d = d `seq` ()

-- | Get the resultant thread identifier of a 'Decision', with a default case
-- for 'Continue'.
tidOf :: tid -> Decision tid -> tid
tidOf _ (Start t)    = t
tidOf _ (SwitchTo t) = t
tidOf tid _          = tid

-- | Get the 'Decision' that would have resulted in this thread identifier,
-- given a prior thread (if any) and list of runnable threads.
decisionOf :: (Eq tid, Foldable f)
  => Maybe tid
  -- ^ The prior thread.
  -> f tid
  -- ^ The runnable threads.
  -> tid
  -- ^ The current thread.
  -> Decision tid
decisionOf Nothing _ chosen = Start chosen
decisionOf (Just prior) runnable chosen
  | prior == chosen = Continue
  | prior `elem` runnable = SwitchTo chosen
  | otherwise = Start chosen

-------------------------------------------------------------------------------
-- Preemptive

-- | A simple random scheduler which, at every step, picks a random
-- thread to run.
randomSched :: RandomGen g => Scheduler tid action lookahead g
randomSched _ _ threads g = (Just $ threads' !! choice, g') where
  (choice, g') = randomR (0, length threads' - 1) g
  threads' = map fst $ toList threads

-- | A round-robin scheduler which, at every step, schedules the
-- thread with the next 'ThreadId'.
roundRobinSched :: Ord tid => Scheduler tid action lookahead ()
roundRobinSched _ Nothing ((tid,_):|_) _ = (Just tid, ())
roundRobinSched _ (Just (prior, _)) threads _
  | prior >= maximum threads' = (Just $ minimum threads', ())
  | otherwise = (Just . minimum $ filter (>prior) threads', ())

  where
    threads' = map fst $ toList threads

-------------------------------------------------------------------------------
-- Non-preemptive

-- | A random scheduler which doesn't preempt the running
-- thread. That is, if the last thread scheduled is still runnable,
-- run that, otherwise schedule randomly.
randomSchedNP :: (RandomGen g, Eq tid) => Scheduler tid action lookahead g
randomSchedNP = makeNonPreemptive randomSched

-- | A round-robin scheduler which doesn't preempt the running
-- thread.
roundRobinSchedNP :: Ord tid => Scheduler tid action lookahead ()
roundRobinSchedNP = makeNonPreemptive roundRobinSched

-------------------------------------------------------------------------------
-- Utilities

-- | Turn a potentially preemptive scheduler into a non-preemptive
-- one.
makeNonPreemptive :: Eq tid
  => Scheduler tid action lookahead s
  -> Scheduler tid action lookahead s
makeNonPreemptive sched = newsched where
  newsched trc p@(Just (prior, _)) threads s
    | prior `elem` map fst (toList threads) = (Just prior, s)
    | otherwise = sched trc p threads s
  newsched trc Nothing threads s = sched trc Nothing threads s

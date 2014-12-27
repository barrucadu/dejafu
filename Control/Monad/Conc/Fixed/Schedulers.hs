-- | Schedulers for the fixed @Conc@ monads.
module Control.Monad.Conc.Fixed.Schedulers
  ( -- * Types
    Scheduler
  , ThreadId
  -- * Pre-emptive
  , randomSched
  , roundRobinSched
  -- * Non pre-emptive
  , randomSchedNP
  , roundRobinSchedNP
  -- * Utilities
  , makeNP
  ) where

import Control.Monad.Conc.Fixed.Internal
import System.Random (RandomGen, randomR)

-- | A simple random scheduler which, at every step, picks a random
-- thread to run.
randomSched :: RandomGen g => Scheduler g
randomSched g _ threads = (threads !! choice, g') where
  (choice, g') = randomR (0, length threads - 1) g

-- | A random scheduler which doesn't pre-empt the running
-- thread. That is, if the last thread scheduled is still runnable,
-- run that, otherwise schedule randomly.
randomSchedNP :: RandomGen g => Scheduler g
randomSchedNP = makeNP randomSched

-- | A round-robin scheduler which, at every step, schedules the
-- thread with the next 'ThreadId'.
roundRobinSched :: Scheduler ()
roundRobinSched _ prior threads
  | prior >= maximum threads = (minimum threads, ())
  | otherwise = (minimum $ filter (>prior) threads, ())

-- | A round-robin scheduler which doesn't pre-empt the running
-- thread.
roundRobinSchedNP :: Scheduler ()
roundRobinSchedNP = makeNP roundRobinSched

-- | Turn a potentially pre-emptive scheduler into a non-preemptive
-- one.
makeNP :: Scheduler s -> Scheduler s
makeNP sched = newsched where
  newsched s prior threads
    | prior `elem` threads = (prior, s)
    | otherwise = sched s prior threads

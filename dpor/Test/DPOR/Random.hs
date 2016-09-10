-- |
-- Module      : Test.DPOR.Random
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Random and incomplete techniques for when complete testing is
-- infeasible.
module Test.DPOR.Random
  ( -- * Randomness and partial-order reduction

  -- | These use the 'dpor' algorithm in "Test.DPOR", however without
  -- the promise to test every distinct schedule: instead, an optional
  -- execution limit is passed in, and a PRNG used to decide which
  -- actual schedules to test. Testing terminates when either the
  -- execution limit is reached, or when there are no distinct
  -- schedules remaining.
  --
  -- Despite being \"random\", these still uses the normal
  -- partial-order reduction and schedule bounding machinery, and so
  -- will prune the search space to \"interesting\" cases, and will
  -- never try the same schedule twice. Additionally, the thread
  -- partitioning function still applies when selecting schedules.
    randomDPOR

  -- * Non-POR techniques

  -- | These algorithms do not make use of partial-order reduction to
  -- systematically prune the search space and search for interesting
  -- interleavings. Instead, the exploration is driven entirely by
  -- random choice, with optional bounds. However, the same schedule
  -- will never be explored twice.
  , boundedRandom

  -- * Unsystematic techniques

  -- | These algorithms do not promise to visit every unique schedule,
  -- or to avoid trying the same schedule twice. This greatly reduces
  -- the memory requirements in testing complex systems. Randomness
  -- drives everything. No partial-order reduction is done, but
  -- schedule bounds are still available.

  , unsystematicRandom
  , unsystematicPCT
  ) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Random (RandomGen, randomR)

import Test.DPOR.Internal

-------------------------------------------------------------------------------
-- Randomness and partial-order reduction

-- | Random dynamic partial-order reduction.
--
-- This uses a systematic variant of PCT internally. See
-- 'unsystematicPCT' for an overview of the algorithm.
randomDPOR :: ( Ord       tid
              , NFData    tid
              , NFData    action
              , NFData    lookahead
              , NFData    s
              , Monad     m
              , RandomGen g
              )
  => Maybe Int
  -- ^ Optional execution limit, used to abort the execution whilst
  -- schedules still remain.
  -> g
  -- ^ Random number generator, used to determine which schedules to
  -- try.
  -> (action    -> Bool)
  -- ^ Determine if a thread yielded.
  -> (lookahead -> Bool)
  -- ^ Determine if a thread will yield.
  -> s
  -- ^ The initial state for backtracking.
  -> (s -> (tid, action) -> s)
  -- ^ The backtracking state step function.
  -> (s -> (tid, action) -> (tid, action)    -> Bool)
  -- ^ The dependency (1) function.
  -> (s -> (tid, action) -> (tid, lookahead) -> Bool)
  -- ^ The dependency (2) function.
  -> (s -> (tid, lookahead) -> NonEmpty tid -> Bool)
  -- ^ The daemon-termination predicate.
  -> tid
  -- ^ The initial thread.
  -> (tid -> Bool)
  -- ^ The thread partitioning function: when choosing what to
  -- execute, prefer threads which return true.
  -> BoundFunc tid action lookahead
  -- ^ The bounding function.
  -> BacktrackFunc tid action lookahead s
  -- ^ The backtracking function. Note that, for some bounding
  -- functions, this will need to add conservative backtracking
  -- points.
  -> (DPOR tid action -> DPOR tid action)
  -- ^ Some post-processing to do after adding the new to-do points.
  -> (DPORScheduler tid action lookahead s g
    -> SchedState tid action lookahead s g
    -> m (a, SchedState tid action lookahead s g, Trace tid action lookahead))
  -- ^ The runner: given the scheduler and state, execute the
  -- computation under that scheduler.
  -> m [(a, Trace tid action lookahead)]
randomDPOR lim0 g0 = runDPOR lim0 g0 genprior gennum genpch where
  -- Generate random priorities in the range [0, num threads)
  genprior _ _ ps = gennum (M.size ps + 1)
  gennum hi = randomR (0, hi - 1)
  -- Change the priorities with a 1/4 probability.
  genpch _ g = let (x, g') = gennum 4 g in (x == (0::Int), g')

-------------------------------------------------------------------------------
-- Non-POR techniques

-- | Pure random scheduling. Uses 'randomDPOR' internally, but all
-- actions are dependent and the bounds are optional.
boundedRandom :: ( Ord       tid
                 , NFData    tid
                 , NFData    action
                 , NFData    lookahead
                 , Monad     m
                 , RandomGen g
                 )
  => Maybe Int
  -- ^ Optional execution limit, used to abort the execution whilst
  -- schedules still remain.
  -> g
  -- ^ Random number generator, used to determine which schedules to
  -- try.
  -> (action    -> Bool)
  -- ^ Determine if a thread yielded.
  -> (lookahead -> Bool)
  -- ^ Determine if a thread will yield.
  -> tid
  -- ^ The initial thread.
  -> Maybe (BoundFunc tid action lookahead)
  -- ^ The bounding function. If no function is provided, 'trueBound'
  -- is used.
  -> (DPORScheduler tid action lookahead () g
    -> SchedState tid action lookahead () g
    -> m (a, SchedState tid action lookahead () g, Trace tid action lookahead))
  -- ^ The runner: given the scheduler and state, execute the
  -- computation under that scheduler.
  -> m [(a, Trace tid action lookahead)]
boundedRandom lim gen didYield willYield initialTid inBoundm
  = randomDPOR lim
               gen
               didYield
               willYield
               stinit
               ststep
               dependency1
               dependency2
               killsDaemons
               initialTid
               predicate
               inBound
               backtrack
               transform
  where
    stinit = ()
    ststep _ _ = ()
    dependency1 _ _ _ = True
    dependency2 _ _ _ = True
    killsDaemons _ _ _ = True
    predicate _ = True
    inBound = fromMaybe trueBound inBoundm
    backtrack = backtrackAt (const False) False
    transform = id

-------------------------------------------------------------------------------
-- Unsystematic techniques

-- | Random scheduling. Like 'randomDPOR' but all actions are
-- dependent and the bounds are optional.
unsystematicRandom :: ( Ord       tid
                      , NFData    tid
                      , NFData    action
                      , NFData    lookahead
                      , Monad     m
                      , RandomGen g
                      )
  => Int
  -- ^ Execution limit, used to abort the execution.
  -> g
  -- ^ Random number generator, used to determine which schedules to
  -- try.
  -> Maybe (BoundFunc tid action lookahead)
  -- ^ The bounding function. If no function is provided, 'trueBound'
  -- is used.
  -> (DPORScheduler tid action lookahead () g
    -> SchedState tid action lookahead () g
    -> m (a, SchedState tid action lookahead () g, Trace tid action lookahead))
  -- ^ The runner: given the scheduler and state, execute the
  -- computation under that scheduler.
  -> m [(a, Trace tid action lookahead)]
unsystematicRandom lim gen = runUnsystematic lim gen genprior gennum genpch  . fromMaybe trueBound where
  genprior _ ps = gennum (M.size ps + 1)
  gennum hi = randomR (0, hi - 1)
  genpch _ g = (False, g)

-- | Probabilistic concurrency testing (PCT). This is like random
-- scheduling, but schedules threads by priority, with random
-- priority-reassignment points during the execution. This is
-- typically more effective at finding bugs than just random
-- scheduling. This may be because, conceptually, PCT causes threads
-- to get very \"out of sync\" with each other, whereas random
-- scheduling will result in every thread progressing at roughly the
-- same rate.
--
-- See /A Randomized Scheduler with Probabilistic Guarantees of
-- Finding Bugs/, S. Burckhardt et al (2010).
unsystematicPCT :: ( Ord       tid
                   , NFData    tid
                   , NFData    action
                   , NFData    lookahead
                   , Monad     m
                   , RandomGen g
                   )
  => Int
  -- ^ Execution limit, used to abort the execution.
  -> g
  -- ^ Random number generator, used to determine which schedules to
  -- try.
  -> Maybe (BoundFunc tid action lookahead)
  -- ^ The bounding function. If no function is provided, 'trueBound'
  -- is used.
  -> (DPORScheduler tid action lookahead () g
    -> SchedState tid action lookahead () g
    -> m (a, SchedState tid action lookahead () g, Trace tid action lookahead))
  -- ^ The runner: given the scheduler and state, execute the
  -- computation under that scheduler.
  -> m [(a, Trace tid action lookahead)]
unsystematicPCT lim gen = runUnsystematic lim gen genprior gennum genpch . fromMaybe trueBound where
  genprior _ ps = gennum (M.size ps + 1)
  gennum hi = randomR (0, hi - 1)
  genpch _ g = let (x, g') = gennum 4 g in (x == (0::Int), g')

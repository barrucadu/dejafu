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
    randomDPOR

  -- * Non-POR techniques

  -- | These algorithms do not make use of partial-order reduction to
  -- systematically prune the search space and search for interesting
  -- interleavings. Instead, the exploration is driven entirely by
  -- random choice, with optional bounds. However, the same schedule
  -- will never be explored twice.
  , boundedRandom
  ) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import System.Random (RandomGen, randomR)

import Test.DPOR.Internal

-------------------------------------------------------------------------------
-- Randomness and partial-order reduction

-- | Random dynamic partial-order reduction.
--
-- This is the 'dpor' algorithm in "Test.DPOR", however it does not
-- promise to test every distinct schedule: instead, an execution
-- limit is passed in, and a PRNG used to decide which actual
-- schedules to test. Testing terminates when either the execution
-- limit is reached, or when there are no distinct schedules
-- remaining.
--
-- Despite being \"random\", this still uses the normal partial-order
-- reduction and schedule bounding machinery, and so will prune the
-- search space to \"interesting\" cases, and will never try the same
-- schedule twice. Additionally, the thread partitioning function
-- still applies when selecting schedules.
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
randomDPOR lim0 g0 = runDPOR lim0 g0 (\hi -> randomR (0, hi - 1))

-------------------------------------------------------------------------------
-- Unsystematic techniques

-- | Pure random scheduling. Like 'randomDPOR' but all actions are
-- dependent and the bounds are optional.
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

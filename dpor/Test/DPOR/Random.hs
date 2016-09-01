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
  => (action    -> Bool)
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
  -> g
  -- ^ Random number generator, used to determine which schedules to
  -- try.
  -> Int
  -- ^ Execution limit, used to abort the execution whilst schedules
  -- still remain.
  -> m [(a, Trace tid action lookahead)]
randomDPOR didYield
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
           run
  = go (initialState initialTid)

  where
    -- Repeatedly run the computation gathering all the results and
    -- traces into a list until there are no schedules remaining to
    -- try.
    go _ _ 0 = pure []
    go dp g elim = case nextPrefix g dp of
      Just (prefix, conservative, sleep, g') -> do
        (res, s, trace) <- run (scheduler gen)
                               (initialSchedState stinit sleep prefix g')

        let bpoints  = findBacktracks (schedBoundKill s) (schedBPoints s) trace
        let newDPOR  = addTrace conservative trace dp
        let newDPOR' = transform (addBacktracks bpoints newDPOR)

        let g'' = schedGenState s

        if schedIgnore s
        then go newDPOR g'' (elim-1)
        else ((res, trace):) <$> go newDPOR' g'' (elim-1)

      Nothing -> pure []

    -- Generate a random value from a range
    gen hi = randomR (0, hi - 1)

    -- Find the next schedule prefix.
    nextPrefix = findSchedulePrefix predicate . flip gen

    -- The DPOR scheduler.
    scheduler = dporSched didYield willYield dependency1 killsDaemons ststep inBound

    -- Find the new backtracking steps.
    findBacktracks = findBacktrackSteps stinit ststep dependency2 backtrack

    -- Incorporate a trace into the DPOR tree.
    addTrace = incorporateTrace stinit ststep dependency1

    -- Incorporate the new backtracking steps into the DPOR tree.
    addBacktracks = incorporateBacktrackSteps inBound

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
  => (action    -> Bool)
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
  -> g
  -- ^ Random number generator, used to determine which schedules to
  -- try.
  -> Int
  -- ^ Execution limit, used to abort the execution whilst schedules
  -- still remain.
  -> m [(a, Trace tid action lookahead)]
boundedRandom didYield willYield initialTid inBoundm
  = randomDPOR didYield
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

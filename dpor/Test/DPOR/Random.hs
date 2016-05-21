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

  -- * Unsystematic techniques

  -- | These algorithms do not make use of partial-order reduction or
  -- schedule bounding to systematically prune the search space and
  -- search for interesting interleavings. Instead, the exploration is
  -- driven entirely by random choice. However, the same schedule will
  -- never be explored twice.
  , pureRandom
  ) where

import Control.DeepSeq (NFData)
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
  -> (s -> action -> s)
  -- ^ The backtracking state step function.
  -> (s -> (tid, action) -> (tid, action)    -> Bool)
  -- ^ The dependency (1) function.
  -> (s -> (tid, action) -> (tid, lookahead) -> Bool)
  -- ^ The dependency (2) function.
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
  -> (DPORScheduler tid action lookahead s
    -> SchedState tid action lookahead s
    -> m (a, SchedState tid action lookahead s, Trace tid action lookahead))
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
        (res, s, trace) <- run scheduler
                              (initialSchedState stinit sleep prefix)

        let bpoints  = findBacktracks s trace
        let newDPOR  = addTrace conservative trace dp
        let newDPOR' = transform (addBacktracks bpoints newDPOR)

        if schedIgnore s
        then go newDPOR g' (elim-1)
        else ((res, trace):) <$> go newDPOR' g' (elim-1)

      Nothing -> pure []

    -- Generate a random value from a range
    gen g hi = randomR (0, hi - 1) g

    -- Find the next schedule prefix.
    nextPrefix = findSchedulePrefix predicate . gen

    -- The DPOR scheduler.
    scheduler = dporSched didYield willYield dependency1 ststep inBound

    -- Find the new backtracking steps.
    findBacktracks = findBacktrackSteps stinit ststep dependency2 backtrack .
                     schedBPoints

    -- Incorporate a trace into the DPOR tree.
    addTrace = incorporateTrace stinit ststep dependency1

    -- Incorporate the new backtracking steps into the DPOR tree.
    addBacktracks = incorporateBacktrackSteps inBound

-------------------------------------------------------------------------------
-- Unsystematic techniques

-- | Pure random scheduling. Like 'randomDPOR' but all actions are
-- dependent and there are no bounds.
pureRandom :: ( Ord       tid
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
  -> (DPORScheduler tid action lookahead ()
    -> SchedState tid action lookahead ()
    -> m (a, SchedState tid action lookahead (), Trace tid action lookahead))
  -- ^ The runner: given the scheduler and state, execute the
  -- computation under that scheduler.
  -> g
  -- ^ Random number generator, used to determine which schedules to
  -- try.
  -> Int
  -- ^ Execution limit, used to abort the execution whilst schedules
  -- still remain.
  -> m [(a, Trace tid action lookahead)]
pureRandom didYield willYield initialTid
  = randomDPOR didYield
               willYield
               stinit
               ststep
               dependency1
               dependency2
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
    predicate _ = True
    inBound = trueBound
    backtrack = backtrackAt (const False) False
    transform = id

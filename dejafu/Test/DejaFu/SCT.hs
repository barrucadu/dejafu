{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Test.DejaFu.SCT
-- Copyright   : (c) 2015--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : BangPatterns, LambdaCase
--
-- Systematic testing for concurrent computations.
module Test.DejaFu.SCT
  ( -- * Running Concurrent Programs
    runSCT
  , runSCTWithSettings
  , resultsSet
  , resultsSetWithSettings

  -- ** Strict variants
  , runSCT'
  , resultsSet'
  , runSCTWithSettings'
  , resultsSetWithSettings'

  -- ** Configuration
  , module Test.DejaFu.Settings

  -- * Deprecated
  , runSCTDiscard
  , resultsSetDiscard
  , runSCTDiscard'
  , resultsSetDiscard'
  , sctBound
  , sctBoundDiscard
  , sctUniformRandom
  , sctUniformRandomDiscard
  , sctWeightedRandom
  , sctWeightedRandomDiscard
  ) where

import           Control.Applicative               ((<|>))
import           Control.DeepSeq                   (NFData(..), force)
import           Control.Monad.Conc.Class          (MonadConc)
import           Control.Monad.Ref                 (MonadRef)
import           Data.List                         (foldl')
import qualified Data.Map.Strict                   as M
import           Data.Maybe                        (fromMaybe)
import           Data.Set                          (Set)
import qualified Data.Set                          as S
import           System.Random                     (RandomGen, randomR)

import           Test.DejaFu.Conc
import           Test.DejaFu.Internal
import           Test.DejaFu.SCT.Internal.DPOR
import           Test.DejaFu.SCT.Internal.Weighted
import           Test.DejaFu.Settings
import           Test.DejaFu.Types
import           Test.DejaFu.Utils

-------------------------------------------------------------------------------
-- Running Concurrent Programs

-- | Explore possible executions of a concurrent program according to
-- the given 'Way'.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.
--
-- @since 1.0.0.0
runSCT :: (MonadConc n, MonadRef r n)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> ConcT r n a
  -- ^ The computation to run many times.
  -> n [(Either Failure a, Trace)]
runSCT way = runSCTWithSettings . fromWayAndMemType way

-- | Return the set of results of a concurrent program.
--
-- @since 1.0.0.0
resultsSet :: (MonadConc n, MonadRef r n, Ord a)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> ConcT r n a
  -- ^ The computation to run many times.
  -> n (Set (Either Failure a))
resultsSet way = resultsSetWithSettings . fromWayAndMemType way

-- | A variant of 'runSCT' which can selectively discard results.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.
--
-- @since 1.0.0.0
runSCTDiscard :: (MonadConc n, MonadRef r n)
  => (Either Failure a -> Maybe Discard)
  -- ^ Selectively discard results.
  -> Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> ConcT r n a
  -- ^ The computation to run many times.
  -> n [(Either Failure a, Trace)]
runSCTDiscard discard way = runSCTWithSettings . set ldiscard (Just discard) . fromWayAndMemType way
{-# DEPRECATED runSCTDiscard "Use runSCTWithSettings instead" #-}

-- | A variant of 'resultsSet' which can selectively discard results.
--
-- @since 1.0.0.0
resultsSetDiscard :: (MonadConc n, MonadRef r n, Ord a)
  => (Either Failure a -> Maybe Discard)
  -- ^ Selectively discard results.  Traces are always discarded.
  -> Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> ConcT r n a
  -- ^ The computation to run many times.
  -> n (Set (Either Failure a))
resultsSetDiscard discard way memtype conc =
  let discard' efa = discard efa <|> Just DiscardTrace
  in S.fromList . map fst <$> runSCTDiscard discard' way memtype conc
{-# DEPRECATED resultsSetDiscard "Use resultsSetWithSettings instead" #-}

-- | A strict variant of 'runSCT'.
--
-- Demanding the result of this will force it to normal form, which
-- may be more efficient in some situations.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.
--
-- @since 1.0.0.0
runSCT' :: (MonadConc n, MonadRef r n, NFData a)
  => Way -> MemType -> ConcT r n a -> n [(Either Failure a, Trace)]
runSCT' way = runSCTWithSettings' . fromWayAndMemType way

-- | A strict variant of 'resultsSet'.
--
-- Demanding the result of this will force it to normal form, which
-- may be more efficient in some situations.
--
-- @since 1.0.0.0
resultsSet' :: (MonadConc n, MonadRef r n, Ord a, NFData a)
  => Way -> MemType -> ConcT r n a -> n (Set (Either Failure a))
resultsSet' way = resultsSetWithSettings' . fromWayAndMemType way

-- | A strict variant of 'runSCTDiscard'.
--
-- Demanding the result of this will force it to normal form, which
-- may be more efficient in some situations.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.
--
-- @since 1.0.0.0
runSCTDiscard' :: (MonadConc n, MonadRef r n, NFData a)
  => (Either Failure a -> Maybe Discard) -> Way -> MemType -> ConcT r n a -> n [(Either Failure a, Trace)]
runSCTDiscard' discard way memtype conc = do
  res <- runSCTDiscard discard way memtype conc
  rnf res `seq` pure res
{-# DEPRECATED runSCTDiscard' "Use runSCTWithSettings' instead" #-}

-- | A strict variant of 'resultsSetDiscard'.
--
-- Demanding the result of this will force it to normal form, which
-- may be more efficient in some situations.
--
-- @since 1.0.0.0
resultsSetDiscard' :: (MonadConc n, MonadRef r n, Ord a, NFData a)
  => (Either Failure a -> Maybe Discard) -> Way -> MemType -> ConcT r n a -> n (Set (Either Failure a))
resultsSetDiscard' discard way memtype conc = do
  res <- resultsSetDiscard discard way memtype conc
  rnf res `seq` pure res
{-# DEPRECATED resultsSetDiscard' "Use resultsSetWithSettings' instead" #-}

-------------------------------------------------------------------------------
-- Configuration

-- | A variant of 'runSCT' which takes a 'Settings' record.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.
--
-- @since unreleased
runSCTWithSettings :: (MonadConc n, MonadRef r n)
  => Settings n a
  -- ^ The SCT settings.
  -> ConcT r n a
  -- ^ The computation to run many times.
  -> n [(Either Failure a, Trace)]
runSCTWithSettings settings conc = case _way settings of
  Systematic cb0 ->
    let initial = initialState

        check = findSchedulePrefix

        step dp (prefix, conservative, sleep) run = do
          (res, s, trace) <- run
            (dporSched (cBound cb0))
            (initialDPORSchedState sleep prefix)

          let bpoints = findBacktrackSteps (cBacktrack cb0) (schedBoundKill s) (schedBPoints s) trace
          let newDPOR = incorporateTrace conservative trace dp

          pure $ if schedIgnore s
                 then (force newDPOR, Nothing)
                 else (force (incorporateBacktrackSteps bpoints newDPOR), Just (res, trace))
    in sct settings initial check step conc

  Uniform g0 lim0 ->
    let initial _ = (g0, max 0 lim0)

        check (_, 0) = Nothing
        check s = Just s

        step _ (g, n) run = do
          (res, s, trace) <- run
            (randSched $ \g' -> (1, g'))
            (initialRandSchedState Nothing g)
          pure ((schedGen s, n-1), Just (res, trace))
    in sct settings initial check step conc

  Weighted g0 lim0 use0 ->
    let initial _ = (g0, max 0 lim0, max 1 use0, M.empty)

        check (_, 0, _, _) = Nothing
        check s = Just s

        step s (g, n, 0, _) run = step s (g, n, max 1 use0, M.empty) run
        step _ (g, n, use, ws) run = do
          (res, s, trace) <- run
            (randSched $ randomR (1, 50))
            (initialRandSchedState (Just ws) g)
          pure ((schedGen s, n-1, use-1, schedWeights s), Just (res, trace))
    in sct settings initial check step conc

-- | A variant of 'resultsSet' which takes a 'Settings' record.
--
-- @since unreleased
resultsSetWithSettings :: (MonadConc n, MonadRef r n, Ord a)
  => Settings n a
  -- ^ The SCT settings.
  -> ConcT r n a
  -- ^ The computation to run many times.
  -> n (Set (Either Failure a))
resultsSetWithSettings settings conc =
  let settings' = settings { _discard = Just $ \efa -> fromMaybe (const Nothing) (_discard settings) efa <|> Just DiscardTrace }
  in S.fromList . map fst <$> runSCTWithSettings settings' conc

-- | A strict variant of 'runSCTWithSettings'.
--
-- Demanding the result of this will force it to normal form, which
-- may be more efficient in some situations.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.
--
-- @since unreleased
runSCTWithSettings' :: (MonadConc n, MonadRef r n, NFData a)
  => Settings n a
  -> ConcT r n a
  -> n [(Either Failure a, Trace)]
runSCTWithSettings' settings conc = do
  res <- runSCTWithSettings settings conc
  rnf res `seq` pure res

-- | A strict variant of 'resultsSetWithSettings'.
--
-- Demanding the result of this will force it to normal form, which
-- may be more efficient in some situations.
--
-- @since unreleased
resultsSetWithSettings' :: (MonadConc n, MonadRef r n, Ord a, NFData a)
  => Settings n a
  -> ConcT r n a
  -> n (Set (Either Failure a))
resultsSetWithSettings' settings conc = do
  res <- resultsSetWithSettings settings conc
  rnf res `seq` pure res

-------------------------------------------------------------------------------
-- Combined Bounds

-- | Combination bound function
cBound :: Bounds -> IncrementalBoundFunc ((Int, Maybe ThreadId), M.Map ThreadId Int, Int)
cBound (Bounds pb fb lb) (Just (k1, k2, k3)) prior lh =
  let k1' = maybe (\k _ _ -> k) pBound pb (Just k1) prior lh
      k2' = maybe (\k _ _ -> k) fBound fb (Just k2) prior lh
      k3' = maybe (\k _ _ -> k) lBound lb (Just k3) prior lh
  in (,,) <$> k1' <*> k2' <*> k3'
cBound _ Nothing _ _ = Just ((0, Nothing), M.empty, 1)

-- | Combination backtracking function. Add all backtracking points
-- corresponding to enabled bound functions.
--
-- If no bounds are enabled, just backtrack to the given point.
cBacktrack :: Bounds -> BacktrackFunc
cBacktrack (Bounds (Just _) _ _) = pBacktrack
cBacktrack (Bounds _ (Just _) _) = fBacktrack
cBacktrack (Bounds _ _ (Just _)) = lBacktrack
cBacktrack _ = backtrackAt (\_ _ -> False)

-------------------------------------------------------------------------------
-- Pre-emption bounding

-- | Pre-emption bound function. This does not count pre-emptive
-- context switches to a commit thread.
pBound :: PreemptionBound -> IncrementalBoundFunc (Int, Maybe ThreadId)
pBound (PreemptionBound pb) k prior lhead =
  let k'@(pcount, _) = preEmpCountInc (fromMaybe (0, Nothing) k) prior lhead
  in if pcount <= pb then Just k' else Nothing

-- | Add a backtrack point, and also conservatively add one prior to
-- the most recent transition before that point. This may result in
-- the same state being reached multiple times, but is needed because
-- of the artificial dependency imposed by the bound.
pBacktrack :: BacktrackFunc
pBacktrack bs = backtrackAt (\_ _ -> False) bs . concatMap addConservative where
  addConservative o@(i, _, tid) = o : case conservative i of
    Just j  -> [(j, True, tid)]
    Nothing -> []

  -- index of conservative point
  conservative i = go (reverse (take (i-1) bs)) (i-1) where
    go _ (-1) = Nothing
    go (b1:rest@(b2:_)) j
      | bcktThreadid b1 /= bcktThreadid b2
        && not (isCommitRef $ bcktAction b1)
        && not (isCommitRef $ bcktAction b2) = Just j
      | otherwise = go rest (j-1)
    go _ _ = Nothing

-------------------------------------------------------------------------------
-- Fair bounding

-- | Fair bound function
fBound :: FairBound -> IncrementalBoundFunc (M.Map ThreadId Int)
fBound (FairBound fb) k prior lhead =
  let k' = yieldCountInc (fromMaybe M.empty k) prior lhead
  in if not (willYield (snd lhead)) || maxDiff (M.elems k') <= fb
     then Just k'
     else Nothing

-- | Add a backtrack point. If the thread doesn't exist or is blocked,
-- or performs a release operation, add all unblocked threads.
fBacktrack :: BacktrackFunc
fBacktrack = backtrackAt check where
  -- True if a release operation is performed.
  check t b = Just True == (willRelease <$> M.lookup t (bcktRunnable b))

-------------------------------------------------------------------------------
-- Length bounding

-- | Length bound function
lBound :: LengthBound -> IncrementalBoundFunc Int
lBound (LengthBound lb) len _ _ =
  let len' = maybe 1 (+1) len
  in if len' < lb then Just len' else Nothing

-- | Add a backtrack point. If the thread doesn't exist or is blocked,
-- add all unblocked threads.
lBacktrack :: BacktrackFunc
lBacktrack = backtrackAt (\_ _ -> False)

-------------------------------------------------------------------------------
-- Systematic concurrency testing

-- | SCT via BPOR.
--
-- Schedules are generated by running the computation with a
-- deterministic scheduler with some initial list of decisions. At
-- each step of execution, possible-conflicting actions are looked
-- for, if any are found, \"backtracking points\" are added, to cause
-- the events to happen in a different order in a future execution.
--
-- Note that unlike with non-bounded partial-order reduction, this may
-- do some redundant work as the introduction of a bound can make
-- previously non-interfering events interfere with each other.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.
--
-- @since 1.0.0.0
sctBound :: (MonadConc n, MonadRef r n)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The combined bounds.
  -> ConcT r n a
  -- ^ The computation to run many times
  -> n [(Either Failure a, Trace)]
sctBound = sctBoundDiscard (const Nothing)
{-# DEPRECATED sctBound "Use runSCT instead" #-}

-- | A variant of 'sctBound' which can selectively discard results.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.
--
-- @since 1.0.0.0
sctBoundDiscard :: (MonadConc n, MonadRef r n)
  => (Either Failure a -> Maybe Discard)
  -- ^ Selectively discard results.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The combined bounds.
  -> ConcT r n a
  -- ^ The computation to run many times
  -> n [(Either Failure a, Trace)]
sctBoundDiscard discard memtype cb = runSCTWithSettings $
  set ldiscard (Just discard) (fromWayAndMemType (systematically cb) memtype)
{-# DEPRECATED sctBoundDiscard "Use runSCTWithSettings instead" #-}

-- | SCT via uniform random scheduling.
--
-- Schedules are generated by assigning to each new thread a random
-- weight. Threads are then scheduled by a weighted random selection.
--
-- This is not guaranteed to find all distinct results.
--
-- @since 1.0.0.0
sctUniformRandom :: (MonadConc n, MonadRef r n, RandomGen g)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> g
  -- ^ The random number generator.
  -> Int
  -- ^ The number of executions to perform.
  -> ConcT r n a
  -- ^ The computation to run many times.
  -> n [(Either Failure a, Trace)]
sctUniformRandom = sctUniformRandomDiscard (const Nothing)
{-# DEPRECATED sctUniformRandom "Use runSCT instead" #-}

-- | A variant of 'sctUniformRandom' which can selectively discard
-- results.
--
-- This is not guaranteed to find all distinct results.
--
-- @since 1.0.0.0
sctUniformRandomDiscard :: (MonadConc n, MonadRef r n, RandomGen g)
  => (Either Failure a -> Maybe Discard)
  -- ^ Selectively discard results.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> g
  -- ^ The random number generator.
  -> Int
  -- ^ The number of executions to perform.
  -> ConcT r n a
  -- ^ The computation to run many times.
  -> n [(Either Failure a, Trace)]
sctUniformRandomDiscard discard memtype g lim = runSCTWithSettings $
  set ldiscard (Just discard) (fromWayAndMemType (uniformly g lim) memtype)
{-# DEPRECATED sctUniformRandomDiscard "Use runSCTWithSettings instead" #-}

-- | SCT via weighted random scheduling.
--
-- Schedules are generated by assigning to each new thread a random
-- weight. Threads are then scheduled by a weighted random selection.
--
-- This is not guaranteed to find all distinct results.
--
-- @since 1.0.0.0
sctWeightedRandom :: (MonadConc n, MonadRef r n, RandomGen g)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> g
  -- ^ The random number generator.
  -> Int
  -- ^ The number of executions to perform.
  -> Int
  -- ^ The number of executions to use the same set of weights for.
  -> ConcT r n a
  -- ^ The computation to run many times.
  -> n [(Either Failure a, Trace)]
sctWeightedRandom = sctWeightedRandomDiscard (const Nothing)
{-# DEPRECATED sctWeightedRandom "Use runSCT instead" #-}

-- | A variant of 'sctWeightedRandom' which can selectively discard
-- results.
--
-- This is not guaranteed to find all distinct results.
--
-- @since 1.0.0.0
sctWeightedRandomDiscard :: (MonadConc n, MonadRef r n, RandomGen g)
  => (Either Failure a -> Maybe Discard)
  -- ^ Selectively discard results.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> g
  -- ^ The random number generator.
  -> Int
  -- ^ The number of executions to perform.
  -> Int
  -- ^ The number of executions to use the same set of weights for.
  -> ConcT r n a
  -- ^ The computation to run many times.
  -> n [(Either Failure a, Trace)]
sctWeightedRandomDiscard discard memtype g lim use = runSCTWithSettings $
  set ldiscard (Just discard) (fromWayAndMemType (swarmy g lim use) memtype)
{-# DEPRECATED sctWeightedRandomDiscard "Use runSCTWithSettings instead" #-}

-- | General-purpose SCT function.
sct :: (MonadConc n, MonadRef r n)
  => Settings n a
  -- ^ The SCT settings ('Way' is ignored)
  -> ([ThreadId] -> s)
  -- ^ Initial state
  -> (s -> Maybe t)
  -- ^ State predicate
  -> (s -> t -> (Scheduler g -> g -> n (Either Failure a, g, Trace)) -> n (s, Maybe (Either Failure a, Trace)))
  -- ^ Run the computation and update the state
  -> ConcT r n a
  -> n [(Either Failure a, Trace)]
sct settings s0 sfun srun conc
    | canDCSnapshot conc = runForDCSnapshot conc >>= \case
        Just (Right snap, _) -> go (runSnap snap) (fst (threadsFromDCSnapshot snap))
        Just (Left f, trace) -> pure [(Left f, trace)]
        _ -> do
          debugPrint "Failed to construct snapshot, continuing without."
          go runFull [initialThread]
    | otherwise = go runFull [initialThread]
  where
    go run = go' Nothing . s0 where
      go' (Just res) _ | earlyExit res = pure []
      go' _ !s = case sfun s of
        Just t -> srun s t run >>= \case
          (s', Just (res, trace)) -> case discard res of
            Just DiscardResultAndTrace -> go' (Just res) s'
            Just DiscardTrace -> ((res, []):) <$> go' (Just res) s'
            Nothing -> ((res, trace):) <$> go' (Just res) s'
          (s', Nothing) -> go' Nothing s'
        Nothing -> pure []

    runFull sched s = runConcurrent sched (_memtype settings) s conc
    runSnap snap sched s = runWithDCSnapshot sched (_memtype settings) s snap

    debugPrint = fromMaybe (const (pure ())) (_debugPrint settings)
    earlyExit = fromMaybe (const False) (_earlyExit settings)
    discard = fromMaybe (const Nothing) (_discard settings)

-------------------------------------------------------------------------------
-- Utilities

-- | An incremental version of 'preEmpCount', going one step at a time.
preEmpCountInc
  :: (Int, Maybe ThreadId)
  -- ^ The number of preemptions so far and, if currently executing a
  -- commit thread, what the prior thread was.
  -> Maybe (ThreadId, ThreadAction)
  -- ^ What just happened.
  -> (Decision, a)
  -- ^ What's coming up.
  -> (Int, Maybe ThreadId)
preEmpCountInc (sofar, lastnoncommit) prior (d, _) = case (prior, d) of
    (Just (tid, _),   Start    tnext) -> cswitch tid tnext False
    (Just (tid, act), SwitchTo tnext) -> cswitch tid tnext (not (didYield act))
    (_, Continue) -> (sofar, lastnoncommit)
    (Nothing, _)  -> (sofar, lastnoncommit)
  where
    cswitch tid tnext isPreemptive
      | isCommitThread tnext = (sofar, if isCommitThread tid then lastnoncommit else Just tid)
      | isCommitThread tid   = (if lastnoncommit == Just tnext then sofar else sofar + 1, Nothing)
      | otherwise = (if isPreemptive then sofar + 1 else sofar, Nothing)

    isCommitThread = (< initialThread)

-- | An incremental count of yields, going one step at a time.
yieldCountInc
  :: M.Map ThreadId Int
  -- ^ The number of yields of each thread so far
  -> Maybe (ThreadId, a)
  -- ^ What just happened.
  -> (Decision, Lookahead)
  -- ^ What's coming up.
  -> M.Map ThreadId Int
yieldCountInc sofar prior (d, lnext) = case prior of
    Just (tid, _) -> ycount (tidOf tid d)
    Nothing       -> ycount initialThread
  where
    ycount tnext
      | willYield lnext = M.alter (Just . maybe 1 (+1)) tnext sofar
      | otherwise       = M.alter (Just . fromMaybe 0) tnext sofar

-- | Determine if an action is a commit or not.
isCommitRef :: ThreadAction -> Bool
isCommitRef (CommitCRef _ _) = True
isCommitRef _ = False

-- | Get the maximum difference between two ints in a list.
maxDiff :: [Int] -> Int
maxDiff = go 0 where
  go m (x:xs) =
    let m' = m `max` foldl' (go' x) 0 xs
    in go m' xs
  go m [] = m
  go' x0 m x = m `max` abs (x0 - x)

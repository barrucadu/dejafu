{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Test.DejaFu.SCT
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : GeneralizedNewtypeDeriving
--
-- Systematic testing for concurrent computations.
module Test.DejaFu.SCT
  ( -- * Bounded Partial-order Reduction

  -- | We can characterise the state of a concurrent computation by
  -- considering the ordering of dependent events. This is a partial
  -- order: independent events can be performed in any order without
  -- affecting the result, and so are /not/ ordered.
  --
  -- Partial-order reduction is a technique for computing these
  -- partial orders, and only testing one total order for each partial
  -- order. This cuts down the amount of work to be done
  -- significantly. /Bounded/ partial-order reduction is a further
  -- optimisation, which only considers schedules within some bound.
  --
  -- This module provides a combination pre-emption, fair, and length
  -- bounding runner:
  --
  -- * Pre-emption + fair bounding is useful for programs which use
  --   loop/yield control flows but are otherwise terminating.
  --
  -- * Pre-emption, fair + length bounding is useful for
  --   non-terminating programs, and used by the testing functionality
  --   in @Test.DejaFu@.
  --
  -- See /Bounded partial-order reduction/, K. Coons, M. Musuvathi,
  -- K. McKinley for more details.

    Bounds(..)
  , defaultBounds
  , noBounds

  , sctBound

  -- * Individual Bounds

  -- ** Pre-emption Bounding

  -- | BPOR using pre-emption bounding. This adds conservative
  -- backtracking points at the prior context switch whenever a
  -- non-conervative backtracking point is added, as alternative
  -- decisions can influence the reachability of different states.
  --
  -- See the BPOR paper for more details.

  , PreemptionBound(..)
  , defaultPreemptionBound
  , sctPreBound

  -- ** Fair Bounding

  -- | BPOR using fair bounding. This bounds the maximum difference
  -- between the number of yield operations different threads have
  -- performed.
  --
  -- See the BPOR paper for more details.

  , FairBound(..)
  , defaultFairBound
  , sctFairBound

  -- ** Length Bounding

  -- | BPOR using length bounding. This bounds the maximum length (in
  -- terms of primitive actions) of an execution.

  , LengthBound(..)
  , defaultLengthBound
  , sctLengthBound
  ) where

import Control.Monad.Ref (MonadRef)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as S

import Test.DejaFu.Common
import Test.DejaFu.Conc
import Test.DejaFu.SCT.Internal

-------------------------------------------------------------------------------
-- Combined Bounds

data Bounds = Bounds
  { boundPreemp :: Maybe PreemptionBound
  , boundFair   :: Maybe FairBound
  , boundLength :: Maybe LengthBound
  }

-- | All bounds enabled, using their default values.
defaultBounds :: Bounds
defaultBounds = Bounds
  { boundPreemp = Just defaultPreemptionBound
  , boundFair   = Just defaultFairBound
  , boundLength = Just defaultLengthBound
  }

-- | No bounds enabled. This forces the scheduler to just use
-- partial-order reduction and sleep sets to prune the search
-- space. This will /ONLY/ work if your computation always terminated!
noBounds :: Bounds
noBounds = Bounds
  { boundPreemp = Nothing
  , boundFair   = Nothing
  , boundLength = Nothing
  }

-- | Combination bound function
cBound :: Bounds -> BoundFunc
cBound (Bounds pb fb lb) =
  maybe trueBound pBound pb &+&
  maybe trueBound fBound fb &+&
  maybe trueBound lBound lb

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

newtype PreemptionBound = PreemptionBound Int
  deriving (Enum, Eq, Ord, Num, Real, Integral, Read, Show)

-- | A sensible default preemption bound: 2.
--
-- See /Concurrency Testing Using Schedule Bounding: an Empirical Study/,
-- P. Thomson, A. F. Donaldson, A. Betts for justification.
defaultPreemptionBound :: PreemptionBound
defaultPreemptionBound = 2

-- | An SCT runner using a pre-emption bounding scheduler.
sctPreBound :: MonadRef r n
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> PreemptionBound
  -- ^ The maximum number of pre-emptions to allow in a single
  -- execution
  -> Conc n r a
  -- ^ The computation to run many times
  -> n [(Either Failure a, Trace)]
sctPreBound memtype pb = sctBound memtype $ Bounds (Just pb) Nothing Nothing

-- | Pre-emption bound function. This does not count pre-emptive
-- context switches to a commit thread.
pBound :: PreemptionBound -> BoundFunc
pBound (PreemptionBound pb) ts dl = preEmpCount ts dl <= pb

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

newtype FairBound = FairBound Int
  deriving (Enum, Eq, Ord, Num, Real, Integral, Read, Show)

-- | A sensible default fair bound: 5.
--
-- This comes from playing around myself, but there is probably a
-- better default.
defaultFairBound :: FairBound
defaultFairBound = 5

-- | An SCT runner using a fair bounding scheduler.
sctFairBound :: MonadRef r n
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> FairBound
  -- ^ The maximum difference between the number of yield operations
  -- performed by different threads.
  -> Conc n r a
  -- ^ The computation to run many times
  -> n [(Either Failure a, Trace)]
sctFairBound memtype fb = sctBound memtype $ Bounds Nothing (Just fb) Nothing

-- | Fair bound function
fBound :: FairBound -> BoundFunc
fBound (FairBound fb) ts (_, l) = maxYieldCountDiff ts l <= fb

-- | Add a backtrack point. If the thread isn't runnable, or performs
-- a release operation, add all runnable threads.
fBacktrack :: BacktrackFunc
fBacktrack = backtrackAt check where
  -- True if a release operation is performed.
  check t b = Just True == (willRelease <$> M.lookup t (bcktRunnable b))

-------------------------------------------------------------------------------
-- Length bounding

newtype LengthBound = LengthBound Int
  deriving (Enum, Eq, Ord, Num, Real, Integral, Read, Show)

-- | A sensible default length bound: 250.
--
-- Based on the assumption that anything which executes for much
-- longer (or even this long) will take ages to test.
defaultLengthBound :: LengthBound
defaultLengthBound = 250

-- | An SCT runner using a length bounding scheduler.
sctLengthBound :: MonadRef r n
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> LengthBound
  -- ^ The maximum length of a schedule, in terms of primitive
  -- actions.
  -> Conc n r a
  -- ^ The computation to run many times
  -> n [(Either Failure a, Trace)]
sctLengthBound memtype lb = sctBound memtype $ Bounds Nothing Nothing (Just lb)

-- | Length bound function
lBound :: LengthBound -> BoundFunc
lBound (LengthBound lb) ts _ = length ts < lb

-- | Add a backtrack point. If the thread isn't runnable, add all
-- runnable threads.
lBacktrack :: BacktrackFunc
lBacktrack = backtrackAt (\_ _ -> False)

-------------------------------------------------------------------------------
-- DPOR

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
sctBound :: MonadRef r n
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The combined bounds.
  -> Conc n r a
  -- ^ The computation to run many times
  -> n [(Either Failure a, Trace)]
sctBound memtype cb conc = go initialState where
  -- Repeatedly run the computation gathering all the results and
  -- traces into a list until there are no schedules remaining to try.
  go dp = case nextPrefix dp of
    Just (prefix, conservative, sleep, ()) -> do
      (res, s, trace) <- runConcurrent scheduler
                                       memtype
                                       (initialSchedState sleep prefix)
                                       conc

      let bpoints = findBacktracks (schedBoundKill s) (schedBPoints s) trace
      let newDPOR = addTrace conservative trace dp

      if schedIgnore s
      then go newDPOR
      else ((res, trace):) <$> go (addBacktracks bpoints newDPOR)

    Nothing -> pure []

  -- Find the next schedule prefix.
  nextPrefix = findSchedulePrefix (>=initialThread) (const (0, ()))

  -- The DPOR scheduler.
  scheduler = dporSched (dependent memtype) (cBound cb)

  -- Find the new backtracking steps.
  findBacktracks = findBacktrackSteps (dependent' memtype) (cBacktrack cb)

  -- Incorporate a trace into the DPOR tree.
  addTrace = incorporateTrace (dependent memtype)

  -- Incorporate the new backtracking steps into the DPOR tree.
  addBacktracks = incorporateBacktrackSteps (cBound cb)

-------------------------------------------------------------------------------
-- Dependency function

-- | Check if an action is dependent on another.
dependent :: MemType -> DepState -> ThreadId -> ThreadAction -> ThreadId -> ThreadAction -> Bool
-- This is basically the same as 'dependent'', but can make use of the
-- additional information in a 'ThreadAction' to make different
-- decisions in a few cases:
--
--  - @SetNumCapabilities@ and @GetNumCapabilities@ are NOT dependent
--    IF the value read is the same as the value written. 'dependent''
--    can not see the value read (as it hasn't happened yet!), and so
--    is more pessimistic here.
--
--  - When masked interruptible, a thread can only be interrupted when
--    actually blocked. 'dependent'' has to assume that all
--    potentially-blocking operations can block, and so is more
--    pessimistic in this case.
--
--  - The @isBlock@ / @isBarrier@ case in 'dependent'' is NOT a sound
--    optimisation when dealing with a 'ThreadAction' that has been
--    converted to a 'Lookahead'. I'm not entirely sure why, which
--    makes me question whether the \"optimisation\" is sound as it
--    is.
--
--  - Dependency of STM transactions can be /greatly/ improved here,
--    as the 'Lookahead' does not know which @TVar@s will be touched,
--    and so has to assume all transactions are dependent.
dependent _ _ _ (SetNumCapabilities a) _ (GetNumCapabilities b) = a /= b
dependent _ ds _ (ThrowTo t) t2 a = t == t2 && canInterrupt ds t2 a
dependent memtype ds t1 a1 t2 a2 = case rewind a2 of
  Just l2
    | isSTM a1 && isSTM a2
      -> not . S.null $ tvarsOf a1 `S.intersection` tvarsOf a2
    | not (isBlock a1 && isBarrier (simplifyLookahead l2)) ->
      dependent' memtype ds t1 a1 t2 l2
  _ -> dependentActions memtype ds (simplifyAction a1) (simplifyAction a2)

  where
    isSTM (STM _ _) = True
    isSTM (BlockedSTM _) = True
    isSTM _ = False

-- | Variant of 'dependent' to handle 'Lookahead'.
--
-- Termination of the initial thread is handled specially in the DPOR
-- implementation.
dependent' :: MemType -> DepState -> ThreadId -> ThreadAction -> ThreadId -> Lookahead -> Bool
dependent' memtype ds t1 a1 t2 l2 = case (a1, l2) of
  -- Worst-case assumption: all IO is dependent.
  (LiftIO, WillLiftIO) -> True

  -- Throwing an exception is only dependent with actions in that
  -- thread and if the actions can be interrupted. We can also
  -- slightly improve on that by not considering interrupting the
  -- normal termination of a thread: it doesn't make a difference.
  (ThrowTo t, WillStop) | t == t2 -> False
  (Stop, WillThrowTo t) | t == t1 -> False
  (ThrowTo t, _)     -> t == t2 && canInterruptL ds t2 l2
  (_, WillThrowTo t) -> t == t1 && canInterrupt  ds t1 a1

  -- Another worst-case: assume all STM is dependent.
  (STM _ _, WillSTM) -> True

  -- This is a bit pessimistic: Set/Get are only dependent if the
  -- value set is not the same as the value that will be got, but we
  -- can't know that here. 'dependent' optimises this case.
  (GetNumCapabilities a, WillSetNumCapabilities b) -> a /= b
  (SetNumCapabilities _, WillGetNumCapabilities)   -> True
  (SetNumCapabilities a, WillSetNumCapabilities b) -> a /= b

  -- Don't impose a dependency if the other thread will immediately
  -- block already. This is safe because a context switch will occur
  -- anyway so there's no point pre-empting the action UNLESS the
  -- pre-emption would possibly allow for a different relaxed memory
  -- stage.
  _ | isBlock a1 && isBarrier (simplifyLookahead l2) -> False
    | otherwise -> dependentActions memtype ds (simplifyAction a1) (simplifyLookahead l2)

-- | Check if two 'ActionType's are dependent. Note that this is not
-- sufficient to know if two 'ThreadAction's are dependent, without
-- being so great an over-approximation as to be useless!
dependentActions :: MemType -> DepState -> ActionType -> ActionType -> Bool
dependentActions memtype ds a1 a2 = case (a1, a2) of
  -- Unsynchronised reads and writes are always dependent, even under
  -- a relaxed memory model, as an unsynchronised write gives rise to
  -- a commit, which synchronises.
  (UnsynchronisedRead          r1, _) | same crefOf && a2 /= PartiallySynchronisedCommit r1 -> a2 /= UnsynchronisedRead r1
  (UnsynchronisedWrite         r1, _) | same crefOf && a2 /= PartiallySynchronisedCommit r1 -> True
  (PartiallySynchronisedWrite  r1, _) | same crefOf && a2 /= PartiallySynchronisedCommit r1 -> True
  (PartiallySynchronisedModify r1, _) | same crefOf && a2 /= PartiallySynchronisedCommit r1 -> True
  (SynchronisedModify          r1, _) | same crefOf && a2 /= PartiallySynchronisedCommit r1 -> True

  -- Unsynchronised writes and synchronisation where the buffer is not
  -- empty.
  --
  -- See [RMMVerification], lemma 5.25.
  (UnsynchronisedWrite r1, _) | same crefOf && isCommit a2 r1 && isBuffered ds r1 -> False
  (_, UnsynchronisedWrite r2) | same crefOf && isCommit a1 r2 && isBuffered ds r2 -> False

  -- Unsynchronised reads where a memory barrier would flush a
  -- buffered write
  (UnsynchronisedRead r1, _) | isBarrier a2 -> isBuffered ds r1 && memtype /= SequentialConsistency
  (_, UnsynchronisedRead r2) | isBarrier a1 -> isBuffered ds r2 && memtype /= SequentialConsistency

  (_, _)
    -- Two actions on the same CRef where at least one is synchronised
    | same crefOf && (synchronises a1 (fromJust $ crefOf a1) || synchronises a2 (fromJust $ crefOf a2)) -> True
    -- Two actions on the same MVar
    | same mvarOf -> True

  _ -> False

  where
    same f = isJust (f a1) && f a1 == f a2

-------------------------------------------------------------------------------
-- Utilities

-- | Determine if an action is a commit or not.
isCommitRef :: ThreadAction -> Bool
isCommitRef (CommitRef _ _) = True
isCommitRef _ = False

-- | Extra threads created in a fork.
forkTids :: ThreadAction -> [ThreadId]
forkTids (Fork t) = [t]
forkTids _ = []

-- | Count the number of yields by a thread in a schedule prefix.
yieldCount :: ThreadId
  -- ^ The thread to count yields for.
  -> [(Decision, ThreadAction)]
  -> Lookahead
  -> Int
yieldCount tid ts l = go initialThread ts where
  go t ((Start    t', act):rest) = go' t t' act rest
  go t ((SwitchTo t', act):rest) = go' t t' act rest
  go t ((Continue,    act):rest) = go' t t  act rest
  go t []
    | t == tid && willYield l = 1
    | otherwise = 0

  {-# INLINE go' #-}
  go' t t' act rest
    | t == tid && didYield act = 1 + go t' rest
    | otherwise = go t' rest

-- | Get the maximum difference between the yield counts of all
-- threads in this schedule prefix.
maxYieldCountDiff :: [(Decision, ThreadAction)]
  -> Lookahead
  -> Int
maxYieldCountDiff ts l = go 0 yieldCounts where
  go m (yc:ycs) =
    let m' = m `max` foldl' (go' yc) 0 ycs
    in go m' ycs
  go m [] = m
  go' yc0 m yc = m `max` abs (yc0 - yc)

  yieldCounts = [yieldCount t ts l | t <- allTids ts]

  -- All the threads created during the lifetime of the system.
  allTids ((_, act):rest) =
    let tids' = forkTids act
    in if null tids' then allTids rest else tids' ++ allTids rest
  allTids [] = [initialThread]

-- | The \"true\" bound, which allows everything.
trueBound :: BoundFunc
trueBound _ _ = True

-- | Combine two bounds into a larger bound, where both must be
-- satisfied.
(&+&) :: BoundFunc -> BoundFunc -> BoundFunc
(&+&) b1 b2 ts dl = b1 ts dl && b2 ts dl

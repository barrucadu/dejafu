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
  -- This module provides both a generic function for BPOR, and also a
  -- pre-emption bounding BPOR runner, which is used by the
  -- "Test.DejaFu" module.
  --
  -- See /Bounded partial-order reduction/, K. Coons, M. Musuvathi,
  -- K. McKinley for more details.

    sctBounded

  -- * Combination Bounds

  -- | Combination schedule bounding, where individual bounds are
  -- enabled if they are set.
  --
  -- * Pre-emption + fair bounding is useful for programs which use
  --   loop/yield control flows but are otherwise terminating.
  --
  -- * Pre-emption, fair + length bounding is useful for
  -- non-terminating programs, and used by the testing functionality
  -- in @Test.DejaFu@.

  , Bounds(..)
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
  , pBound
  , pBacktrack

  -- ** Fair Bounding

  -- | BPOR using fair bounding. This bounds the maximum difference
  -- between the number of yield operations different threads have
  -- performed.
  --
  -- See the BPOR paper for more details.

  , FairBound(..)
  , defaultFairBound
  , sctFairBound
  , fBound
  , fBacktrack

  -- ** Length Bounding

  -- | BPOR using length bounding. This bounds the maximum length (in
  -- terms of primitive actions) of an execution.

  , LengthBound(..)
  , defaultLengthBound
  , sctLengthBound
  , lBound
  , lBacktrack

  -- * Backtracking

  , I.BacktrackStep(..)
  , I.BacktrackFunc
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Ref (MonadRef)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as S
import qualified Test.DPOR.Internal as I

import Test.DejaFu.Common
import Test.DejaFu.Conc

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

-- | An SCT runner using a bounded scheduler
sctBound :: MonadRef r n
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The combined bounds.
  -> Conc n r a
  -- ^ The computation to run many times
  -> n [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctBound memtype cb = sctBounded memtype (cBound cb) (cBacktrack cb)

-- | Combination bound function
cBound :: Bounds -> I.BoundFunc ThreadId ThreadAction Lookahead
cBound (Bounds pb fb lb) =
  maybe trueBound pBound pb &+&
  maybe trueBound fBound fb &+&
  maybe trueBound lBound lb

-- | Combination backtracking function. Add all backtracking points
-- corresponding to enabled bound functions.
--
-- If no bounds are enabled, just backtrack to the given point.
cBacktrack :: Bounds -> I.BacktrackFunc ThreadId ThreadAction Lookahead s
cBacktrack (Bounds Nothing Nothing Nothing) bs i t = I.backtrackAt (const False) False bs i t
cBacktrack (Bounds pb fb lb) bs i t = lBack . fBack $ pBack bs where
  pBack backs = if isJust pb then pBacktrack   backs i t else backs
  fBack backs = if isJust fb then fBacktrack   backs i t else backs
  lBack backs = if isJust lb then lBacktrack backs i t else backs

-------------------------------------------------------------------------------
-- Pre-emption bounding

newtype PreemptionBound = PreemptionBound Int
  deriving (NFData, Enum, Eq, Ord, Num, Real, Integral, Read, Show)

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
  -> n [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctPreBound memtype pb = sctBounded memtype (pBound pb) pBacktrack

-- | Pre-emption bound function. This does not count pre-emptive
-- context switches to a commit thread.
pBound :: PreemptionBound -> I.BoundFunc ThreadId ThreadAction Lookahead
pBound (PreemptionBound pb) ts dl = preEmpCount ts dl <= pb

-- | Add a backtrack point, and also conservatively add one prior to
-- the most recent transition before that point. This may result in
-- the same state being reached multiple times, but is needed because
-- of the artificial dependency imposed by the bound.
pBacktrack :: I.BacktrackFunc ThreadId ThreadAction Lookahead s
pBacktrack bs i tid =
  maybe id (\j' b -> backtrack True b j' tid) j $ backtrack False bs i tid

  where
    -- Index of the conservative point
    j = goJ . reverse . pairs $ zip [0..i-1] bs where
      goJ (((_,b1), (j',b2)):rest)
        | I.bcktThreadid b1 /= I.bcktThreadid b2
          && not (isCommitRef . snd $ I.bcktDecision b1)
          && not (isCommitRef . snd $ I.bcktDecision b2) = Just j'
        | otherwise = goJ rest
      goJ [] = Nothing

    -- List of adjacent pairs
    {-# INLINE pairs #-}
    pairs = zip <*> tail

    -- Add a backtracking point.
    backtrack = I.backtrackAt $ const False

-------------------------------------------------------------------------------
-- Fair bounding

newtype FairBound = FairBound Int
  deriving (NFData, Enum, Eq, Ord, Num, Real, Integral, Read, Show)

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
  -> n [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctFairBound memtype fb = sctBounded memtype (fBound fb) fBacktrack

-- | Fair bound function
fBound :: FairBound -> I.BoundFunc ThreadId ThreadAction Lookahead
fBound (FairBound fb) ts (_, l) = maxYieldCountDiff ts l <= fb

-- | Add a backtrack point. If the thread isn't runnable, or performs
-- a release operation, add all runnable threads.
fBacktrack :: I.BacktrackFunc ThreadId ThreadAction Lookahead s
fBacktrack bs i t = I.backtrackAt check False bs i t where
  -- True if a release operation is performed.
  check b = Just True == (willRelease <$> M.lookup t (I.bcktRunnable b))

-------------------------------------------------------------------------------
-- Length bounding

newtype LengthBound = LengthBound Int
  deriving (NFData, Enum, Eq, Ord, Num, Real, Integral, Read, Show)

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
  -> n [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctLengthBound memtype lb = sctBounded memtype (lBound lb) lBacktrack

-- | Length bound function
lBound :: LengthBound -> I.BoundFunc tid action lookahead
lBound (LengthBound lb) ts _ = length ts < lb

-- | Add a backtrack point. If the thread isn't runnable, add all
-- runnable threads.
lBacktrack :: Ord tid => I.BacktrackFunc tid action lookahead s
lBacktrack = I.backtrackAt (const False) False

-------------------------------------------------------------------------------
-- DPOR

-- | SCT via BPOR.
--
-- Schedules are generated by running the computation with a
-- deterministic scheduler with some initial list of decisions, after
-- which the supplied function is called. At each step of execution,
-- possible-conflicting actions are looked for, if any are found,
-- \"backtracking points\" are added, to cause the events to happen in
-- a different order in a future execution.
--
-- Note that unlike with non-bounded partial-order reduction, this may
-- do some redundant work as the introduction of a bound can make
-- previously non-interfering events interfere with each other.
sctBounded :: MonadRef r n
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> I.BoundFunc ThreadId ThreadAction Lookahead
  -- ^ Check if a prefix trace is within the bound
  -> I.BacktrackFunc ThreadId ThreadAction Lookahead DepState
  -- ^ Add a new backtrack point, this takes the history of the
  -- execution so far, the index to insert the backtracking point, and
  -- the thread to backtrack to. This may insert more than one
  -- backtracking point.
  -> Conc n r a
  -> n [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctBounded memtype bf backtrack conc = go (I.initialState initialThread) where
  -- Repeatedly run the computation gathering all the results and
  -- traces into a list until there are no schedules remaining to try.
  go dp = case nextPrefix dp of
    Just (prefix, conservative, sleep, ()) -> do
      (res, s, trace) <- runConcurrent scheduler
                                       memtype
                                       (I.initialSchedState initialDepState sleep prefix)
                                       conc

      let bpoints = findBacktracks (I.schedBoundKill s) (I.schedBPoints s) trace
      let newDPOR = addTrace conservative trace dp

      if I.schedIgnore s
      then go newDPOR
      else ((res, trace):) <$> go (pruneCommits $ addBacktracks bpoints newDPOR)

    Nothing -> pure []

  -- Find the next schedule prefix.
  nextPrefix = I.findSchedulePrefix (>=initialThread) (const (0, ()))

  -- The DPOR scheduler.
  scheduler = I.dporSched didYield willYield (dependent memtype) killsDaemons updateDepState bf

  -- Find the new backtracking steps.
  findBacktracks = I.findBacktrackSteps initialDepState updateDepState (dependent' memtype) backtrack

  -- Incorporate a trace into the DPOR tree.
  addTrace = I.incorporateTrace initialDepState updateDepState (dependent memtype)

  -- Incorporate the new backtracking steps into the DPOR tree.
  addBacktracks = I.incorporateBacktrackSteps bf

  -- Check if an action kills daemon threads.
  killsDaemons _ (t, l) _ = t == initialThread && case l of WillStop -> True; _ -> False

-------------------------------------------------------------------------------
-- Post-processing

-- | Remove commits from the todo sets where every other action will
-- result in a write barrier (and so a commit) occurring.
--
-- To get the benefit from this, do not execute commit actions from
-- the todo set until there are no other choises.
pruneCommits :: I.DPOR ThreadId ThreadAction -> I.DPOR ThreadId ThreadAction
pruneCommits bpor
  | not onlycommits || not alldonesync = go bpor
  | otherwise = go bpor { I.dporTodo = M.empty }

  where
    go b = b { I.dporDone = pruneCommits <$> I.dporDone bpor }

    onlycommits = all (<initialThread) . M.keys $ I.dporTodo bpor
    alldonesync = all barrier . M.elems $ I.dporDone bpor

    barrier = isBarrier . simplifyAction . fromJust . I.dporAction

-------------------------------------------------------------------------------
-- Dependency function

-- | Check if an action is dependent on another.
dependent :: MemType -> DepState -> (ThreadId, ThreadAction) -> (ThreadId, ThreadAction) -> Bool
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
dependent _ _ (_, SetNumCapabilities a) (_, GetNumCapabilities b) = a /= b
dependent _ ds (_, ThrowTo t) (t2, a) = t == t2 && canInterrupt ds t2 a
dependent memtype ds (t1, a1) (t2, a2) = case rewind a2 of
  Just l2
    | isSTM a1 && isSTM a2
      -> not . S.null $ tvarsOf a1 `S.intersection` tvarsOf a2
    | not (isBlock a1 && isBarrier (simplifyLookahead l2)) ->
      dependent' memtype ds (t1, a1) (t2, l2)
  _ -> dependentActions memtype ds (simplifyAction a1) (simplifyAction a2)

  where
    isSTM (STM _ _) = True
    isSTM (BlockedSTM _) = True
    isSTM _ = False

-- | Variant of 'dependent' to handle 'Lookahead'.
--
-- Termination of the initial thread is handled specially in the DPOR
-- implementation.
dependent' :: MemType -> DepState -> (ThreadId, ThreadAction) -> (ThreadId, Lookahead) -> Bool
dependent' memtype ds (t1, a1) (t2, l2) = case (a1, l2) of
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
  (UnsynchronisedRead  r1, UnsynchronisedWrite r2) -> r1 == r2
  (UnsynchronisedWrite r1, UnsynchronisedRead  r2) -> r1 == r2
  (UnsynchronisedWrite r1, UnsynchronisedWrite r2) -> r1 == r2

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
-- Dependency function state

data DepState = DepState
  { depCRState :: Map CRefId Bool
  -- ^ Keep track of which @CRef@s have buffered writes.
  , depMaskState :: Map ThreadId MaskingState
  -- ^ Keep track of thread masking states. If a thread isn't present,
  -- the masking state is assumed to be @Unmasked@. This nicely
  -- provides compatibility with dpor-0.1, where the thread IDs are
  -- not available.
  }

instance NFData DepState where
  -- Cheats: 'MaskingState' has no 'NFData' instance.
  rnf ds = rnf (depCRState ds, M.keys (depMaskState ds))

-- | Initial dependency state.
initialDepState :: DepState
initialDepState = DepState M.empty M.empty

-- | Update the 'CRef' buffer state with the action that has just
-- happened.
updateDepState :: DepState -> (ThreadId, ThreadAction) -> DepState
updateDepState depstate (tid, act) = DepState
  { depCRState   = updateCRState       act $ depCRState   depstate
  , depMaskState = updateMaskState tid act $ depMaskState depstate
  }

-- | Update the 'CRef' buffer state with the action that has just
-- happened.
updateCRState :: ThreadAction -> Map CRefId Bool -> Map CRefId Bool
updateCRState (CommitRef _ r) = M.delete r
updateCRState (WriteRef    r) = M.insert r True
updateCRState ta
  | isBarrier $ simplifyAction ta = const M.empty
  | otherwise = id

-- | Update the thread masking state with the action that has just
-- happened.
updateMaskState :: ThreadId -> ThreadAction -> Map ThreadId MaskingState -> Map ThreadId MaskingState
updateMaskState tid (Fork tid2) = \masks -> case M.lookup tid masks of
  -- A thread inherits the masking state of its parent.
  Just ms -> M.insert tid2 ms masks
  Nothing -> masks
updateMaskState tid (SetMasking   _ ms) = M.insert tid ms
updateMaskState tid (ResetMasking _ ms) = M.insert tid ms
updateMaskState _ _ = id

-- | Check if a 'CRef' has a buffered write pending.
isBuffered :: DepState -> CRefId -> Bool
isBuffered depstate r = M.findWithDefault False r (depCRState depstate)

-- | Check if an exception can interrupt a thread (action).
canInterrupt :: DepState -> ThreadId -> ThreadAction -> Bool
canInterrupt depstate tid act
  -- If masked interruptible, blocked actions can be interrupted.
  | isMaskedInterruptible depstate tid = case act of
    BlockedPutVar  _ -> True
    BlockedReadVar _ -> True
    BlockedTakeVar _ -> True
    BlockedSTM     _ -> True
    BlockedThrowTo _ -> True
    _ -> False
  -- If masked uninterruptible, nothing can be.
  | isMaskedUninterruptible depstate tid = False
  -- If no mask, anything can be.
  | otherwise = True

-- | Check if an exception can interrupt a thread (lookahead).
canInterruptL :: DepState -> ThreadId -> Lookahead -> Bool
canInterruptL depstate tid lh
  -- If masked interruptible, actions which can block may be
  -- interrupted.
  | isMaskedInterruptible depstate tid = case lh of
    WillPutVar  _ -> True
    WillReadVar _ -> True
    WillTakeVar _ -> True
    WillSTM       -> True
    WillThrowTo _ -> True
    _ -> False
  -- If masked uninterruptible, nothing can be.
  | isMaskedUninterruptible depstate tid = False
  -- If no mask, anything can be.
  | otherwise = True

-- | Check if a thread is masked interruptible.
isMaskedInterruptible :: DepState -> ThreadId -> Bool
isMaskedInterruptible depstate tid =
  M.lookup tid (depMaskState depstate) == Just MaskedInterruptible

-- | Check if a thread is masked uninterruptible.
isMaskedUninterruptible :: DepState -> ThreadId -> Bool
isMaskedUninterruptible depstate tid =
  M.lookup tid (depMaskState depstate) == Just MaskedUninterruptible

-------------------------------------------------------------------------------
-- Utilities

-- | Determine if an action is a commit or not.
isCommitRef :: ThreadAction -> Bool
isCommitRef (CommitRef _ _) = True
isCommitRef _ = False

-- | Check if a thread yielded.
didYield :: ThreadAction -> Bool
didYield Yield = True
didYield _ = False

-- | Check if a thread will yield.
willYield :: Lookahead -> Bool
willYield WillYield = True
willYield _ = False

-- | Extra threads created in a fork.
forkTids :: ThreadAction -> [ThreadId]
forkTids (Fork t) = [t]
forkTids _ = []

-- | Count the number of yields by a thread in a schedule prefix.
yieldCount :: ThreadId
  -- ^ The thread to count yields for.
  -> [(Decision ThreadId, ThreadAction)]
  -> Lookahead
  -> Int
yieldCount tid ts l = go initialThread ts where
  go t ((Start    t', act):rest) = go' t t' act rest
  go t ((SwitchTo t', act):rest) = go' t t' act rest
  go t ((Continue,    act):rest) = go' t t  act rest
  go t []
    | t == tid && willYield l = 1
    | otherwise = 0

  go' t t' act rest
    | t == tid && didYield act = 1 + go t' rest
    | otherwise = go t' rest

-- | Get the maximum difference between the yield counts of all
-- threads in this schedule prefix.
maxYieldCountDiff :: [(Decision ThreadId, ThreadAction)]
  -> Lookahead
  -> Int
maxYieldCountDiff ts l = maximum yieldCountDiffs where
  yieldsBy tid = yieldCount tid ts l
  yieldCounts = [yieldsBy tid | tid <- nub $ allTids ts]
  yieldCountDiffs = [y1 - y2 | y1 <- yieldCounts, y2 <- yieldCounts]

  -- All the threads created during the lifetime of the system.
  allTids ((_, act):rest) =
    let tids' = forkTids act
    in if null tids' then allTids rest else tids' ++ allTids rest
  allTids [] = [initialThread]

-- | The \"true\" bound, which allows everything.
trueBound :: I.BoundFunc tid action lookahead
trueBound _ _ = True

-- | Combine two bounds into a larger bound, where both must be
-- satisfied.
(&+&) :: I.BoundFunc tid action lookahead
      -> I.BoundFunc tid action lookahead
      -> I.BoundFunc tid action lookahead
(&+&) b1 b2 ts dl = b1 ts dl && b2 ts dl

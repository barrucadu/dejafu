-- |
-- Module      : Test.DejaFu.SCT
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
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
  , cBacktrack
  , cBound

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
  , pBacktrack
  , pBound

  -- ** Fair Bounding

  -- | BPOR using fair bounding. This bounds the maximum difference
  -- between the number of yield operations different threads have
  -- performed.
  --
  -- See the BPOR paper for more details.

  , FairBound(..)
  , defaultFairBound
  , sctFairBound
  , fBacktrack
  , fBound

  -- ** Length Bounding

  -- | BPOR using length bounding. This bounds the maximum length (in
  -- terms of primitive actions) of an execution.

  , LengthBound(..)
  , defaultLengthBound
  , sctLengthBound

  -- * Backtracking

  , BacktrackStep(..)
  , BacktrackFunc

  -- * Unsystematic and Incomplete Techniques

  -- | These algorithms do not promise to visit every unique schedule,
  -- or to avoid trying the same schedule twice. This greatly reduces
  -- the memory requirements in testing complex systems. Randomness
  -- drives everything. No partial-order reduction is done, but
  -- schedule bounds are still available.

  , unsystematicRandom
  , unsystematicPCT
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Ref (MonadRef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as S
import Test.DPOR ( DPOR(..), dpor
                 , BacktrackFunc, BacktrackStep(..), backtrackAt
                 , BoundFunc, (&+&), trueBound
                 , PreemptionBound(..), defaultPreemptionBound, preempBacktrack
                 , FairBound(..), defaultFairBound, fairBound, fairBacktrack
                 , LengthBound(..), defaultLengthBound, lenBound, lenBacktrack
                 )
import qualified Test.DPOR.Random as Unsystematic
import System.Random (RandomGen)

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
cBound :: Bounds -> BoundFunc ThreadId ThreadAction Lookahead
cBound (Bounds pb fb lb) = maybe trueBound pBound pb &+& maybe trueBound fBound fb &+& maybe trueBound lenBound lb

-- | Combination backtracking function. Add all backtracking points
-- corresponding to enabled bound functions.
--
-- If no bounds are enabled, just backtrack to the given point.
cBacktrack :: Bounds -> BacktrackFunc ThreadId ThreadAction Lookahead s
cBacktrack (Bounds Nothing Nothing Nothing) bs i t = backtrackAt (const False) False bs i t
cBacktrack (Bounds pb fb lb) bs i t = lBack . fBack $ pBack bs where
  pBack backs = if isJust pb then pBacktrack   backs i t else backs
  fBack backs = if isJust fb then fBacktrack   backs i t else backs
  lBack backs = if isJust lb then lenBacktrack backs i t else backs

-------------------------------------------------------------------------------
-- Pre-emption bounding

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

-- | Add a backtrack point, and also conservatively add one prior to
-- the most recent transition before that point. This may result in
-- the same state being reached multiple times, but is needed because
-- of the artificial dependency imposed by the bound.
pBacktrack :: BacktrackFunc ThreadId ThreadAction Lookahead s
pBacktrack = preempBacktrack isCommitRef

-- | Pre-emption bound function. This is different to @preempBound@ in
-- that it does not count pre-emptive context switches to a commit
-- thread.
pBound :: PreemptionBound -> BoundFunc ThreadId ThreadAction Lookahead
pBound (PreemptionBound pb) ts dl = preEmpCount ts dl <= pb

-------------------------------------------------------------------------------
-- Fair bounding

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
fBound :: FairBound -> BoundFunc ThreadId ThreadAction Lookahead
fBound = fairBound didYield willYield (\act -> case act of Fork t -> [t]; _ -> [])

-- | Add a backtrack point. If the thread isn't runnable, or performs
-- a release operation, add all runnable threads.
fBacktrack :: BacktrackFunc ThreadId ThreadAction Lookahead s
fBacktrack = fairBacktrack willRelease

-------------------------------------------------------------------------------
-- Length bounding

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
sctLengthBound memtype lb = sctBounded memtype (lenBound lb) lenBacktrack

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
  -> BoundFunc ThreadId ThreadAction Lookahead
  -- ^ Check if a prefix trace is within the bound
  -> BacktrackFunc ThreadId ThreadAction Lookahead DepState
  -- ^ Add a new backtrack point, this takes the history of the
  -- execution so far, the index to insert the backtracking point, and
  -- the thread to backtrack to. This may insert more than one
  -- backtracking point.
  -> Conc n r a
  -> n [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctBounded memtype bf backtrack conc =
  dpor didYield
       willYield
       initialDepState
       updateDepState
       (dependent  memtype)
       (dependent' memtype)
       (\_ (t, l) _ -> t == initialThread && case l of WillStop -> True; _ -> False)
       initialThread
       (>=initialThread)
       bf
       backtrack
       pruneCommits
       (\sched s -> runConcurrent sched memtype s conc)

-------------------------------------------------------------------------------
-- Unsystematic and Incomplete Techniques

-- | Try random schedules up to some limit.
unsystematicRandom :: (MonadRef r n, RandomGen g)
  => Int
  -- ^ Execution limit.
  -> g
  -- ^ Random number generator
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> BoundFunc ThreadId ThreadAction Lookahead
  -- ^ Check if a prefix trace is within the bound.
  -> Conc n r a
  -> n [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
unsystematicRandom lim gen memtype bf conc =
  Unsystematic.unsystematicRandom lim
                                  gen
                                  (Just bf)
                                  (\sched s -> runConcurrent sched memtype s conc)

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
unsystematicPCT :: (MonadRef r n, RandomGen g)
  => Int
  -- ^ Execution limit.
  -> g
  -- ^ Random number generator
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> BoundFunc ThreadId ThreadAction Lookahead
  -- ^ Check if a prefix trace is within the bound.
  -> Conc n r a
  -> n [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
unsystematicPCT lim gen memtype bf conc =
  Unsystematic.unsystematicPCT lim
                               gen
                               (Just bf)
                               (\sched s -> runConcurrent sched memtype s conc)

-------------------------------------------------------------------------------
-- Post-processing

-- | Remove commits from the todo sets where every other action will
-- result in a write barrier (and so a commit) occurring.
--
-- To get the benefit from this, do not execute commit actions from
-- the todo set until there are no other choises.
pruneCommits :: DPOR ThreadId ThreadAction -> DPOR ThreadId ThreadAction
pruneCommits bpor
  | not onlycommits || not alldonesync = go bpor
  | otherwise = go bpor { dporTodo = M.empty }

  where
    go b = b { dporDone = pruneCommits <$> dporDone bpor }

    onlycommits = all (<initialThread) . M.keys $ dporTodo bpor
    alldonesync = all barrier . M.elems $ dporDone bpor

    barrier = isBarrier . simplifyAction . fromJust . dporAction

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
-- Strictly speaking, there is a dependency between the @Stop@ of
-- thread 0 and everything else, but dpor handles that more nicely
-- through its \"daemon killing\" special case.
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

{-# LANGUAGE RankNTypes #-}

-- | Systematic testing for concurrent computations.
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

    BacktrackStep(..)

  , sctBounded
  , sctBoundedIO

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
  , sctBoundIO

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
  , sctPreBoundIO
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
  , sctFairBoundIO
  , fBacktrack
  , fBound

  -- ** Length Bounding

  -- | BPOR using length bounding. This bounds the maximum length (in
  -- terms of primitive actions) of an execution.

  , LengthBound(..)
  , defaultLengthBound
  , sctLengthBound
  , sctLengthBoundIO
  ) where

import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Maybe (isJust)
import Test.DejaFu.Deterministic
import Test.DejaFu.Deterministic.Internal (initialThread, preEmpCount, willRelease)
import Test.DejaFu.DPOR ( BacktrackStep(..), backtrackAt
                        , BoundFunc, (&+&), trueBound
                        , PreemptionBound(..), defaultPreemptionBound, preempBacktrack
                        , FairBound(..), defaultFairBound, fairBound, fairBacktrack
                        , LengthBound(..), defaultLengthBound, lenBound, lenBacktrack
                        , dpor
                        )
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

-- | An SCT runner using a bounded scheduler
sctBound :: MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The combined bounds.
  -> (forall t. ConcST t a)
  -- ^ The computation to run many times
  -> [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctBound memtype cb = sctBounded memtype (cBound cb) (cBacktrack cb)

-- | Variant of 'sctBound' for computations which do 'IO'.
sctBoundIO :: MemType
  -> Bounds
  -> ConcIO a
  -> IO [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctBoundIO memtype cb = sctBoundedIO memtype (cBound cb) (cBacktrack cb)

-- | Combination bound function
cBound :: Bounds -> BoundFunc ThreadId ThreadAction Lookahead
cBound (Bounds pb fb lb) = maybe trueBound pBound pb &+& maybe trueBound fBound fb &+& maybe trueBound lenBound lb

-- | Combination backtracking function. Add all backtracking points
-- corresponding to enabled bound functions.
--
-- If no bounds are enabled, just backtrack to the given point.
cBacktrack :: Bounds
  -> [BacktrackStep ThreadId ThreadAction Lookahead s]
  -> Int
  -> ThreadId
  -> [BacktrackStep ThreadId ThreadAction Lookahead s]
cBacktrack (Bounds Nothing Nothing Nothing) bs i t = backtrackAt (const False) False bs i t
cBacktrack (Bounds pb fb lb) bs i t = lBack . fBack $ pBack bs where
  pBack backs = if isJust pb then pBacktrack   backs i t else backs
  fBack backs = if isJust fb then fBacktrack   backs i t else backs
  lBack backs = if isJust lb then lenBacktrack backs i t else backs

-------------------------------------------------------------------------------
-- Pre-emption bounding

-- | An SCT runner using a pre-emption bounding scheduler.
sctPreBound :: MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> PreemptionBound
  -- ^ The maximum number of pre-emptions to allow in a single
  -- execution
  -> (forall t. ConcST t a)
  -- ^ The computation to run many times
  -> [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctPreBound memtype pb = sctBounded memtype (pBound pb) pBacktrack

-- | Variant of 'sctPreBound' for computations which do 'IO'.
sctPreBoundIO :: MemType
  -> PreemptionBound
  -> ConcIO a
  -> IO [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctPreBoundIO memtype pb = sctBoundedIO memtype (pBound pb) pBacktrack

-- | Add a backtrack point, and also conservatively add one prior to
-- the most recent transition before that point. This may result in
-- the same state being reached multiple times, but is needed because
-- of the artificial dependency imposed by the bound.
pBacktrack :: [BacktrackStep ThreadId ThreadAction Lookahead s]
  -- ^ The current backtracking points.
  -> Int
  -- ^ The point to backtrack to.
  -> ThreadId
  -- ^ The thread to backtrack to.
  -> [BacktrackStep ThreadId ThreadAction Lookahead s]
pBacktrack = preempBacktrack isCommit

-- | Pre-emption bound function. This is different to @preempBound@ in
-- that it does not count pre-emptive context switches to a commit
-- thread.
pBound :: PreemptionBound -> BoundFunc ThreadId ThreadAction Lookahead
pBound (PreemptionBound pb) ts dl = preEmpCount ts dl <= pb

-------------------------------------------------------------------------------
-- Fair bounding

-- | An SCT runner using a fair bounding scheduler.
sctFairBound :: MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> FairBound
  -- ^ The maximum difference between the number of yield operations
  -- performed by different threads.
  -> (forall t. ConcST t a)
  -- ^ The computation to run many times
  -> [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctFairBound memtype fb = sctBounded memtype (fBound fb) fBacktrack

-- | Variant of 'sctFairBound' for computations which do 'IO'.
sctFairBoundIO :: MemType
  -> FairBound
  -> ConcIO a
  -> IO [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctFairBoundIO memtype fb = sctBoundedIO memtype (fBound fb) fBacktrack

-- | Fair bound function
fBound :: FairBound -> BoundFunc ThreadId ThreadAction Lookahead
fBound = fairBound didYield willYield (\act -> case act of Fork t -> [t]; _ -> [])

-- | Add a backtrack point. If the thread isn't runnable, or performs
-- a release operation, add all runnable threads.
fBacktrack :: [BacktrackStep ThreadId ThreadAction Lookahead s]
  -- ^ The current backtracking points.
  -> Int
  -- ^ The point to backtrack to.
  -> ThreadId
  -- ^ The thread to backtrack to.
  -> [BacktrackStep ThreadId ThreadAction Lookahead s]
fBacktrack = fairBacktrack willRelease

-------------------------------------------------------------------------------
-- Length bounding

-- | An SCT runner using a length bounding scheduler.
sctLengthBound :: MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> LengthBound
  -- ^ The maximum length of a schedule, in terms of primitive
  -- actions.
  -> (forall t. ConcST t a)
  -- ^ The computation to run many times
  -> [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctLengthBound memtype lb = sctBounded memtype (lenBound lb) lenBacktrack

-- | Variant of 'sctFairBound' for computations which do 'IO'.
sctLengthBoundIO :: MemType
  -> LengthBound
  -> ConcIO a
  -> IO [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctLengthBoundIO memtype lb = sctBoundedIO memtype (lenBound lb) lenBacktrack

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
sctBounded :: MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> BoundFunc ThreadId ThreadAction Lookahead
  -- ^ Check if a prefix trace is within the bound
  -> ([BacktrackStep ThreadId ThreadAction Lookahead CRState] -> Int -> ThreadId -> [BacktrackStep ThreadId ThreadAction Lookahead CRState])
  -- ^ Add a new backtrack point, this takes the history of the
  -- execution so far, the index to insert the backtracking point, and
  -- the thread to backtrack to. This may insert more than one
  -- backtracking point.
  -> (forall t. ConcST t a) -> [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctBounded memtype bf backtrack c = runIdentity $ sctBoundedM memtype bf backtrack run where
  run memty sched s = Identity $ runConcST sched memty s c

-- | Variant of 'sctBounded' for computations which do 'IO'.
sctBoundedIO :: MemType
  -> BoundFunc ThreadId ThreadAction Lookahead
  -> ([BacktrackStep ThreadId ThreadAction Lookahead CRState] -> Int -> ThreadId -> [BacktrackStep ThreadId ThreadAction Lookahead CRState])
  -> ConcIO a -> IO [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctBoundedIO memtype bf backtrack c = sctBoundedM memtype bf backtrack run where
  run memty sched s = runConcIO sched memty s c

-- | Generic SCT runner.
sctBoundedM :: Monad m
  => MemType
  -> ([(Decision ThreadId, ThreadAction)] -> (Decision ThreadId, Lookahead) -> Bool)
  -> ([BacktrackStep ThreadId ThreadAction Lookahead CRState] -> Int -> ThreadId -> [BacktrackStep ThreadId ThreadAction Lookahead CRState])
  -> (forall s. MemType -> Scheduler ThreadId ThreadAction Lookahead s -> s -> m (Either Failure a, s, Trace ThreadId ThreadAction Lookahead))
  -- ^ Monadic runner, with computation fixed.
  -> m [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
sctBoundedM memtype bf backtrack run =
  dpor didYield
       willYield
       initialCRState
       updateCRState
       (dependent  memtype)
       (dependent' memtype)
       initialThread
       (>=initialThread)
       bf
       backtrack
       pruneCommits
       (run memtype)

-------------------------------------------------------------------------------
-- Utilities

-- | Determine if an action is a commit or not.
isCommit :: ThreadAction -> Bool
isCommit (CommitRef _ _) = True
isCommit _ = False

-- | Check if a thread yielded.
didYield :: ThreadAction -> Bool
didYield Yield = True
didYield _ = False

-- | Check if a thread will yield.
willYield :: Lookahead -> Bool
willYield WillYield = True
willYield _ = False

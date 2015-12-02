{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

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
  , BoundFunc

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

  -- ** Length Bounding

  -- | BPOR using length bounding. This bounds the maximum length (in
  -- terms of primitive actions) of an execution.

  , LengthBound(..)
  , defaultLengthBound
  , sctLengthBound
  , sctLengthBoundIO

  -- * Utilities

  , (&+&)
  , trueBound
  , backtrackAt
  , tidOf
  , decisionOf
  , activeTid
  , preEmpCount
  , preEmpCount'
  , yieldCount
  , maxYieldCountDiff
  , initialise
  , initialCVState
  , updateCVState
  , willBlock
  , willBlockSafely
  ) where

import Control.DeepSeq (NFData, force)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.List (nub, partition)
import Data.Sequence (Seq, (|>))
import Data.Map (Map)
import Data.Maybe (isNothing, isJust, fromJust)
import Test.DejaFu.Deterministic
import Test.DejaFu.Deterministic.Internal (willRelease)
import Test.DejaFu.SCT.Internal

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Sq
import qualified Data.Set as S

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

-- | A bounding function takes the scheduling decisions so far and a
-- decision chosen to come next, and returns if that decision is
-- within the bound.
type BoundFunc = [(Decision, ThreadAction)] -> (Decision, Lookahead) -> Bool

-- | Combine two bounds into a larger bound, where both must be
-- satisfied.
(&+&) :: BoundFunc -> BoundFunc -> BoundFunc
(&+&) b1 b2 ts dl = b1 ts dl && b2 ts dl

-- | The \"true\" bound, which allows everything.
trueBound :: BoundFunc
trueBound _ _ = True

-- * Combined Bounds

data Bounds = Bounds
  { preemptionBound :: Maybe PreemptionBound
  , fairBound       :: Maybe FairBound
  , lengthBound     :: Maybe LengthBound
  }

-- | All bounds enabled, using their default values.
defaultBounds :: Bounds
defaultBounds = Bounds
  { preemptionBound = Just defaultPreemptionBound
  , fairBound       = Just defaultFairBound
  , lengthBound     = Just defaultLengthBound
  }

-- | An SCT runner using a bounded scheduler
sctBound :: MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The combined bounds.
  -> (forall t. ConcST t a)
  -- ^ The computation to run many times
  -> [(Either Failure a, Trace)]
sctBound memtype cb = sctBounded memtype (cBound cb) (cBacktrack cb)

-- | Variant of 'sctBound' for computations which do 'IO'.
sctBoundIO :: MemType -> Bounds -> ConcIO a -> IO [(Either Failure a, Trace)]
sctBoundIO memtype cb = sctBoundedIO memtype (cBound cb) (cBacktrack cb)

-- | Combination bound function
cBound :: Bounds -> BoundFunc
cBound (Bounds pb fb lb) = maybe trueBound pbBound pb &+& maybe trueBound fBound fb &+& maybe trueBound lBound lb

-- | Combination backtracking function. Add all backtracking points
-- corresponding to enabled bound functions.
cBacktrack :: Bounds -> [BacktrackStep] -> Int -> ThreadId -> [BacktrackStep]
cBacktrack (Bounds pb fb lb) bs i t = lBack . fBack $ pBack bs where
  pBack backs = if isJust pb then pbBacktrack backs i t else backs
  fBack backs = if isJust fb then fBacktrack  backs i t else backs
  lBack backs = if isJust lb then lBacktrack  backs i t else backs

-- | Add a backtracking point. If the thread isn't runnable, add all
-- runnable threads.
--
-- If the backtracking point is already present, don't re-add it
-- UNLESS this is a conservative backtracking point.
backtrackAt :: (BacktrackStep -> Bool)
  -- ^ If this returns @True@, backtrack to all runnable threads,
  -- rather than just the given thread.
  -> Bool
  -- ^ Is this backtracking point conservative? Conservative points
  -- are always explored, whereas non-conservative ones might be
  -- skipped based on future information.
  -> [BacktrackStep]
  -- ^ Original list of backtracking steps.
  -> Int
  -- ^ Index in the list to add the step. MUST be in the list.
  -> ThreadId
  -- ^ The thread to backtrack to. If not runnable at that step, all
  -- runnable threads will be backtracked to instead.
  -> [BacktrackStep]
backtrackAt toAll conservative bs i tid = go bs i where
  go bx@(b:rest) 0
    -- If the backtracking point is already present, don't re-add it,
    -- UNLESS this would force it to backtrack (it's conservative)
    -- where before it might not.
    | not (toAll b) && tid `M.member` _runnable b =
      let val = M.lookup tid $ _backtrack b
      in  if isNothing val || (val == Just False && conservative)
          then b { _backtrack = M.insert tid conservative $ _backtrack b } : rest
          else bx

    -- Otherwise just backtrack to everything runnable.
    | otherwise = b { _backtrack = M.fromList [ (t',conservative) | t' <- M.keys $ _runnable b ] } : rest

  go (b:rest) n = b : go rest (n-1)
  go [] _ = error "Ran out of schedule whilst backtracking!"

-- * Pre-emption bounding

newtype PreemptionBound = PreemptionBound Int
  deriving (NFData, Enum, Eq, Ord, Num, Real, Integral, Read, Show)

-- | A sensible default pre-emption bound: 2
defaultPreemptionBound :: PreemptionBound
defaultPreemptionBound = 2

-- | An SCT runner using a pre-emption bounding scheduler.
sctPreBound :: MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> PreemptionBound
  -- ^ The maximum number of pre-emptions to allow in a single
  -- execution
  -> (forall t. ConcST t a)
  -- ^ The computation to run many times
  -> [(Either Failure a, Trace)]
sctPreBound memtype pb = sctBounded memtype (pbBound pb) pbBacktrack

-- | Variant of 'sctPreBound' for computations which do 'IO'.
sctPreBoundIO :: MemType -> PreemptionBound -> ConcIO a -> IO [(Either Failure a, Trace)]
sctPreBoundIO memtype pb = sctBoundedIO memtype (pbBound pb) pbBacktrack

-- | Pre-emption bound function
pbBound :: PreemptionBound -> BoundFunc
pbBound (PreemptionBound pb) ts dl = preEmpCount ts dl <= pb

-- | Count the number of pre-emptions in a schedule prefix.
preEmpCount :: [(Decision, ThreadAction)] -> (Decision, a) -> Int
preEmpCount ts (d, _) = go Nothing ts where
  go p ((d, a):rest) = preEmpC p d + go (Just a) rest
  go p [] = preEmpC p d

  preEmpC (Just Yield) (SwitchTo _) = 0
  preEmpC _ (SwitchTo t) = if t >= 0 then 1 else 0
  preEmpC _ _ = 0

-- | Count the number of pre-emptions in an entire trace
preEmpCount' :: Trace -> Int
preEmpCount' trc = preEmpCount (map (\(d,_,a) -> (d, a)) trc) (Continue, WillStop)

-- | Add a backtrack point, and also conservatively add one prior to
-- the most recent transition before that point. This may result in
-- the same state being reached multiple times, but is needed because
-- of the artificial dependency imposed by the bound.
pbBacktrack :: [BacktrackStep] -> Int -> ThreadId -> [BacktrackStep]
pbBacktrack bs i tid = maybe id (\j' b -> backtrack True b j' tid) j $ backtrack False bs i tid where
  -- Index of the conservative point
  j = goJ . reverse . pairs $ zip [0..i-1] bs where
    goJ (((_,b1), (j',b2)):rest)
      | _threadid b1 /= _threadid b2 && not (commit b1) && not (commit b2) = Just j'
      | otherwise = goJ rest
    goJ [] = Nothing

  {-# INLINE pairs #-}
  pairs = zip <*> tail

  commit b = case _decision b of
    (_, CommitRef _ _) -> True
    _ -> False

  backtrack = backtrackAt $ const False

-- * Fair bounding

newtype FairBound = FairBound Int
  deriving (NFData, Enum, Eq, Ord, Num, Real, Integral, Read, Show)

-- | A sensible default fair bound: 5
defaultFairBound :: FairBound
defaultFairBound = 5

-- | An SCT runner using a fair bounding scheduler.
sctFairBound :: MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> FairBound
  -- ^ The maximum difference between the number of yield operations
  -- performed by different threads.
  -> (forall t. ConcST t a)
  -- ^ The computation to run many times
  -> [(Either Failure a, Trace)]
sctFairBound memtype fb = sctBounded memtype (fBound fb) fBacktrack

-- | Variant of 'sctFairBound' for computations which do 'IO'.
sctFairBoundIO :: MemType -> FairBound -> ConcIO a -> IO [(Either Failure a, Trace)]
sctFairBoundIO memtype fb = sctBoundedIO memtype (fBound fb) fBacktrack

-- | Fair bound function
fBound :: FairBound -> BoundFunc
fBound (FairBound fb) ts dl = maxYieldCountDiff ts dl <= fb

-- | Count the number of yields by a thread in a schedule prefix.
yieldCount :: ThreadId -> [(Decision, ThreadAction)] -> (Decision, Lookahead) -> Int
yieldCount tid ts (_, l) = go 0 ts where
  go t ((Start    t', Yield):rest) = (if t == tid then 1 else 0) + go t' rest
  go t ((SwitchTo t', Yield):rest) = (if t == tid then 1 else 0) + go t' rest
  go t ((Continue,    Yield):rest) = (if t == tid then 1 else 0) + go t  rest
  go _ ((Start    t', _):rest) = go t' rest
  go _ ((SwitchTo t', _):rest) = go t' rest
  go t ((Continue,    _):rest) = go t  rest
  go t (_:rest) = go t rest
  go t [] = if l == WillYield && t == tid then 1 else 0

-- | Get the maximum difference between the yield counts of all
-- threads in this schedule prefix.
maxYieldCountDiff :: [(Decision, ThreadAction)] -> (Decision, Lookahead) -> Int
maxYieldCountDiff ts dl = maximum yieldCountDiffs where
  yieldCounts = [yieldCount tid ts dl | tid <- nub $ allTids ts]
  yieldCountDiffs = [y1 - y2 | y1 <- yieldCounts, y2 <- yieldCounts]

  allTids ((_, Fork tid):rest) = tid : allTids rest
  allTids (_:rest) = allTids rest
  allTids [] = [0]

-- | Add a backtrack point. If the thread isn't runnable, or performs
-- a release operation, add all runnable threads.
fBacktrack :: [BacktrackStep] -> Int -> ThreadId -> [BacktrackStep]
fBacktrack bs i t = backtrackAt check False bs i t where
  -- True if a release operation is performed.
  check b = Just True == (willRelease <$> M.lookup t (_runnable b))

-- * Length Bounding

newtype LengthBound = LengthBound Int
  deriving (NFData, Enum, Eq, Ord, Num, Real, Integral, Read, Show)

-- | A sensible default length bound: 250
defaultLengthBound :: LengthBound
defaultLengthBound = 250

-- | An SCT runner using a length bounding scheduler.
sctLengthBound :: MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> LengthBound
  -- ^ The maximum length of a schedule, in terms of primitive
  -- actions.
  -> (forall t. ConcST t a)
  -- ^ The computation to run many times
  -> [(Either Failure a, Trace)]
sctLengthBound memtype lb = sctBounded memtype (lBound lb) lBacktrack

-- | Variant of 'sctFairBound' for computations which do 'IO'.
sctLengthBoundIO :: MemType -> LengthBound -> ConcIO a -> IO [(Either Failure a, Trace)]
sctLengthBoundIO memtype lb = sctBoundedIO memtype (lBound lb) lBacktrack

-- | Length bound function
lBound :: LengthBound -> BoundFunc
lBound (LengthBound lb) ts _ = length ts < lb

-- | Add a backtrack point. If the thread isn't runnable, add all
-- runnable threads.
lBacktrack :: [BacktrackStep] -> Int -> ThreadId -> [BacktrackStep]
lBacktrack = backtrackAt (const False) False

-- * BPOR

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
  -> BoundFunc
  -- ^ Check if a prefix trace is within the bound
  -> ([BacktrackStep] -> Int -> ThreadId -> [BacktrackStep])
  -- ^ Add a new backtrack point, this takes the history of the
  -- execution so far, the index to insert the backtracking point, and
  -- the thread to backtrack to. This may insert more than one
  -- backtracking point.
  -> (forall t. ConcST t a) -> [(Either Failure a, Trace)]
sctBounded memtype bf backtrack c = runIdentity $ sctBoundedM memtype bf backtrack run where
  run memty sched s = Identity $ runConcST' sched memty s c

-- | Variant of 'sctBounded' for computations which do 'IO'.
sctBoundedIO :: MemType -> BoundFunc
  -> ([BacktrackStep] -> Int -> ThreadId -> [BacktrackStep])
  -> ConcIO a -> IO [(Either Failure a, Trace)]
sctBoundedIO memtype bf backtrack c = sctBoundedM memtype bf backtrack run where
  run memty sched s = runConcIO' sched memty s c

-- | Generic SCT runner.
sctBoundedM :: (Functor m, Monad m)
  => MemType
  -> ([(Decision, ThreadAction)] -> (Decision, Lookahead) -> Bool)
  -> ([BacktrackStep] -> Int -> ThreadId -> [BacktrackStep])
  -> (MemType -> Scheduler SchedState -> SchedState -> m (Either Failure a, SchedState, Trace'))
  -- ^ Monadic runner, with computation fixed.
  -> m [(Either Failure a, Trace)]
sctBoundedM memtype bf backtrack run = go initialState where
  go bpor = case next bpor of
    Just (sched, conservative, sleep) -> do
      (res, s, trace) <- run memtype (bporSched memtype $ initialise bf) (initialSchedState sleep sched)

      let bpoints = findBacktrack memtype backtrack (_sbpoints s) trace
      let newBPOR = grow memtype conservative trace bpor

      if _signore s
      then go newBPOR
      else ((res, toTrace trace):) <$> go (pruneCommits $ todo bf bpoints newBPOR)

    Nothing -> return []

-- * BPOR Scheduler

-- | The scheduler state
data SchedState = SchedState
  { _ssleep   :: Map ThreadId ThreadAction
  -- ^ The sleep set: decisions not to make until something dependent
  -- with them happens.
  , _sprefix  :: [ThreadId]
  -- ^ Decisions still to make
  , _sbpoints :: Seq (NonEmpty (ThreadId, Lookahead), [ThreadId])
  -- ^ Which threads are runnable at each step, and the alternative
  -- decisions still to make.
  , _signore  :: Bool
  -- ^ Whether to ignore this execution or not: @True@ if the
  -- execution is aborted due to all possible decisions being in the
  -- sleep set, as then everything in this execution is covered by
  -- another.
  } deriving Show

-- | Initial scheduler state for a given prefix
initialSchedState :: Map ThreadId ThreadAction -> [ThreadId] -> SchedState
initialSchedState sleep prefix = SchedState
  { _ssleep   = sleep
  , _sprefix  = prefix
  , _sbpoints = Sq.empty
  , _signore  = False
  }

-- | BPOR scheduler: takes a list of decisions, and maintains a trace
-- including the runnable threads, and the alternative choices allowed
-- by the bound-specific initialise function.
bporSched :: MemType
  -> ([(Decision, ThreadAction)] -> Maybe (ThreadId, ThreadAction) -> NonEmpty (ThreadId, Lookahead) -> [ThreadId])
  -> Scheduler SchedState
bporSched memtype init = force $ \s trc prior threads -> case _sprefix s of
  -- If there is a decision available, make it
  (d:ds) ->
    let threads' = fmap (\(t,a:|_) -> (t,a)) threads
    in  (Just d, s { _sprefix = ds, _sbpoints = _sbpoints s |> (threads', []) })

  -- Otherwise query the initialise function for a list of possible
  -- choices, filter out anything in the sleep set, and make one of
  -- them arbitrarily (recording the others).
  [] ->
    let threads' = fmap (\(t,a:|_) -> (t,a)) threads
        choices  = init trc prior threads'
        checkDep t a = case prior of
          Just (tid, act) -> dependent memtype unknownCRState (tid, act) (t, a)
          Nothing -> False
        ssleep'  = M.filterWithKey (\t a -> not $ checkDep t a) $ _ssleep s
        choices' = filter (`notElem` M.keys ssleep') choices
        signore' = not (null choices) && all (`elem` M.keys ssleep') choices
    in  case choices' of
          (nextTid:rest) -> (Just nextTid, s { _sbpoints = _sbpoints s |> (threads', rest), _ssleep = ssleep' })
          [] -> (Nothing, s { _sbpoints = _sbpoints s |> (threads', []), _signore = signore' })

-- | Pick a new thread to run, which does not exceed the bound. Choose
-- the current thread if available and it hasn't just yielded,
-- otherwise add all runnable threads.
initialise :: BoundFunc
  -> [(Decision, ThreadAction)]
  -> Maybe (ThreadId, ThreadAction)
  -> NonEmpty (ThreadId, Lookahead)
  -> [ThreadId]
initialise bf trc prior threads = restrictToBound . yieldsToEnd $ case prior of
  Just (_, Yield) -> map fst threads'
  Just (tid, _)
    | any (\(t, _) -> t == tid) threads' -> [tid]
  _ -> map fst threads'

  where
    -- Restrict the possible decisions to those in the bound.
    restrictToBound = fst . partition (\t -> bf trc (decision t, action t))

    -- Move the threads which will immediately yield to the end of the list
    yieldsToEnd ts = case partition ((== WillYield) . action) ts of
      (willYield, noYield) -> noYield ++ willYield

    -- Get the decision that will lead to a thread being scheduled.
    decision = decisionOf (fst <$> prior) (S.fromList $ map fst threads')

    -- Get the action of a thread
    action t = fromJust $ lookup t threads'

    -- The list of threads
    threads' = toList threads

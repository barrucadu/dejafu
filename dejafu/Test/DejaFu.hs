{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Test.DejaFu
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : RankNTypes
--
-- Deterministic testing for concurrent computations.
--
-- As an example, consider this program, which has two locks and a
-- shared variable. Two threads are spawned, which claim the locks,
-- update the shared variable, and release the locks. The main thread
-- waits for them both to terminate, and returns the final result.
--
-- > example1 :: MonadConc m => m Int
-- > example1 = do
-- >   a <- newEmptyMVar
-- >   b <- newEmptyMVar
-- >
-- >   c <- newMVar 0
-- >
-- >   let lock m = putMVar m ()
-- >   let unlock = takeMVar
-- >
-- >   j1 <- spawn $ lock a >> lock b >> modifyMVar_ c (return . succ) >> unlock b >> unlock a
-- >   j2 <- spawn $ lock b >> lock a >> modifyMVar_ c (return . pred) >> unlock a >> unlock b
-- >
-- >   takeMVar j1
-- >   takeMVar j2
-- >
-- >   takeMVar c
--
-- The correct result is 0, as it starts out as 0 and is incremented
-- and decremented by threads 1 and 2, respectively. However, note the
-- order of acquisition of the locks in the two threads. If thread 2
-- pre-empts thread 1 between the acquisition of the locks (or if
-- thread 1 pre-empts thread 2), a deadlock situation will arise, as
-- thread 1 will have lock @a@ and be waiting on @b@, and thread 2
-- will have @b@ and be waiting on @a@.
--
-- Here is what Deja Fu has to say about it:
--
-- > > autocheck example1
-- > [fail] Never Deadlocks (checked: 5)
-- >         [deadlock] S0------------S1-P2--S1-
-- > [pass] No Exceptions (checked: 12)
-- > [fail] Consistent Result (checked: 11)
-- >         0 S0------------S2-----------------S1-----------------S0----
-- >
-- >         [deadlock] S0------------S1-P2--S1-
-- > False
--
-- It identifies the deadlock, and also the possible results the
-- computation can produce, and displays a simplified trace leading to
-- each failing outcome. The trace contains thread numbers, and the
-- names (which can be set by the programmer) are displayed beneath.
-- It also returns @False@ as there are test failures. The automatic
-- testing functionality is good enough if you only want to check your
-- computation is deterministic, but if you have more specific
-- requirements (or have some expected and tolerated level of
-- nondeterminism), you can write tests yourself using the @dejafu*@
-- functions.
--
-- __Warning:__ If your computation under test does @IO@, the @IO@
-- will be executed lots of times! Be sure that it is deterministic
-- enough not to invalidate your test results. Mocking may be useful
-- where possible.
module Test.DejaFu
  ( -- * Testing

  -- | Testing in Deja Fu is similar to unit testing, the programmer
  -- produces a self-contained monadic action to execute under
  -- different schedules, and supplies a list of predicates to apply
  -- to the list of results produced.
  --
  -- If you simply wish to check that something is deterministic, see
  -- the 'autocheck' and 'autocheckIO' functions.
  --
  -- These functions use a Total Store Order (TSO) memory model for
  -- unsynchronised actions, see \"Testing under Alternative Memory
  -- Models\" for some explanation of this.

    autocheck
  , dejafu
  , dejafus
  , autocheckIO
  , dejafuIO
  , dejafusIO

  -- * Testing with different settings

  , autocheck'
  , dejafu'
  , dejafus'
  , autocheckIO'
  , dejafuIO'
  , dejafusIO'

  -- ** Memory Models

  -- | Threads running under modern multicore processors do not behave
  -- as a simple interleaving of the individual thread
  -- actions. Processors do all sorts of complex things to increase
  -- speed, such as buffering writes. For concurrent programs which
  -- make use of non-synchronised functions (such as 'readCRef'
  -- coupled with 'writeCRef') different memory models may yield
  -- different results.
  --
  -- As an example, consider this program (modified from the
  -- Data.IORef documentation). Two @CRef@s are created, and two
  -- threads spawned to write to and read from both. Each thread
  -- returns the value it observes.
  --
  -- > example2 :: MonadConc m => m (Bool, Bool)
  -- > example2 = do
  -- >   r1 <- newCRef False
  -- >   r2 <- newCRef False
  -- >
  -- >   x <- spawn $ writeCRef r1 True >> readCRef r2
  -- >   y <- spawn $ writeCRef r2 True >> readCRef r1
  -- >
  -- >   (,) <$> readMVar x <*> readMVar y
  --
  -- Under a sequentially consistent memory model the possible results
  -- are @(True, True)@, @(True, False)@, and @(False, True)@. Under
  -- total or partial store order, @(False, False)@ is also a possible
  -- result, even though there is no interleaving of the threads which
  -- can lead to this.
  --
  -- We can see this by testing with different memory models:
  --
  -- > > autocheck' SequentialConsistency example2
  -- > [pass] Never Deadlocks (checked: 6)
  -- > [pass] No Exceptions (checked: 6)
  -- > [fail] Consistent Result (checked: 5)
  -- >         (False,True) S0-------S1-----S0--S2-----S0---
  -- >         (True,False) S0-------S1-P2-----S1----S0----
  -- >         (True,True) S0-------S1--P2-----S1---S0----
  -- >         (False,True) S0-------S1---P2-----S1--S0----
  -- >         (True,False) S0-------S2-----S1-----S0----
  -- >         ...
  -- > False
  --
  -- > > autocheck' TotalStoreOrder example2
  -- > [pass] Never Deadlocks (checked: 303)
  -- > [pass] No Exceptions (checked: 303)
  -- > [fail] Consistent Result (checked: 302)
  -- >         (False,True) S0-------S1-----C-S0--S2-----C-S0---
  -- >         (True,False) S0-------S1-P2-----C-S1----S0----
  -- >         (True,True) S0-------S1-P2--C-S1----C-S0--S2---S0---
  -- >         (False,True) S0-------S1-P2--P1--C-C-S1--S0--S2---S0---
  -- >         (False,False) S0-------S1-P2--P1----S2---C-C-S0----
  -- >         ...
  -- > False
  --
  -- Traces for non-sequentially-consistent memory models show where
  -- writes to @CRef@s are /committed/, which makes a write visible to
  -- all threads rather than just the one which performed the
  -- write. Only 'writeCRef' is broken up into separate write and
  -- commit steps, 'atomicModifyCRef' is still atomic and imposes a
  -- memory barrier.

  , MemType(..)
  , defaultMemType

  -- ** Schedule Bounding

  -- | Schedule bounding is an optimisation which only considers
  -- schedules within some /bound/. This sacrifices completeness
  -- outside of the bound, but can drastically reduce the number of
  -- schedules to test, and is in fact necessary for non-terminating
  -- programs.
  --
  -- The standard testing mechanism uses a combination of pre-emption
  -- bounding, fair bounding, and length bounding. Pre-emption + fair
  -- bounding is useful for programs which use loop/yield control
  -- flows but are otherwise terminating. Length bounding makes it
  -- possible to test potentially non-terminating programs.

  , Bounds(..)
  , defaultBounds
  , noBounds
  , PreemptionBound(..)
  , defaultPreemptionBound
  , FairBound(..)
  , defaultFairBound
  , LengthBound(..)
  , defaultLengthBound

  -- * Unsystematic techniques

  -- | These algorithms do not promise to visit every unique schedule,
  -- or to avoid trying the same schedule twice. This greatly reduces
  -- the memory requirements in testing complex systems. Randomness
  -- drives everything. No partial-order reduction is done, but
  -- schedule bounds are still available.

  , unsystematicRandom
  , unsystematicPCT
  , unsystematicRandom'
  , unsystematicPCT'
  , unsystematicRandomIO
  , unsystematicPCTIO
  , unsystematicRandomIO'
  , unsystematicPCTIO'

  -- * Results

  -- | The results of a test can be pretty-printed to the console, as
  -- with the above functions, or used in their original, much richer,
  -- form for debugging purposes. These functions provide full access
  -- to this data type which, most usefully, contains a detailed trace
  -- of execution, showing what each thread did at each point.

  , Result(..)
  , Failure(..)
  , runTest
  , runTest'
  , runTestM
  , runTestM'

  -- * Predicates

  -- | Predicates evaluate a list of results of execution and decide
  -- whether some test case has passed or failed. They can be lazy and
  -- make use of short-circuit evaluation to avoid needing to examine
  -- the entire list of results, and can check any property which can
  -- be defined for the return type of your monadic action.
  --
  -- A collection of common predicates are provided, along with the
  -- helper functions 'alwaysTrue', 'alwaysTrue2' and 'somewhereTrue'
  -- to lfit predicates over a single result to over a collection of
  -- results.

  , Predicate
  , representative
  , abortsNever
  , abortsAlways
  , abortsSometimes
  , deadlocksNever
  , deadlocksAlways
  , deadlocksSometimes
  , exceptionsNever
  , exceptionsAlways
  , exceptionsSometimes
  , alwaysSame
  , notAlwaysSame
  , alwaysTrue
  , alwaysTrue2
  , somewhereTrue
  , gives
  , gives'
  ) where

import Control.Arrow (first)
import Control.DeepSeq (NFData(..))
import Control.Monad (when, unless)
import Control.Monad.Ref (MonadRef)
import Control.Monad.ST (runST)
import Data.Function (on)
import Data.List (intercalate, intersperse, minimumBy)
import Data.Ord (comparing)
import System.Random (RandomGen)

import Test.DejaFu.Common
import Test.DejaFu.Conc
import Test.DejaFu.SCT hiding (unsystematicRandom, unsystematicPCT)
import qualified Test.DejaFu.SCT as SCT

-- | The default memory model: @TotalStoreOrder@
defaultMemType :: MemType
defaultMemType = TotalStoreOrder

-- | Automatically test a computation. In particular, look for
-- deadlocks, uncaught exceptions, and multiple return values.
--
-- This uses the 'Conc' monad for testing, which is an instance of
-- 'MonadConc'. If you need to test something which also uses
-- 'MonadIO', use 'autocheckIO'.
autocheck :: (Eq a, Show a)
  => (forall t. ConcST t a)
  -- ^ The computation to test
  -> IO Bool
autocheck = autocheck' defaultMemType defaultBounds

-- | Variant of 'autocheck' which takes a memor model and schedule
-- bounds.
--
-- Schedule bounding is used to filter the large number of possible
-- schedules, and can be iteratively increased for further coverage
-- guarantees. Empirical studies (/Concurrency Testing Using Schedule
-- Bounding: an Empirical Study/, P. Thompson, A. Donaldson, and
-- A. Betts) have found that many concurrency bugs can be exhibited
-- with as few as two threads and two pre-emptions, which is part of
-- what 'dejafus' uses.
--
-- __Warning:__ Using largers bounds will almost certainly
-- significantly increase the time taken to test!
autocheck' :: (Eq a, Show a)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bounds
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> IO Bool
autocheck' memtype cb conc = dejafus' memtype cb conc autocheckCases

-- | Variant of 'autocheck' for computations which do 'IO'.
autocheckIO :: (Eq a, Show a) => ConcIO a -> IO Bool
autocheckIO = autocheckIO' defaultMemType defaultBounds

-- | Variant of 'autocheck'' for computations which do 'IO'.
autocheckIO' :: (Eq a, Show a) => MemType -> Bounds -> ConcIO a -> IO Bool
autocheckIO' memtype cb concio = dejafusIO' memtype cb concio autocheckCases

-- | Predicates for the various autocheck functions.
autocheckCases :: (Eq a, Show a) => [(String, Predicate a)]
autocheckCases =
  [ ("Never Deadlocks",   representative deadlocksNever)
  , ("No Exceptions",     representative exceptionsNever)
  , ("Consistent Result", alwaysSame) -- already representative
  ]

-- | Check a predicate and print the result to stdout, return 'True'
-- if it passes.
dejafu :: Show a
  => (forall t. ConcST t a)
  -- ^ The computation to test
  -> (String, Predicate a)
  -- ^ The predicate (with a name) to check
  -> IO Bool
dejafu = dejafu' defaultMemType defaultBounds

-- | Variant of 'dejafu'' which takes a memory model and schedule
-- bounds.
dejafu' :: Show a
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bounds
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> (String, Predicate a)
  -- ^ The predicate (with a name) to check
  -> IO Bool
dejafu' memtype cb conc test = dejafus' memtype cb conc [test]

-- | Variant of 'dejafu' which takes a collection of predicates to
-- test, returning 'True' if all pass.
dejafus :: Show a
  => (forall t. ConcST t a)
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> IO Bool
dejafus = dejafus' defaultMemType defaultBounds

-- | Variant of 'dejafus' which takes a memory model and schedule
-- bounds.
dejafus' :: Show a
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bounds.
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> IO Bool
dejafus' memtype cb conc tests =
  checkPredicates tests $ runST (sctBound memtype cb conc)

-- | Variant of 'dejafu' for computations which do 'IO'.
dejafuIO :: Show a => ConcIO a -> (String, Predicate a) -> IO Bool
dejafuIO = dejafuIO' defaultMemType defaultBounds

-- | Variant of 'dejafu'' for computations which do 'IO'.
dejafuIO' :: Show a => MemType -> Bounds -> ConcIO a -> (String, Predicate a) -> IO Bool
dejafuIO' memtype cb concio test = dejafusIO' memtype cb concio [test]

-- | Variant of 'dejafus' for computations which do 'IO'.
dejafusIO :: Show a => ConcIO a -> [(String, Predicate a)] -> IO Bool
dejafusIO = dejafusIO' defaultMemType defaultBounds

-- | Variant of 'dejafus'' for computations which do 'IO'.
dejafusIO' :: Show a => MemType -> Bounds -> ConcIO a -> [(String, Predicate a)] -> IO Bool
dejafusIO' memtype cb concio tests =
  checkPredicates tests =<< sctBound memtype cb concio

-------------------------------------------------------------------------------
-- Unsystematic techniques

-- | Try random schedules up to some limit.
unsystematicRandom :: (Show a, RandomGen g)
  => Int
  -- ^ The number of executions to try.
  -> g
  -- ^ The random number generator.
  -> (forall t. ConcST t a)
  -- ^ The computation to test.
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> IO Bool
unsystematicRandom = unsystematicRandom' defaultMemType defaultBounds

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
unsystematicPCT :: (Show a, RandomGen g)
  => Int
  -- ^ The number of executions to try.
  -> g
  -- ^ The random number generator.
  -> (forall t. ConcST t a)
  -- ^ The computation to test.
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> IO Bool
unsystematicPCT = unsystematicPCT' defaultMemType defaultBounds

-- | Variant of 'unsystematicRandom' that takes a memory model and
-- schedule bounds.
unsystematicRandom' :: (Show a, RandomGen g)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bounds.
  -> Int
  -- ^ The number of executions to try.
  -> g
  -- ^ The random number generator.
  -> (forall t. ConcST t a)
  -- ^ The computation to test.
  -> [(String, Predicate a)]
  -- ^ The predicate (with a name) to check
  -> IO Bool
unsystematicRandom' memtype cb lim gen conc tests =
  checkPredicates tests $ runST $ SCT.unsystematicRandom lim gen memtype (cBound cb) conc

-- | Variant of 'unsystematicPCT' that takes a memory model and
-- schedule bounds.
unsystematicPCT' :: (Show a, RandomGen g)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bounds.
  -> Int
  -- ^ The number of executions to try.
  -> g
  -- ^ The random number generator.
  -> (forall t. ConcST t a)
  -- ^ The computation to test.
  -> [(String, Predicate a)]
  -- ^ The predicate (with a name) to check
  -> IO Bool
unsystematicPCT' memtype cb lim gen conc tests =
  checkPredicates tests $ runST $ SCT.unsystematicPCT lim gen memtype (cBound cb) conc

-- | Variant of 'unsystematicRandom' for computations which do 'IO'.
unsystematicRandomIO :: (Show a, RandomGen g) => Int -> g -> ConcIO a -> [(String, Predicate a)] -> IO Bool
unsystematicRandomIO = unsystematicRandomIO' defaultMemType defaultBounds

-- | Variant of 'unsystematicPCT' for computations which do 'IO'.
unsystematicPCTIO :: (Show a, RandomGen g) => Int -> g -> ConcIO a -> [(String, Predicate a)] -> IO Bool
unsystematicPCTIO = unsystematicRandomIO' defaultMemType defaultBounds

-- | Variant of 'unsystematicRandom'' for computations which do 'IO'.
unsystematicRandomIO' :: (Show a, RandomGen g) => MemType -> Bounds -> Int -> g -> ConcIO a -> [(String, Predicate a)] -> IO Bool
unsystematicRandomIO' memtype cb lim gen conc tests =
  checkPredicates tests =<< SCT.unsystematicRandom lim gen memtype (cBound cb) conc

-- | Variant of 'unsystematicPCT'' for computations which do 'IO'.
unsystematicPCTIO' :: (Show a, RandomGen g) => MemType -> Bounds -> Int -> g -> ConcIO a -> [(String, Predicate a)] -> IO Bool
unsystematicPCTIO' memtype cb lim gen conc tests =
  checkPredicates tests =<< SCT.unsystematicPCT lim gen memtype (cBound cb) conc


-------------------------------------------------------------------------------
-- Test cases

-- | The results of a test, including the number of cases checked to
-- determine the final boolean outcome.
data Result a = Result
  { _pass         :: Bool
  -- ^ Whether the test passed or not.
  , _casesChecked :: Int
  -- ^ The number of cases checked.
  , _failures     :: [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
  -- ^ The failing cases, if any.
  , _failureMsg   :: String
  -- ^ A message to display on failure, if nonempty
  } deriving Show

-- | A failed result, taking the given list of failures.
defaultFail :: [(Either Failure a, Trace ThreadId ThreadAction Lookahead)] -> Result a
defaultFail failures = Result False 0 failures ""

-- | A passed result.
defaultPass :: Result a
defaultPass = Result True 0 [] ""

instance NFData a => NFData (Result a) where
  rnf r = rnf (_pass r, _casesChecked r, _failures r, _failureMsg r)

instance Functor Result where
  fmap f r = r { _failures = map (first $ fmap f) $ _failures r }

instance Foldable Result where
  foldMap f r = foldMap f [a | (Right a, _) <- _failures r]

-- | Run a predicate over all executions within the default schedule
-- bounds.
runTest ::
    Predicate a
  -- ^ The predicate to check
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> Result a
runTest test conc = runST (runTestM test conc)

-- | Variant of 'runTest' which takes a memory model and schedule
-- bounds.
runTest' ::
    MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bounds.
  -> Predicate a
  -- ^ The predicate to check
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> Result a
runTest' memtype cb predicate conc =
  runST (runTestM' memtype cb predicate conc)

-- | Monad-polymorphic variant of 'runTest'.
runTestM :: MonadRef r n
         => Predicate a -> Conc n r a -> n (Result a)
runTestM = runTestM' defaultMemType defaultBounds

-- | Monad-polymorphic variant of 'runTest''.
runTestM' :: MonadRef r n
          => MemType -> Bounds -> Predicate a -> Conc n r a -> n (Result a)
runTestM' memtype cb predicate conc = predicate <$> sctBound memtype cb conc


-------------------------------------------------------------------------------
-- Predicates

-- | A @Predicate@ is a function which collapses a list of results
-- into a 'Result'.
type Predicate a = [(Either Failure a, Trace ThreadId ThreadAction Lookahead)] -> Result a

-- | Reduce the list of failures in a @Predicate@ to one
-- representative trace for each unique result.
--
-- This may throw away \"duplicate\" failures which have a unique
-- cause but happen to manifest in the same way. However, it is
-- convenient for filtering out true duplicates.
representative :: Eq a => Predicate a -> Predicate a
representative p xs = result { _failures = choose . collect $ _failures result } where
  result  = p xs
  collect = groupBy' [] ((==) `on` fst)
  choose  = map $ minimumBy (comparing $ \(_, trc) -> (preEmps trc, length trc))

  preEmps trc = preEmpCount (map (\(d,_,a) -> (d, a)) trc) (Continue, WillStop)

  groupBy' res _ [] = res
  groupBy' res eq (y:ys) = groupBy' (insert' eq y res) eq ys

  insert' _ x [] = [[x]]
  insert' eq x (ys@(y:_):yss)
    | x `eq` y  = (x:ys) : yss
    | otherwise = ys : insert' eq x yss
  insert' _ _ ([]:_) = undefined

-- | Check that a computation never aborts.
abortsNever :: Predicate a
abortsNever = alwaysTrue (not . either (==Abort) (const False))

-- | Check that a computation always aborts.
abortsAlways :: Predicate a
abortsAlways = alwaysTrue $ either (==Abort) (const False)

-- | Check that a computation aborts at least once.
abortsSometimes :: Predicate a
abortsSometimes = somewhereTrue $ either (==Abort) (const False)

-- | Check that a computation never deadlocks.
deadlocksNever :: Predicate a
deadlocksNever = alwaysTrue (not . either (`elem` [Deadlock, STMDeadlock]) (const False))

-- | Check that a computation always deadlocks.
deadlocksAlways :: Predicate a
deadlocksAlways = alwaysTrue $ either (`elem` [Deadlock, STMDeadlock]) (const False)

-- | Check that a computation deadlocks at least once.
deadlocksSometimes :: Predicate a
deadlocksSometimes = somewhereTrue $ either (`elem` [Deadlock, STMDeadlock]) (const False)

-- | Check that a computation never fails with an uncaught exception.
exceptionsNever :: Predicate a
exceptionsNever = alwaysTrue (not . either (==UncaughtException) (const False))

-- | Check that a computation always fails with an uncaught exception.
exceptionsAlways :: Predicate a
exceptionsAlways = alwaysTrue $ either (==UncaughtException) (const False)

-- | Check that a computation fails with an uncaught exception at least once.
exceptionsSometimes :: Predicate a
exceptionsSometimes = somewhereTrue $ either (==UncaughtException) (const False)

-- | Check that the result of a computation is always the same. In
-- particular this means either: (a) it always fails in the same way,
-- or (b) it never fails and the values returned are all equal.
alwaysSame :: Eq a => Predicate a
alwaysSame = representative $ alwaysTrue2 (==)

-- | Check that the result of a computation is not always the same.
notAlwaysSame :: Eq a => Predicate a
notAlwaysSame [x] = (defaultFail [x]) { _casesChecked = 1 }
notAlwaysSame xs = go xs $ defaultFail [] where
  go [y1,y2] res
    | fst y1 /= fst y2 = incCC res { _pass = True }
    | otherwise = incCC res { _failures = y1 : y2 : _failures res }
  go (y1:y2:ys) res
    | fst y1 /= fst y2 = go (y2:ys) . incCC $ res { _pass = True }
    | otherwise = go (y2:ys) . incCC $ res { _failures = y1 : y2 : _failures res }
  go _ res = res

-- | Check that the result of a unary boolean predicate is always
-- true.
alwaysTrue :: (Either Failure a -> Bool) -> Predicate a
alwaysTrue p xs = go xs $ (defaultFail failures) { _pass = True } where
  go (y:ys) res
    | p (fst y) = go ys . incCC $ res
    | otherwise = incCC $ res { _pass = False }
  go [] res = res

  failures = filter (not . p . fst) xs

-- | Check that the result of a binary boolean predicate is true
-- between all pairs of results. Only properties which are transitive
-- and symmetric should be used here.
--
-- If the predicate fails, /both/ (result,trace) tuples will be added
-- to the failures list.
alwaysTrue2 :: (Either Failure a -> Either Failure a -> Bool) -> Predicate a
alwaysTrue2 _ [_] = defaultPass { _casesChecked = 1 }
alwaysTrue2 p xs = go xs $ defaultPass { _failures = failures } where
  go [y1,y2] res
    | p (fst y1) (fst y2) = incCC res
    | otherwise = incCC res { _pass = False }
  go (y1:y2:ys) res
    | p (fst y1) (fst y2) = go (y2:ys) . incCC $ res
    | otherwise = go (y2:ys) . incCC $ res { _pass = False }
  go _ res = res

  failures = fgo xs where
    fgo (y1:y2:ys)
      | p (fst y1) (fst y2) = fgo (y2:ys)
      | otherwise = y1 : y2 : fgo2 y2 ys
    fgo _ = []

    fgo2 y1 (y2:ys)
      | p (fst y1) (fst y2) = fgo (y2:ys)
      | otherwise = y2 : fgo2 y2 ys
    fgo2 _ _ = []

-- | Check that the result of a unary boolean predicate is true at
-- least once.
somewhereTrue :: (Either Failure a -> Bool) -> Predicate a
somewhereTrue p xs = go xs $ defaultFail failures where
  go (y:ys) res
    | p (fst y) = incCC $ res { _pass = True }
    | otherwise = go ys . incCC $ res { _failures = y : _failures res }
  go [] res = res

  failures = filter (not . p . fst) xs

-- | Predicate for when there is a known set of results where every
-- result must be exhibited at least once.
gives :: (Eq a, Show a) => [Either Failure a] -> Predicate a
gives expected results = go expected [] results $ defaultFail failures where
  go waitingFor alreadySeen ((x, _):xs) res
    -- If it's a result we're waiting for, move it to the
    -- @alreadySeen@ list and continue.
    | x `elem` waitingFor  = go (filter (/=x) waitingFor) (x:alreadySeen) xs res { _casesChecked = _casesChecked res + 1 }

    -- If it's a result we've already seen, continue.
    | x `elem` alreadySeen = go waitingFor alreadySeen xs res { _casesChecked = _casesChecked res + 1 }

    -- If it's not a result we expected, fail.
    | otherwise = res { _casesChecked = _casesChecked res + 1 }

  go [] _ [] res = res { _pass = True }
  go es _ [] res = res { _failureMsg = unlines $ map (\e -> "Expected: " ++ show e) es }

  failures = filter (\(r, _) -> r `notElem` expected) results

-- | Variant of 'gives' that doesn't allow for expected failures.
gives' :: (Eq a, Show a) => [a] -> Predicate a
gives' = gives . map Right


-------------------------------------------------------------------------------
-- Internal

-- | Check predicates against a collection of traces and print the
-- output.
checkPredicates :: Show a
  => [(String, Predicate a)]
  -> [(Either Failure a, Trace ThreadId ThreadAction Lookahead)]
  -> IO Bool
checkPredicates tests traces =
  and <$> mapM (\(name, test) -> doTest name $ test traces) tests

-- | Run a test and print to stdout
doTest :: Show a => String -> Result a -> IO Bool
doTest name result = do
  if _pass result
  then
    -- Display a pass message.
    putStrLn $ "\27[32m[pass]\27[0m " ++ name ++ " (checked: " ++ show (_casesChecked result) ++ ")"
  else do
    -- Display a failure message, and the first 5 (simplified) failed traces
    putStrLn ("\27[31m[fail]\27[0m " ++ name ++ " (checked: " ++ show (_casesChecked result) ++ ")")

    unless (null $ _failureMsg result) $
      putStrLn $ _failureMsg result

    let failures = _failures result
    let output = map (\(r, t) -> putStrLn . indent $ either showFail show r ++ " " ++ showTrace t) $ take 5 failures
    sequence_ $ intersperse (putStrLn "") output
    when (moreThan 5 failures) $
      putStrLn (indent "...")

  return $ _pass result

-- | Check if a list is longer than some value, without needing to
-- compute the entire length.
moreThan :: Int -> [a] -> Bool
moreThan n [] = n < 0
moreThan 0 _ = True
moreThan n (_:rest) = moreThan (n-1) rest

-- | Increment the cases
incCC :: Result a -> Result a
incCC r = r { _casesChecked = _casesChecked r + 1 }

-- | Indent every line of a string.
indent :: String -> String
indent = intercalate "\n" . map ('\t':) . lines

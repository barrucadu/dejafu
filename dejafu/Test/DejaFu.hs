{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | Deterministic testing for concurrent computations.
--
-- As an example, consider this program, which has two locks and a
-- shared variable. Two threads are spawned, which claim the locks,
-- update the shared variable, and release the locks. The main thread
-- waits for them both to terminate, and returns the final result.
--
-- > example1 :: MonadConc m => m Int
-- > example1 = do
-- >   a <- newEmptyCVar
-- >   b <- newEmptyCVar
-- >
-- >   c <- newCVar 0
-- >
-- >   j1 <- spawn $ lock a >> lock b >> modifyCVar_ c (return . succ) >> unlock b >> unlock a
-- >   j2 <- spawn $ lock b >> lock a >> modifyCVar_ c (return . pred) >> unlock a >> unlock b
-- >
-- >   takeCVar j1
-- >   takeCVar j2
-- >
-- >   takeCVar c
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
-- > [fail] Never Deadlocks (checked: 2)
-- >         [deadlock] S0---------S1--P2---S1-
-- > [pass] No Exceptions (checked: 11)
-- > [fail] Consistent Result (checked: 10)
-- >         0 S0---------S1---------------S0--S2---------------S0----
-- >         [deadlock] S0---------S1--P2---S1-
-- > False
--
-- It identifies the deadlock, and also the possible results the
-- computation can produce, and displays a simplified trace leading to
-- each failing outcome. It also returns @False@ as there are test
-- failures. The automatic testing functionality is good enough if you
-- only want to check your computation is deterministic, but if you
-- have more specific requirements (or have some expected and
-- tolerated level of nondeterminism), you can write tests yourself
-- using the @dejafu*@ functions.
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

  -- * Testing under Alternative Memory Models

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
  -- >   (,) <$> readCVar x <*> readCVar y
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
  -- commit steps, 'modifyCRef' is still atomic and imposes a memory
  -- barrier.

  , MemType(..)
  , autocheck'
  , autocheckIO'
  , dejafu'
  , dejafus'
  , dejafuIO'
  , dejafusIO'

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
  , runTestIO
  , runTestIO'

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
  , alwaysTrueNub
  , alwaysTrue2Nub
  , somewhereTrueNub
  , gives
  , gives'
  ) where

import Control.Arrow (first)
import Control.DeepSeq (NFData(..))
import Control.Monad (when, unless)
import Data.Function (on)
import Data.List (minimumBy)
import Data.List.Extra
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Test.DejaFu.Deterministic
import Test.DejaFu.SCT

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
import Data.Foldable (Foldable(..))
#endif

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
autocheck = autocheck' TotalStoreOrder

-- | Variant of 'autocheck' which tests a computation under a given
-- memory model.
autocheck' :: (Eq a, Show a)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> IO Bool
autocheck' memtype conc = dejafus' memtype 2 5 conc autocheckCases

-- | Variant of 'autocheck' for computations which do 'IO'.
autocheckIO :: (Eq a, Show a) => ConcIO a -> IO Bool
autocheckIO = autocheckIO' TotalStoreOrder

-- | Variant of 'autocheck'' for computations which do 'IO'.
autocheckIO' :: (Eq a, Show a) => MemType -> ConcIO a -> IO Bool
autocheckIO' memtype concio = dejafusIO' memtype 2 5 concio autocheckCases

-- | Predicates for the various autocheck functions.
autocheckCases :: (Eq a, Show a) => [(String, Predicate a)]
autocheckCases =
  [ ("Never Deadlocks",   deadlocksNever)
  , ("No Exceptions",     exceptionsNever)
  , ("Consistent Result", alwaysSame)
  ]

-- | Check a predicate and print the result to stdout, return 'True'
-- if it passes.
dejafu :: Show a
  => (forall t. ConcST t a)
  -- ^ The computation to test
  -> (String, Predicate a)
  -- ^ The predicate (with a name) to check
  -> IO Bool
dejafu = dejafu' TotalStoreOrder 2 5

-- | Variant of 'dejafu'' which takes a memory model and pre-emption
-- bound.
--
-- Pre-emption bounding is used to filter the large number of possible
-- schedules, and can be iteratively increased for further coverage
-- guarantees. Empirical studies (/Concurrency Testing Using Schedule Bounding: an Empirical Study/,
-- P. Thompson, A. Donaldson, and A. Betts) have found that many
-- concurrency bugs can be exhibited with as few as two threads and
-- two pre-emptions, which is what 'dejafus' uses.
--
-- __Warning:__ Using a larger pre-emption bound will almost certainly
-- significantly increase the time taken to test!
dejafu' :: Show a
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Int
  -- ^ The maximum number of pre-emptions to allow in a single
  -- execution
  -> Int
  -- ^ The maximum difference between the number of yield operations
  -- across all threads.
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> (String, Predicate a)
  -- ^ The predicate (with a name) to check
  -> IO Bool
dejafu' memtype pb fb conc test = dejafus' memtype pb fb conc [test]

-- | Variant of 'dejafu' which takes a collection of predicates to
-- test, returning 'True' if all pass.
dejafus :: Show a
  => (forall t. ConcST t a)
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> IO Bool
dejafus = dejafus' TotalStoreOrder 2 5

-- | Variant of 'dejafus' which takes a memory model and pre-emption
-- bound.
dejafus' :: Show a
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Int
  -- ^ The maximum number of pre-emptions to allow in a single
  -- execution
  -> Int
  -- ^ The maximum difference between the number of yield operations
  -- across all threads.
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> IO Bool
dejafus' memtype pb fb conc tests = do
  let traces = sctPFBound memtype pb fb conc
  results <- mapM (\(name, test) -> doTest name $ test traces) tests
  return $ and results

-- | Variant of 'dejafu' for computations which do 'IO'.
dejafuIO :: Show a => ConcIO a -> (String, Predicate a) -> IO Bool
dejafuIO = dejafuIO' TotalStoreOrder 2 5

-- | Variant of 'dejafu'' for computations which do 'IO'.
dejafuIO' :: Show a => MemType -> Int -> Int -> ConcIO a -> (String, Predicate a) -> IO Bool
dejafuIO' memtype pb fb concio test = dejafusIO' memtype pb fb concio [test]

-- | Variant of 'dejafus' for computations which do 'IO'.
dejafusIO :: Show a => ConcIO a -> [(String, Predicate a)] -> IO Bool
dejafusIO = dejafusIO' TotalStoreOrder 2 5

-- | Variant of 'dejafus'' for computations which do 'IO'.
dejafusIO' :: Show a => MemType -> Int -> Int -> ConcIO a -> [(String, Predicate a)] -> IO Bool
dejafusIO' memtype pb fb concio tests = do
  traces  <- sctPFBoundIO memtype pb fb concio
  results <- mapM (\(name, test) -> doTest name $ test traces) tests
  return $ and results

-- * Test cases

-- | The results of a test, including the number of cases checked to
-- determine the final boolean outcome.
data Result a = Result
  { _pass         :: Bool
  -- ^ Whether the test passed or not.
  , _casesChecked :: Int
  -- ^ The number of cases checked.
  , _failures     :: [(Either Failure a, Trace)]
  -- ^ The failing cases, if any.
  , _failureMsg   :: String
  -- ^ A message to display on failure, if nonempty
  } deriving (Show, Eq)

-- | A failed result, taking the given list of failures.
defaultFail :: [(Either Failure a, Trace)] -> Result a
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

-- | Run a predicate over all executions with two or fewer
-- pre-emptions.
runTest ::
    Predicate a
  -- ^ The predicate to check
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> Result a
runTest = runTest' TotalStoreOrder 2 5

-- | Variant of 'runTest' which takes a memory model and pre-emption
-- bound.
runTest' ::
    MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Int
  -- ^ The maximum number of pre-emptions to allow in a single
  -- execution
  -> Int
  -- ^ The maximum difference between the number of yield operations
  -- across all threads.
  -> Predicate a
  -- ^ The predicate to check
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> Result a
runTest' memtype pb fb predicate conc = predicate $ sctPFBound memtype pb fb conc

-- | Variant of 'runTest' for computations which do 'IO'.
runTestIO :: Predicate a -> ConcIO a -> IO (Result a)
runTestIO = runTestIO' TotalStoreOrder 2 5

-- | Variant of 'runTest'' for computations which do 'IO'.
runTestIO' :: MemType -> Int -> Int -> Predicate a -> ConcIO a -> IO (Result a)
runTestIO' memtype pb fb predicate conc = predicate <$> sctPFBoundIO memtype pb fb conc

-- * Predicates

-- | A @Predicate@ is a function which collapses a list of results
-- into a 'Result'.
type Predicate a = [(Either Failure a, Trace)] -> Result a

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
  choose  = map $ minimumBy (comparing $ \(_, trc) -> (preEmpCount' trc, length trc))

  groupBy' res _ [] = res
  groupBy' res eq (x:xs) = groupBy' (insert' eq x res) eq xs

  insert' eq x [] = [[x]]
  insert' eq x (ys@(y:_):yss)
    | x `eq` y  = (x:ys) : yss
    | otherwise = ys : insert' eq x yss

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
alwaysSame = alwaysTrue2Nub (==)

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

-- | Variant of 'alwaysTrue' which only shows one trace per failure
-- with the same result. Be aware that this may throw away failures
-- with distinct causes.
alwaysTrueNub :: Eq a => (Either Failure a -> Bool) -> Predicate a
alwaysTrueNub p = representative (alwaysTrue p)

-- | Variant of 'alwaysTrue2' which only shows one trace per failure
-- with the same result. Be aware that this may throw away failures
-- with distinct causes.
alwaysTrue2Nub :: Eq a => (Either Failure a -> Either Failure a -> Bool) -> Predicate a
alwaysTrue2Nub p = representative (alwaysTrue2 p)

-- | Variant of 'somewhereTrue' which only shows one trace per failure
-- with the same result. Be aware that this may throw away failures
-- with distinct causes.
somewhereTrueNub :: Eq a => (Either Failure a -> Bool) -> Predicate a
somewhereTrueNub p = representative (somewhereTrue p)

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

-- * Internal

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
    mapM_ (\(r, t) -> putStrLn $ "\t" ++ either showFail show r ++ " " ++ showTrace t) $ take 5 failures
    when (moreThan failures 5) $
      putStrLn "\t..."

  return $ _pass result

-- | Increment the cases
incCC :: Result a -> Result a
incCC r = r { _casesChecked = _casesChecked r + 1 }

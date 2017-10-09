{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Test.DejaFu
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : MultiParamTypeClasses
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
  -- the 'autocheck' function.
  --
  -- These functions use a Total Store Order (TSO) memory model for
  -- unsynchronised actions, see \"Testing under Alternative Memory
  -- Models\" for some explanation of this.

    autocheck
  , dejafu
  , dejafus

  -- * Testing with different settings

  , Way
  , defaultWay
  , systematically
  , randomly
  , uniformly
  , swarmy

  , autocheckWay
  , dejafuWay
  , dejafusWay

  , Discard(..)
  , defaultDiscarder

  , dejafuDiscard

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
  -- > > autocheckWay defaultWay SequentialConsistency example2
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
  -- > > autocheckWay defaultWay TotalStoreOrder example2
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

  -- * Results

  -- | The results of a test can be pretty-printed to the console, as
  -- with the above functions, or used in their original, much richer,
  -- form for debugging purposes. These functions provide full access
  -- to this data type which, most usefully, contains a detailed trace
  -- of execution, showing what each thread did at each point.

  , Result(..)
  , Failure(..)
  , runTest
  , runTestWay

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

  -- ** Failures

  , isInternalError
  , isAbort
  , isDeadlock
  , isUncaughtException
  , isIllegalSubconcurrency

  -- * Refinement property testing

  -- | Consider this statement about @MVar@s: \"using @readMVar@ is
  -- better than @takeMVar@ followed by @putMVar@ because the former
  -- is atomic but the latter is not.\"
  --
  -- Deja Fu can test properties like that:
  --
  -- @
  -- sig e = Sig
  --   { initialise = maybe newEmptyMVar newMVar
  --   , observe    = \\v _ -> tryReadMVar v
  --   , interfere  = \\v s -> tryTakeMVar v >> maybe (pure ()) (void . tryPutMVar v) s
  --   , expression = e
  --   }
  --
  -- > check $ sig (void . readMVar) \`equivalentTo\` sig (\\v -> takeMVar v >>= putMVar v)
  -- *** Failure: (seed Just ())
  --     left:  [(Nothing,Just ())]
  --     right: [(Nothing,Just ()),(Just Deadlock,Just ())]
  -- @
  --
  -- The two expressions are not equivalent, and we get given the
  -- counterexample!
  , module Test.DejaFu.Refinement
  ) where

import           Control.Arrow            (first)
import           Control.DeepSeq          (NFData(..))
import           Control.Monad            (unless, when)
import           Control.Monad.Conc.Class (MonadConc)
import           Control.Monad.IO.Class   (MonadIO(..))
import           Control.Monad.Ref        (MonadRef)
import           Data.Function            (on)
import           Data.List                (intercalate, intersperse, minimumBy)
import           Data.Ord                 (comparing)

import           Test.DejaFu.Common
import           Test.DejaFu.Conc
import           Test.DejaFu.Defaults
import           Test.DejaFu.Refinement
import           Test.DejaFu.SCT


-------------------------------------------------------------------------------
-- DejaFu

-- | Automatically test a computation. In particular, look for
-- deadlocks, uncaught exceptions, and multiple return values.
--
-- @since 1.0.0.0
autocheck :: (MonadConc n, MonadIO n, MonadRef r n, Eq a, Show a)
  => ConcT r n a
  -- ^ The computation to test
  -> n Bool
autocheck = autocheckWay defaultWay defaultMemType

-- | Variant of 'autocheck' which takes a way to run the program and a
-- memory model.
--
-- Schedule bounding is used to filter the large number of possible
-- schedules, and can be iteratively increased for further coverage
-- guarantees. Empirical studies (/Concurrency Testing Using Schedule
-- Bounding: an Empirical Study/, P. Thompson, A. Donaldson, and
-- A. Betts) have found that many concurrency bugs can be exhibited
-- with as few as two threads and two pre-emptions, which is part of
-- what 'dejafus' uses.
--
-- __Warning:__ Using larger bounds will almost certainly
-- significantly increase the time taken to test!
--
-- @since 1.0.0.0
autocheckWay :: (MonadConc n, MonadIO n, MonadRef r n, Eq a, Show a)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> ConcT r n a
  -- ^ The computation to test
  -> n Bool
autocheckWay way memtype conc =
  dejafusWay way memtype conc autocheckCases

-- | Predicates for the various autocheck functions.
autocheckCases :: Eq a => [(String, Predicate a)]
autocheckCases =
  [ ("Never Deadlocks",   representative deadlocksNever)
  , ("No Exceptions",     representative exceptionsNever)
  , ("Consistent Result", alwaysSame) -- already representative
  ]

-- | Check a predicate and print the result to stdout, return 'True'
-- if it passes.
--
-- @since 1.0.0.0
dejafu :: (MonadConc n, MonadIO n, MonadRef r n, Show a)
  => ConcT r n a
  -- ^ The computation to test
  -> (String, Predicate a)
  -- ^ The predicate (with a name) to check
  -> n Bool
dejafu = dejafuWay defaultWay defaultMemType

-- | Variant of 'dejafu' which takes a way to run the program and a
-- memory model.
--
-- @since 1.0.0.0
dejafuWay :: (MonadConc n, MonadIO n, MonadRef r n, Show a)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> ConcT r n a
  -- ^ The computation to test
  -> (String, Predicate a)
  -- ^ The predicate (with a name) to check
  -> n Bool
dejafuWay = dejafuDiscard (const Nothing)

-- | Variant of 'dejafuWay' which can selectively discard results.
--
-- @since 1.0.0.0
dejafuDiscard :: (MonadConc n, MonadIO n, MonadRef r n, Show a)
  => (Either Failure a -> Maybe Discard)
  -- ^ Selectively discard results.
  -> Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> ConcT r n a
  -- ^ The computation to test
  -> (String, Predicate a)
  -- ^ The predicate (with a name) to check
  -> n Bool
dejafuDiscard discard way memtype conc (name, test) = do
  traces <- runSCTDiscard discard way memtype conc
  liftIO $ doTest name (test traces)

-- | Variant of 'dejafu' which takes a collection of predicates to
-- test, returning 'True' if all pass.
--
-- @since 1.0.0.0
dejafus :: (MonadConc n, MonadIO n, MonadRef r n, Show a)
  => ConcT r n a
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> n Bool
dejafus = dejafusWay defaultWay defaultMemType

-- | Variant of 'dejafus' which takes a way to run the program and a
-- memory model.
--
-- @since 1.0.0.0
dejafusWay :: (MonadConc n, MonadIO n, MonadRef r n, Show a)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> ConcT r n a
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> n Bool
dejafusWay way memtype conc tests = do
  traces <- runSCT way memtype conc
  results <- mapM (\(name, test) -> liftIO . doTest name $ test traces) tests
  pure (and results)


-------------------------------------------------------------------------------
-- Test cases

-- | The results of a test, including the number of cases checked to
-- determine the final boolean outcome.
--
-- @since 0.2.0.0
data Result a = Result
  { _pass         :: Bool
  -- ^ Whether the test passed or not.
  , _casesChecked :: Int
  -- ^ The number of cases checked.
  , _failures     :: [(Either Failure a, Trace)]
  -- ^ The failing cases, if any.
  , _failureMsg   :: String
  -- ^ A message to display on failure, if nonempty
  } deriving (Eq, Show)

-- | @since 0.5.1.0
instance NFData a => NFData (Result a) where
  rnf r = rnf ( _pass         r
              , _casesChecked r
              , _failures     r
              , _failureMsg   r
              )

-- | A failed result, taking the given list of failures.
defaultFail :: [(Either Failure a, Trace)] -> Result a
defaultFail failures = Result False 0 failures ""

-- | A passed result.
defaultPass :: Result a
defaultPass = Result True 0 [] ""

instance Functor Result where
  fmap f r = r { _failures = map (first $ fmap f) $ _failures r }

instance Foldable Result where
  foldMap f r = foldMap f [a | (Right a, _) <- _failures r]

-- | Run a predicate over all executions within the default schedule
-- bounds.
--
-- @since 1.0.0.0
runTest :: (MonadConc n, MonadRef r n)
  => Predicate a
  -- ^ The predicate to check
  -> ConcT r n a
  -- ^ The computation to test
  -> n (Result a)
runTest = runTestWay defaultWay defaultMemType

-- | Variant of 'runTest' which takes a way to run the program and a
-- memory model.
--
-- @since 1.0.0.0
runTestWay :: (MonadConc n, MonadRef r n)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Predicate a
  -- ^ The predicate to check
  -> ConcT r n a
  -- ^ The computation to test
  -> n (Result a)
runTestWay way memtype predicate conc =
  predicate <$> runSCT way memtype conc


-------------------------------------------------------------------------------
-- Predicates

-- | A @Predicate@ is a function which collapses a list of results
-- into a 'Result'.
--
-- @since 0.1.0.0
type Predicate a = [(Either Failure a, Trace)] -> Result a

-- | Reduce the list of failures in a @Predicate@ to one
-- representative trace for each unique result.
--
-- This may throw away \"duplicate\" failures which have a unique
-- cause but happen to manifest in the same way. However, it is
-- convenient for filtering out true duplicates.
--
-- @since 0.2.0.0
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
--
-- @since 0.2.0.0
abortsNever :: Predicate a
abortsNever = alwaysTrue (not . either (==Abort) (const False))

-- | Check that a computation always aborts.
--
-- @since 0.2.0.0
abortsAlways :: Predicate a
abortsAlways = alwaysTrue $ either (==Abort) (const False)

-- | Check that a computation aborts at least once.
--
-- @since 0.2.0.0
abortsSometimes :: Predicate a
abortsSometimes = somewhereTrue $ either (==Abort) (const False)

-- | Check that a computation never deadlocks.
--
-- @since 0.1.0.0
deadlocksNever :: Predicate a
deadlocksNever = alwaysTrue (not . either isDeadlock (const False))

-- | Check that a computation always deadlocks.
--
-- @since 0.1.0.0
deadlocksAlways :: Predicate a
deadlocksAlways = alwaysTrue $ either isDeadlock (const False)

-- | Check that a computation deadlocks at least once.
--
-- @since 0.1.0.0
deadlocksSometimes :: Predicate a
deadlocksSometimes = somewhereTrue $ either isDeadlock (const False)

-- | Check that a computation never fails with an uncaught exception.
--
-- @since 0.1.0.0
exceptionsNever :: Predicate a
exceptionsNever = alwaysTrue (not . either isUncaughtException (const False))

-- | Check that a computation always fails with an uncaught exception.
--
-- @since 0.1.0.0
exceptionsAlways :: Predicate a
exceptionsAlways = alwaysTrue $ either isUncaughtException (const False)

-- | Check that a computation fails with an uncaught exception at least once.
--
-- @since 0.1.0.0
exceptionsSometimes :: Predicate a
exceptionsSometimes = somewhereTrue $ either isUncaughtException (const False)

-- | Check that the result of a computation is always the same. In
-- particular this means either: (a) it always fails in the same way,
-- or (b) it never fails and the values returned are all equal.
--
-- @since 0.1.0.0
alwaysSame :: Eq a => Predicate a
alwaysSame = representative $ alwaysTrue2 (==)

-- | Check that the result of a computation is not always the same.
--
-- @since 0.1.0.0
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
--
-- @since 0.1.0.0
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
--
-- @since 0.1.0.0
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
--
-- @since 0.1.0.0
somewhereTrue :: (Either Failure a -> Bool) -> Predicate a
somewhereTrue p xs = go xs $ defaultFail failures where
  go (y:ys) res
    | p (fst y) = incCC $ res { _pass = True }
    | otherwise = go ys . incCC $ res { _failures = y : _failures res }
  go [] res = res

  failures = filter (not . p . fst) xs

-- | Predicate for when there is a known set of results where every
-- result must be exhibited at least once.
--
-- @since 0.2.0.0
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
--
-- @since 0.2.0.0
gives' :: (Eq a, Show a) => [a] -> Predicate a
gives' = gives . map Right


-------------------------------------------------------------------------------
-- Utils

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

  pure (_pass result)

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

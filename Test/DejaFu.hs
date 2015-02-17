{-# LANGUAGE Rank2Types #-}

-- | Deterministic testing for concurrent computations.
--
-- As an example, consider this program, which has two locks and a
-- shared variable. Two threads are spawned, which claim the locks,
-- update the shared variable, and release the locks. The main thread
-- waits for them both to terminate, and returns the final result.
--
-- > bad :: MonadConc m => m Int
-- > bad = do
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
-- Here is what @dejafu@ has to say about it:
--
-- > > autocheck bad
-- > [fail] Never Deadlocks (checked: 4)
-- >         [deadlock] S0---------S1-P2--S1-
-- >         [deadlock] S0---------S2-P1--S2-
-- > [fail] Consistent Result (checked: 3)
-- >         [deadlock] S0---------S1-P2--S1-
-- >         0 S0---------S1--------S2--------S0-----
-- >         [deadlock] S0---------S2-P1--S2-
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
-- enough not to invalidate your test results.
module Test.DejaFu
  ( autocheck
  , dejafu
  , dejafus
  , autocheckIO
  , dejafuIO
  , dejafusIO
  -- * Test cases
  , Result(..)
  , Failure(..)
  , runTest
  , runTest'
  , runTestIO
  , runTestIO'
  -- * Predicates
  , Predicate
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
  , somewhereTrue2
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Data.List (nub)
import Data.List.Extra
import Data.Monoid (mconcat)
import Test.DejaFu.Deterministic
import Test.DejaFu.Deterministic.IO (ConcIO)
import Test.DejaFu.SCT

-- | Run a test and print the result to stdout, return 'True' if it
-- passes.
dejafu :: (Eq a, Show a)
       => (forall t. Conc t a)
       -- ^ The computation to test
       -> (String, Predicate a)
       -- ^ The test case, as a (name, predicate) pair.
       -> IO Bool
dejafu conc test = dejafus conc [test]

-- | Variant of 'dejafu' for computations which do 'IO'.
dejafuIO :: (Eq a, Show a) => (forall t. ConcIO t a) -> (String, Predicate a) -> IO Bool
dejafuIO concio test = dejafusIO concio [test]

-- | Run a collection of tests, returning 'True' if all pass.
dejafus :: (Eq a, Show a) => (forall t. Conc t a) -> [(String, Predicate a)] -> IO Bool
dejafus conc tests = do
  results <- mapM (\(name, test) -> doTest name $ runTest test conc) tests
  return $ and results

-- | Variant of 'dejafus' for computations which do 'IO'.
dejafusIO :: (Eq a, Show a) => (forall t. ConcIO t a) -> [(String, Predicate a)] -> IO Bool
dejafusIO concio tests = do
  results <- mapM (\(name, test) -> doTest name =<< runTestIO test concio) tests
  return $ and results

-- | Automatically test a computation. In particular, look for
-- deadlocks and multiple return values.
autocheck :: (Eq a, Show a) => (forall t. Conc t a) -> IO Bool
autocheck conc = dejafus conc cases where
  cases = [ ("Never Deadlocks",   deadlocksNever)
          , ("Consistent Result", alwaysSame)
          ]

-- | Variant of 'autocheck' for computations which do 'IO'.
autocheckIO :: (Eq a, Show a) => (forall t. ConcIO t a) -> IO Bool
autocheckIO concio = dejafusIO concio cases where
  cases = [ ("Never Deadlocks",   deadlocksNever)
          , ("Consistent Result", alwaysSame)
          ]

-- * Test cases

-- | The results of a test, including information on the number of
-- cases checked, and number of total cases. Be careful if using the
-- total number of cases, as that value may be very big, and (due to
-- laziness) will actually force a lot more computation!.
data Result a = Result
  { _pass         :: Bool
  -- ^ Whether the test passed or not.
  , _casesChecked :: Int
  -- ^ The number of cases checked.
  , _casesTotal   :: Int
  -- ^ The total number of cases.
  , _failures :: [(Either Failure a, Trace)]
  -- ^ The failed cases, if any.
  } deriving (Show, Eq)

instance NFData a => NFData (Result a) where
  rnf r = rnf (_pass r, _casesChecked r, _casesTotal r, _failures r)

instance Functor Result where
  fmap f r = r { _failures = map (first $ fmap f) $ _failures r }

-- | Run a predicate over all executions with two or fewer
-- pre-emptions. A pre-emption is a context switch where the old
-- thread was still runnable.
--
-- In the resultant traces, a pre-emption is displayed as \"Px\",
-- where @x@ is the ID of the thread being switched to, whereas a
-- regular context switch is displayed as \"Sx\" (for \"start\").
runTest :: Eq a => Predicate a -> (forall t. Conc t a) -> Result a
runTest = runTest' 2

-- | Variant of 'runTest' for computations which do 'IO'.
runTestIO :: Eq a => Predicate a -> (forall t. ConcIO t a) -> IO (Result a)
runTestIO = runTestIO' 2

-- | Variant of 'runTest' which takes a pre-emption bound.
runTest' :: Eq a => Int -> Predicate a -> (forall t. Conc t a) -> Result a
runTest' pb predicate conc = r { _failures = uniques $ _failures r } where
  r = predicate $ sctPreBound pb conc

-- | Variant of 'runTest'' for computations which do 'IO'.
runTestIO' :: Eq a => Int -> Predicate a -> (forall t. ConcIO t a) -> IO (Result  a)
runTestIO' pb predicate conc = do
  r <- predicate <$> sctPreBoundIO pb conc
  return $ r { _failures = uniques $ _failures r }

-- | Strip out duplicates
uniques :: Eq a => [(a, Trace)] -> [(a, Trace)]
uniques = nub . sortNubBy simplicity

-- | Determine which of two failures is simpler, if they are comparable.
simplicity :: Eq a => (a, Trace) -> (a, Trace) -> Maybe Ordering
simplicity (r, t) (s, u)
  | r /= s = Nothing
  | otherwise = Just $ mconcat
    [ preEmpCount t' `compare` preEmpCount u'
    , contextSwitchCount t' `compare` contextSwitchCount u'
    , lexicographic t' u'
    ]

  where
    t' = map (\(d,_,_) -> d) t
    u' = map (\(d,_,_) -> d) u

    contextSwitchCount (Start _:ss) = 1 + contextSwitchCount ss
    contextSwitchCount (_:ss) = contextSwitchCount ss
    contextSwitchCount _ = 0::Int

    lexicographic (SwitchTo i:_) (SwitchTo j:_) = i `compare` j
    lexicographic (Start i:_) (Start j:_) = i `compare` j
    lexicographic (Continue:as) (b:bs) = if b /= Continue then LT else lexicographic as bs
    lexicographic (_:as) (_:bs) = lexicographic as bs
    lexicographic [] [] = EQ
    lexicographic [] _  = LT
    lexicographic _ []  = GT

-- * Predicates

-- | A @Predicate@ is a function which collapses a list of results
-- into a 'Result'.
type Predicate a = [(Either Failure a, Trace)] -> Result a

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
-- particular this means either: (a) it always deadlocks, or (b) the
-- result is always 'Just' @x@, for some fixed @x@.
alwaysSame :: Eq a => Predicate a
alwaysSame = alwaysTrue2 (==)

-- | Check that the result of a computation is not always the same.
notAlwaysSame :: Eq a => Predicate a
notAlwaysSame = somewhereTrue2 (/=)

-- | Check that the result of a unary boolean predicate is always
-- true.
alwaysTrue :: (Either Failure a -> Bool) -> Predicate a
alwaysTrue p xs = go xs Result { _pass = True, _casesChecked = 0, _casesTotal = len, _failures = failures } where
  go [] res = res
  go ((y,_):ys) res
    | p y = go ys $ incCC res
    | otherwise = incCC res { _pass = False }

  (len, failures) = findFailures1 p xs

-- | Check that the result of a binary boolean predicate is always
-- true between adjacent pairs of results. In general, it is probably
-- best to only check properties here which are transitive and
-- symmetric, in order to draw conclusions about the entire collection
-- of executions.
--
-- If the predicate fails, /both/ (result,trace) tuples will be added
-- to the failures list.
alwaysTrue2 :: (Either Failure a -> Either Failure a -> Bool) -> Predicate a
alwaysTrue2 _ [_] = Result { _pass = True, _casesChecked = 1, _casesTotal = 1, _failures = [] }
alwaysTrue2 p xs  = go xs Result { _pass = True, _casesChecked = 0, _casesTotal = len, _failures = failures } where
  go [] = id
  go [(y1,_),(y2,_)]    = check y1 y2 []
  go ((y1,_):(y2,t):ys) = check y1 y2 ((y2,t) : ys)

  check y1 y2 ys res
    | p y1 y2   = go ys $ incCC res
    | otherwise = incCC res { _pass = False }

  (len, failures) = findFailures2 p xs

-- | Check that the result of a unary boolean predicate is true at
-- least once.
somewhereTrue :: (Either Failure a -> Bool) -> Predicate a
somewhereTrue p xs = go xs Result { _pass = False, _casesChecked = 0, _casesTotal = len, _failures = failures } where
  go [] res = res
  go ((y,_):ys) res
    | p y = incCC res { _pass = True }
    | otherwise = go ys $ incCC res

  (len, failures) = findFailures1 p xs

-- | Check that the result of a binary boolean predicate is true
-- between at least one adjacent pair of results. In general, it is
-- probably best to only check properties here which are transitive
-- and symmetric, in order to draw conclusions about the entire
-- collection of executions.
--
-- If the predicate fails, /both/ (result,trace) tuples will be added
-- to the failures list.
somewhereTrue2 :: (Either Failure a -> Either Failure a -> Bool) -> Predicate a
somewhereTrue2 _ [x] = Result { _pass = False, _casesChecked = 1, _casesTotal = 1, _failures = [x] }
somewhereTrue2 p xs  = go xs Result { _pass = False, _casesChecked = 0, _casesTotal = len, _failures = failures } where
  go [] = id
  go [(y1,_),(y2,_)]    = check y1 y2 []
  go ((y1,_):(y2,t):ys) = check y1 y2 ((y2,t) : ys)

  check y1 y2 ys res
    | p y1 y2   = incCC res { _pass = True }
    | otherwise = go ys $ incCC res

  (len, failures) = findFailures2 p xs

-- * Internal

-- | Run a test and print to stdout
doTest :: (Eq a, Show a) => String -> Result a -> IO Bool
doTest name result = do
  if _pass result
  then
    -- Display a pass message.
    putStrLn $ "\27[32m[pass]\27[0m " ++ name ++ " (checked: " ++ show (_casesChecked result) ++ ")"
  else do
    -- Display a failure message, and the first 5 (simplified) failed traces
    putStrLn ("\27[31m[fail]\27[0m " ++ name ++ " (checked: " ++ show (_casesChecked result) ++ ")")

    let failures = _failures result
    mapM_ (\(r, t) -> putStrLn $ "\t" ++ either showfail show r ++ " " ++ showTrace t) $ take 5 failures
    when (moreThan failures 5) $
      putStrLn "\t..."

  return $ _pass result

-- | Increment the cases checked
incCC :: Result a -> Result a
incCC r = r { _casesChecked = _casesChecked r + 1 }

-- | Get the length of the list and find the failing cases in one
-- traversal.
findFailures1 :: (Either Failure a -> Bool) -> [(Either Failure a, Trace)] -> (Int, [(Either Failure a, Trace)])
findFailures1 p xs = findFailures xs 0 [] where
  findFailures [] l fs = (l, fs)
  findFailures ((z,t):zs) l fs
    | p z = findFailures zs (l+1) fs
    | otherwise = findFailures zs (l+1) ((z,t):fs)

-- | Get the length of the list and find the failing cases in one
-- traversal.
findFailures2 :: (Either Failure a -> Either Failure a -> Bool) -> [(Either Failure a, Trace)] -> (Int, [(Either Failure a, Trace)])
findFailures2 p xs = findFailures xs 0 [] where
  findFailures [] l fs = (l, fs)
  findFailures [_] l fs = (l+1, fs)
  findFailures ((z1,t1):(z2,t2):zs) l fs
    | p z1 z2 = findFailures ((z2,t2):zs) (l+1) fs
    | otherwise = findFailures ((z2,t2):zs) (l+1) ((z1,t1):(z2,t2):fs)

-- | Pretty-print a failure
showfail :: Failure -> String
showfail Deadlock          = "[deadlock]"
showfail STMDeadlock       = "[stm-deadlock]"
showfail InternalError     = "[internal-error]"
showfail FailureInNoTest   = "[_concNoTest]"
showfail UncaughtException = "[exception]"

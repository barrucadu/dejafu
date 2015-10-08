{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

-- | This module allows using Deja Fu predicates with Tasty to test
-- the behaviour of concurrent systems.
module Test.Tasty.DejaFu
  ( -- * Testing
    testAuto
  , testDejafu
  , testDejafus
  , testAutoIO
  , testDejafuIO
  , testDejafusIO

  -- * Testing under Relaxed Memory
  , MemType(..)
  , testAuto'
  , testAutoIO'
  , testDejafu'
  , testDejafus'
  , testDejafuIO'
  , testDejafusIO'
  ) where

import Data.Typeable (Typeable)
import Test.DejaFu
import Test.DejaFu.Deterministic (Conc, Trace, showFail, showTrace)
import Test.DejaFu.Deterministic.IO (ConcIO)
import Test.DejaFu.SCT (sctPreBound, sctPreBoundIO)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Providers (IsTest(..), singleTest, testPassed, testFailed)

--------------------------------------------------------------------------------
-- Automated testing

-- | Automatically test a computation. In particular, look for
-- deadlocks, uncaught exceptions, and multiple return values.
-- 
-- This uses the 'Conc' monad for testing, which is an instance of
-- 'MonadConc'. If you need to test something which also uses
-- 'MonadIO', use 'testAutoIO'.
testAuto :: (Eq a, Show a)
  => (forall t. Conc t a)
  -- ^ The computation to test
  -> TestTree
testAuto = testAuto' SequentialConsistency

-- | Variant of 'testAuto' which tests a computation under a given
-- memory model.
testAuto' :: (Eq a, Show a)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> (forall t. Conc t a)
  -- ^ The computation to test
  -> TestTree
testAuto' memtype conc = testDejafus' memtype 2 conc autocheckCases

-- | Variant of 'testAuto' for computations which do 'IO'.
testAutoIO :: (Eq a, Show a) => (forall t. ConcIO t a) -> TestTree
testAutoIO = testAutoIO' SequentialConsistency

-- | Variant of 'testAuto'' for computations which do 'IO'.
testAutoIO' :: (Eq a, Show a) => MemType -> (forall t. ConcIO t a) -> TestTree
testAutoIO' memtype concio = testDejafusIO' memtype 2 concio autocheckCases

-- | Predicates for the various autocheck functions.
autocheckCases :: Eq a => [(TestName, Predicate a)]
autocheckCases =
  [("Never Deadlocks", deadlocksNever)
  , ("No Exceptions", exceptionsNever)
  , ("Consistent Result", alwaysSame)
  ]

--------------------------------------------------------------------------------
-- Manual testing

-- | Check that a predicate holds.
testDejafu :: (Eq a, Show a)
  => (forall t. Conc t a)
  -- ^ The computation to test
  -> TestName
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> TestTree
testDejafu = testDejafu' SequentialConsistency 2

-- | Variant of 'testDejafu' which takes a memory model and
-- pre-emption bound.
testDejafu' :: (Eq a, Show a)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Int
  -- ^ The maximum number of pre-emptions to allow in a single
  -- execution
  -> (forall t. Conc t a)
  -- ^ The computation to test
  -> TestName
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> TestTree
testDejafu' memtype pb conc name p = testDejafus' memtype pb conc [(name, p)]

-- | Variant of 'testDejafu' which takes a collection of predicates to
-- test. This will share work between the predicates, rather than
-- running the concurrent computation many times for each predicate.
testDejafus :: (Eq a, Show a)
  => (forall t. Conc t a)
  -- ^ The computation to test
  -> [(TestName, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> TestTree
testDejafus = testDejafus' SequentialConsistency 2

-- | Variant of 'testDejafus' which takes a memory model and pre-emption
-- bound.
testDejafus' :: (Eq a, Show a)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Int
  -- ^ The maximum number of pre-emptions to allow in a single
  -- execution
  -> (forall t. Conc t a)
  -- ^ The computation to test
  -> [(TestName, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> TestTree
testDejafus' = test

-- | Variant of 'testDejafu' for computations which do 'IO'.
testDejafuIO :: (Eq a, Show a) => (forall t. ConcIO t a) -> TestName -> Predicate a -> TestTree
testDejafuIO = testDejafuIO' SequentialConsistency 2

-- | Variant of 'testDejafu'' for computations which do 'IO'.
testDejafuIO' :: (Eq a, Show a) => MemType -> Int -> (forall t. ConcIO t a) -> TestName -> Predicate a -> TestTree
testDejafuIO' memtype pb concio name p = testDejafusIO' memtype pb concio [(name, p)]

-- | Variant of 'testDejafus' for computations which do 'IO'.
testDejafusIO :: (Eq a, Show a) => (forall t. ConcIO t a) -> [(TestName, Predicate a)] -> TestTree
testDejafusIO = testDejafusIO' SequentialConsistency 2

-- | Variant of 'dejafus'' for computations which do 'IO'.
testDejafusIO' :: (Eq a, Show a) => MemType -> Int -> (forall t. ConcIO t a) -> [(TestName, Predicate a)] -> TestTree
testDejafusIO' = testio

--------------------------------------------------------------------------------
-- Tasty integration

data ConcTest where
  ConcTest   :: Show a => [(Either Failure a, Trace)] -> Predicate a -> ConcTest
  deriving Typeable

data ConcIOTest where
  ConcIOTest :: Show a => IO [(Either Failure a, Trace)] -> Predicate a -> ConcIOTest
  deriving Typeable

instance IsTest ConcTest where
  testOptions = return []

  run _ (ConcTest traces p) _ =
    let err = showErr $ p traces
     in return $ if null err then testPassed "" else testFailed err

instance IsTest ConcIOTest where
  testOptions = return []

  run _ (ConcIOTest iotraces p) _ = do
    traces <- iotraces
    let err = showErr $ p traces
    return $ if null err then testPassed "" else testFailed err

-- | Produce a Tasty 'TestTree' from a Deja Fu test.
test :: Show a => MemType -> Int -> (forall t. Conc t a) -> [(TestName, Predicate a)] -> TestTree
test memtype pb conc tests = case map toTest tests of
  [t] -> t
  ts  -> testGroup "Deja Fu Tests" ts

  where
    toTest (name, p) = singleTest name $ ConcTest traces p

    traces = sctPreBound memtype pb conc

-- | Produce a Tasty 'Test' from an IO-using Deja Fu test.
testio :: Show a => MemType -> Int -> (forall t. ConcIO t a) -> [(TestName, Predicate a)] -> TestTree
testio memtype pb concio tests = case map toTest tests of
  [t] -> t
  ts  -> testGroup "Deja Fu Tests" ts

  where
    toTest (name, p) = singleTest name $ ConcIOTest traces p

    -- As with HUnit, constructing a test is side-effect free, so
    -- sharing of traces can't happen here.
    traces = sctPreBoundIO memtype pb concio

-- | Convert a test result into an error message on failure (empty
-- string on success).
showErr :: Show a => Result a -> String
showErr res
  | _pass res = ""
  | otherwise = "Failed after " ++ show (_casesChecked res) ++ " cases:\n" ++ unlines failures ++ rest where

  failures = map (\(r, t) -> "\t" ++ either showFail show r ++ " " ++ showTrace t) . take 5 $ _failures res

  rest = if moreThan (_failures res) 5 then "\n\t..." else ""

-- | Check if a list has more than some number of elements.
moreThan :: [a] -> Int -> Bool
moreThan [] n = n < 0
moreThan _ 0  = True
moreThan (_:xs) n = moreThan xs (n-1)


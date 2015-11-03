{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}

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

  -- * Testing under Alternative Memory Models
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
import Test.DejaFu.SCT (sctPFBound, sctPFBoundIO)
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
testAuto = testAuto' TotalStoreOrder

-- | Variant of 'testAuto' which tests a computation under a given
-- memory model.
testAuto' :: (Eq a, Show a)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> (forall t. Conc t a)
  -- ^ The computation to test
  -> TestTree
testAuto' memtype conc = testDejafus' memtype 2 5 conc autocheckCases

-- | Variant of 'testAuto' for computations which do 'IO'.
testAutoIO :: (Eq a, Show a) => (forall t. ConcIO t a) -> TestTree
testAutoIO = testAutoIO' TotalStoreOrder

-- | Variant of 'testAuto'' for computations which do 'IO'.
testAutoIO' :: (Eq a, Show a) => MemType -> (forall t. ConcIO t a) -> TestTree
testAutoIO' memtype concio = testDejafusIO' memtype 2 5 concio autocheckCases

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
testDejafu :: Show a
  => (forall t. Conc t a)
  -- ^ The computation to test
  -> TestName
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> TestTree
testDejafu = testDejafu' TotalStoreOrder 2 5

-- | Variant of 'testDejafu' which takes a memory model and
-- pre-emption bound.
testDejafu' :: Show a
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Int
  -- ^ The maximum number of pre-emptions to allow in a single
  -- execution
  -> Int
  -- ^ The maximum difference between the number of yield operations
  -- across all threads.
  -> (forall t. Conc t a)
  -- ^ The computation to test
  -> TestName
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> TestTree
testDejafu' memtype pb fb conc name p = testDejafus' memtype pb fb conc [(name, p)]

-- | Variant of 'testDejafu' which takes a collection of predicates to
-- test. This will share work between the predicates, rather than
-- running the concurrent computation many times for each predicate.
testDejafus :: Show a
  => (forall t. Conc t a)
  -- ^ The computation to test
  -> [(TestName, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> TestTree
testDejafus = testDejafus' TotalStoreOrder 2 5

-- | Variant of 'testDejafus' which takes a memory model and pre-emption
-- bound.
testDejafus' :: Show a
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Int
  -- ^ The maximum number of pre-emptions to allow in a single
  -- execution
  -> Int
  -- ^ The maximum difference between the number of yield operations
  -- across all threads.
  -> (forall t. Conc t a)
  -- ^ The computation to test
  -> [(TestName, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> TestTree
testDejafus' = test

-- | Variant of 'testDejafu' for computations which do 'IO'.
testDejafuIO :: Show a => (forall t. ConcIO t a) -> TestName -> Predicate a -> TestTree
testDejafuIO = testDejafuIO' TotalStoreOrder 2 5

-- | Variant of 'testDejafu'' for computations which do 'IO'.
testDejafuIO' :: Show a => MemType -> Int -> Int -> (forall t. ConcIO t a) -> TestName -> Predicate a -> TestTree
testDejafuIO' memtype pb fb concio name p = testDejafusIO' memtype pb fb concio [(name, p)]

-- | Variant of 'testDejafus' for computations which do 'IO'.
testDejafusIO :: Show a => (forall t. ConcIO t a) -> [(TestName, Predicate a)] -> TestTree
testDejafusIO = testDejafusIO' TotalStoreOrder 2 5

-- | Variant of 'dejafus'' for computations which do 'IO'.
testDejafusIO' :: Show a => MemType -> Int -> Int -> (forall t. ConcIO t a) -> [(TestName, Predicate a)] -> TestTree
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
test :: Show a => MemType -> Int -> Int -> (forall t. Conc t a) -> [(TestName, Predicate a)] -> TestTree
test memtype pb fb conc tests = case map toTest tests of
  [t] -> t
  ts  -> testGroup "Deja Fu Tests" ts

  where
    toTest (name, p) = singleTest name $ ConcTest traces p

    traces = sctPFBound memtype pb fb conc

-- | Produce a Tasty 'Test' from an IO-using Deja Fu test.
testio :: Show a => MemType -> Int -> Int -> (forall t. ConcIO t a) -> [(TestName, Predicate a)] -> TestTree
testio memtype pb fb concio tests = case map toTest tests of
  [t] -> t
  ts  -> testGroup "Deja Fu Tests" ts

  where
    toTest (name, p) = singleTest name $ ConcIOTest traces p

    -- As with HUnit, constructing a test is side-effect free, so
    -- sharing of traces can't happen here.
    traces = sctPFBoundIO memtype pb fb concio

-- | Convert a test result into an error message on failure (empty
-- string on success).
showErr :: Show a => Result a -> String
showErr res
  | _pass res = ""
  | otherwise = "Failed after " ++ show (_casesChecked res) ++ " cases:\n" ++ msg ++ unlines failures ++ rest where

  msg = if null (_failureMsg res) then "" else _failureMsg res ++ "\n"

  failures = map (\(r, t) -> "\t" ++ either showFail show r ++ " " ++ showTrace t) . take 5 $ _failures res

  rest = if moreThan (_failures res) 5 then "\n\t..." else ""

-- | Check if a list has more than some number of elements.
moreThan :: [a] -> Int -> Bool
moreThan [] n = n < 0
moreThan _ 0  = True
moreThan (_:xs) n = moreThan xs (n-1)


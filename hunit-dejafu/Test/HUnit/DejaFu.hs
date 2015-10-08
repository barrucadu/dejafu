{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

-- | This module allows using Deja Fu predicates with HUnit to test
-- the behaviour of concurrent systems.
module Test.HUnit.DejaFu
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
  , testDejafus'
  , testDejafusIO'
  ) where

import Test.DejaFu
import Test.DejaFu.Deterministic (Conc, showFail, showTrace)
import Test.DejaFu.Deterministic.IO (ConcIO)
import Test.DejaFu.SCT (sctPreBound, sctPreBoundIO)
import Test.HUnit

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
  -> ConcTest a
testAuto = testAuto' SequentialConsistency

-- | Variant of 'testAuto' which tests a computation under a given
-- memory model.
testAuto' :: (Eq a, Show a)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> (forall t. Conc t a)
  -- ^ The computation to test
  -> ConcTest a
testAuto' memtype conc = testDejafus' memtype 2 conc autocheckCases

-- | Variant of 'testAuto' for computations which do 'IO'.
testAutoIO :: (Eq a, Show a) => (forall t. ConcIO t a) -> ConcIOTest a
testAutoIO = testAutoIO' SequentialConsistency

-- | Variant of 'testAuto'' for computations which do 'IO'.
testAutoIO' :: (Eq a, Show a) => MemType -> (forall t. ConcIO t a) -> ConcIOTest a
testAutoIO' memtype concio = testDejafusIO' memtype 2 concio autocheckCases

-- | Predicates for the various autocheck functions.
autocheckCases :: Eq a => [(String, Predicate a)]
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
  -> String
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> ConcTest a
testDejafu conc name p = testDejafus conc [(name, p)]

-- | Variant of 'testDejafu' which takes a collection of predicates to
-- test. This will share work between the predicates, rather than
-- running the concurrent computation many times for each predicate.
testDejafus :: (Eq a, Show a)
  => (forall t. Conc t a)
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> ConcTest a
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
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> ConcTest a
testDejafus' = ConcTest

-- | Variant of 'testDejafu' for computations which do 'IO'.
testDejafuIO :: (Eq a, Show a) => (forall t. ConcIO t a) -> String -> Predicate a -> ConcIOTest a
testDejafuIO concio name p = testDejafusIO concio [(name, p)]

-- | Variant of 'testDejafus' for computations which do 'IO'.
testDejafusIO :: (Eq a, Show a) => (forall t. ConcIO t a) -> [(String, Predicate a)] -> ConcIOTest a
testDejafusIO = testDejafusIO' SequentialConsistency 2

-- | Variant of 'dejafus'' for computations which do 'IO'.
testDejafusIO' :: (Eq a, Show a) => MemType -> Int -> (forall t. ConcIO t a) -> [(String, Predicate a)] -> ConcIOTest a
testDejafusIO' = ConcIOTest

--------------------------------------------------------------------------------
-- HUnit integration

data ConcTest a where
  ConcTest :: MemType -> Int -> (forall t. Conc t a) -> [(String, Predicate a)] -> ConcTest a

data ConcIOTest a where
  ConcIOTest :: MemType -> Int -> (forall t. ConcIO t a) -> [(String, Predicate a)] -> ConcIOTest a

instance Show a => Testable (ConcTest a) where
  test (ConcTest memtype pb conc tests) = case map toTest tests of
    [t] -> t
    ts  -> TestList ts

    where
      -- TODO: Sharing of traces
      toTest (name, p) = TestLabel name . TestCase $ do
        let traces = sctPreBound memtype pb conc
        assertString . showErr $ p traces

instance Show a => Testable (ConcIOTest a) where
  test (ConcIOTest memtype pb concio tests) = case map toTest tests of
    [t] -> t
    ts  -> TestList ts

    where
      -- TODO: Sharing of traces
      toTest (name, p) = TestLabel name . TestCase $ do
        traces <- sctPreBoundIO memtype pb concio
        assertString . showErr $ p traces

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


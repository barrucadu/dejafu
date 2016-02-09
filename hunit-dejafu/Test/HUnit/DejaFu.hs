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

  -- * Testing under Alternative Memory Models
  , MemType(..)
  , testAuto'
  , testAutoIO'
  , testDejafu'
  , testDejafus'
  , testDejafuIO'
  , testDejafusIO'
  ) where

import Data.List (intercalate, intersperse)
import Test.DejaFu
import Test.DejaFu.Deterministic (ConcST, ConcIO, showFail, showTrace)
import Test.DejaFu.SCT (sctBound, sctBoundIO)
import Test.HUnit (Test(..), assertString)

--------------------------------------------------------------------------------
-- Automated testing

-- | Automatically test a computation. In particular, look for
-- deadlocks, uncaught exceptions, and multiple return values.
-- 
-- This uses the 'Conc' monad for testing, which is an instance of
-- 'MonadConc'. If you need to test something which also uses
-- 'MonadIO', use 'testAutoIO'.
testAuto :: (Eq a, Show a)
  => (forall t. ConcST t a)
  -- ^ The computation to test
  -> Test
testAuto = testAuto' defaultMemType

-- | Variant of 'testAuto' which tests a computation under a given
-- memory model.
testAuto' :: (Eq a, Show a)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> Test
testAuto' memtype conc = testDejafus' memtype defaultBounds conc autocheckCases

-- | Variant of 'testAuto' for computations which do 'IO'.
testAutoIO :: (Eq a, Show a) => ConcIO a -> Test
testAutoIO = testAutoIO' defaultMemType

-- | Variant of 'testAuto'' for computations which do 'IO'.
testAutoIO' :: (Eq a, Show a) => MemType -> ConcIO a -> Test
testAutoIO' memtype concio = testDejafusIO' memtype defaultBounds concio autocheckCases

-- | Predicates for the various autocheck functions.
autocheckCases :: Eq a => [(String, Predicate a)]
autocheckCases =
  [("Never Deadlocks", representative deadlocksNever)
  , ("No Exceptions", representative exceptionsNever)
  , ("Consistent Result", alwaysSame)
  ]

--------------------------------------------------------------------------------
-- Manual testing

-- | Check that a predicate holds.
testDejafu :: Show a
  => (forall t. ConcST t a)
  -- ^ The computation to test
  -> String
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> Test
testDejafu = testDejafu' defaultMemType defaultBounds

-- | Variant of 'testDejafu' which takes a memory model and
-- pre-emption bound.
testDejafu' :: Show a
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bound.
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> String
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> Test
testDejafu' memtype cb conc name p = testDejafus' memtype cb conc [(name, p)]

-- | Variant of 'testDejafu' which takes a collection of predicates to
-- test. This will share work between the predicates, rather than
-- running the concurrent computation many times for each predicate.
testDejafus :: Show a
  => (forall t. ConcST t a)
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> Test
testDejafus = testDejafus' defaultMemType defaultBounds

-- | Variant of 'testDejafus' which takes a memory model and pre-emption
-- bound.
testDejafus' :: Show a
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bounds
  -> (forall t. ConcST t a)
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> Test
testDejafus' = test

-- | Variant of 'testDejafu' for computations which do 'IO'.
testDejafuIO :: Show a => ConcIO a -> String -> Predicate a -> Test
testDejafuIO = testDejafuIO' defaultMemType defaultBounds

-- | Variant of 'testDejafu'' for computations which do 'IO'.
testDejafuIO' :: Show a => MemType -> Bounds -> ConcIO a -> String -> Predicate a -> Test
testDejafuIO' memtype cb concio name p = testDejafusIO' memtype cb concio [(name, p)]

-- | Variant of 'testDejafus' for computations which do 'IO'.
testDejafusIO :: Show a => ConcIO a -> [(String, Predicate a)] -> Test
testDejafusIO = testDejafusIO' defaultMemType defaultBounds

-- | Variant of 'dejafus'' for computations which do 'IO'.
testDejafusIO' :: Show a => MemType -> Bounds -> ConcIO a -> [(String, Predicate a)] -> Test
testDejafusIO' = testio

--------------------------------------------------------------------------------
-- HUnit integration

-- | Produce a HUnit 'Test' from a Deja Fu test.
test :: Show a => MemType -> Bounds -> (forall t. ConcST t a) -> [(String, Predicate a)] -> Test
test memtype cb conc tests = case map toTest tests of
  [t] -> t
  ts  -> TestList ts

  where
    toTest (name, p) = TestLabel name . TestCase $
      assertString . showErr $ p traces

    traces = sctBound memtype cb conc

-- | Produce a HUnit 'Test' from an IO-using Deja Fu test.
testio :: Show a => MemType -> Bounds -> ConcIO a -> [(String, Predicate a)] -> Test
testio memtype cb concio tests = case map toTest tests of
  [t] -> t
  ts  -> TestList ts

  where
    toTest (name, p) = TestLabel name . TestCase $ do
      -- Sharing of traces probably not possible (without something
      -- really unsafe) here, as 'test' doesn't allow side-effects
      -- (eg, constructing an 'MVar' to share the traces after one
      -- test computed them).
      traces <- sctBoundIO memtype cb concio
      assertString . showErr $ p traces

-- | Convert a test result into an error message on failure (empty
-- string on success).
showErr :: Show a => Result a -> String
showErr res
  | _pass res = ""
  | otherwise = "Failed after " ++ show (_casesChecked res) ++ " cases:\n" ++ msg ++ unlines failures ++ rest where

  msg = if null (_failureMsg res) then "" else _failureMsg res ++ "\n"

  failures = intersperse "" . map (\(r, t) -> indent $ either showFail show r ++ " " ++ showTrace t) . take 5 $ _failures res

  rest = if moreThan (_failures res) 5 then "\n\t..." else ""

-- | Check if a list has more than some number of elements.
moreThan :: [a] -> Int -> Bool
moreThan [] n = n < 0
moreThan _ 0  = True
moreThan (_:xs) n = moreThan xs (n-1)

-- | Indent every line of a string.
indent :: String -> String
indent = intercalate "\n" . map ('\t':) . lines

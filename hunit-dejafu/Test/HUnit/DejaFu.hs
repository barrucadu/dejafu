{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 800
-- Impredicative polymorphism checks got stronger in GHC 8, breaking
-- the use of 'unsafeCoerce' below.
{-# LANGUAGE ImpredicativeTypes #-}
#endif

-- |
-- Module      : Test.HUnit.DejaFu
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : CPP, FlexibleContexts, FlexibleInstances, ImpredicativeTypes, RankNTypes, ScopedTypeVariables, TypeSynonymInstances
--
-- This module allows using Deja Fu predicates with HUnit to test the
-- behaviour of concurrent systems.
module Test.HUnit.DejaFu
  ( -- * Unit testing

  -- | This is supported by the 'Assertable' and 'Testable'
  -- instances for 'ConcST' and 'ConcIO'. These instances try all
  -- executions, reporting as failures the cases which throw an
  -- 'HUnitFailure' exception.
  --
  -- @instance Testable   (ConcST t ())@
  -- @instance Assertable (ConcST t ())@
  -- @instance Testable   (ConcIO   ())@
  -- @instance Assertable (ConcIO   ())@
  --
  -- These instances use 'defaultWay' and 'defaultMemType'.

  -- * Unit testing
    testAuto
  , testDejafu
  , testDejafus

  , testAutoWay
  , testDejafuWay
  , testDejafusWay

  -- ** @IO@
  , testAutoIO
  , testDejafuIO
  , testDejafusIO

  , testAutoWayIO
  , testDejafuWayIO
  , testDejafusWayIO

  -- ** Re-exports
  , Way
  , defaultWay
  , systematically
  , randomly
  , swarmy
  , Bounds(..)
  , defaultBounds
  , MemType(..)
  , defaultMemType

  -- * Refinement property testing
  , testProperty

  -- ** Re-exports
  , R.Sig(..)
  , R.RefinementProperty
  , R.Testable(..)
  , R.Listable(..)
  , R.expectFailure
  , R.refines, (R.=>=)
  , R.strictlyRefines, (R.->-)
  , R.equivalentTo, (R.===)
  ) where

import           Control.Monad.Catch    (try)
import           Control.Monad.ST       (runST)
import qualified Data.Foldable          as F
import           Data.List              (intercalate, intersperse)
import           Test.DejaFu            hiding (Testable(..))
import qualified Test.DejaFu.Conc       as Conc
import qualified Test.DejaFu.Refinement as R
import qualified Test.DejaFu.SCT        as SCT
import           Test.HUnit             (Assertable(..), Test(..), Testable(..),
                                         assertFailure, assertString)
import           Test.HUnit.Lang        (HUnitFailure(..))

-- Can't put the necessary forall in the @Assertable Conc.ConcST t@
-- instance :(
import           Unsafe.Coerce          (unsafeCoerce)

runSCTst :: Way -> MemType -> (forall t. Conc.ConcST t a) -> [(Either Failure a, Conc.Trace)]
runSCTst way memtype conc = runST (SCT.runSCT way memtype conc)

runSCTio :: Way -> MemType -> Conc.ConcIO a -> IO [(Either Failure a, Conc.Trace)]
runSCTio = SCT.runSCT

--------------------------------------------------------------------------------
-- HUnit-style unit testing

-- | @since 0.3.0.0
instance Testable (Conc.ConcST t ()) where
  test conc = TestCase (assert conc)

-- | @since 0.3.0.0
instance Testable (Conc.ConcIO ()) where
  test conc = TestCase (assert conc)

-- | @since 0.3.0.0
instance Assertable (Conc.ConcST t ()) where
  assert conc = do
    let traces = runSCTst' conc'
    assertString . showErr $ assertableP traces

    where
      conc' :: Conc.ConcST t (Either HUnitFailure ())
      conc' = try conc

      runSCTst' :: Conc.ConcST t (Either HUnitFailure ()) -> [(Either Failure (Either HUnitFailure ()), Conc.Trace)]
      runSCTst' = unsafeCoerce $ runSCTst defaultWay defaultMemType

-- | @since 0.3.0.0
instance Assertable (Conc.ConcIO ()) where
  assert conc = do
    traces <- runSCTio defaultWay defaultMemType (try conc)
    assertString . showErr $ assertableP traces

assertableP :: Predicate (Either HUnitFailure ())
assertableP = alwaysTrue $ \r -> case r of
  Right (Left HUnitFailure {}) -> False
  _ -> True


--------------------------------------------------------------------------------
-- DejaFu-style unit testing

-- | Automatically test a computation. In particular, look for
-- deadlocks, uncaught exceptions, and multiple return values.
--
-- This uses the 'Conc' monad for testing, which is an instance of
-- 'MonadConc'. If you need to test something which also uses
-- 'MonadIO', use 'testAutoIO'.
--
-- @since 0.2.0.0
testAuto :: (Eq a, Show a)
  => (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> Test
testAuto = testAutoWay defaultWay defaultMemType

-- | Variant of 'testAuto' which tests a computation under a given
-- execution way and memory model.
--
-- @since 0.5.0.0
testAutoWay :: (Eq a, Show a)
  => Way
  -- ^ How to execute the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> Test
testAutoWay way memtype conc =
  testDejafusWay way memtype conc autocheckCases

-- | Variant of 'testAuto' for computations which do 'IO'.
--
-- @since 0.2.0.0
testAutoIO :: (Eq a, Show a) => Conc.ConcIO a -> Test
testAutoIO = testAutoWayIO defaultWay defaultMemType

-- | Variant of 'testAutoWay' for computations which do 'IO'.
--
-- @since 0.5.0.0
testAutoWayIO :: (Eq a, Show a)
  => Way -> MemType -> Conc.ConcIO a -> Test
testAutoWayIO way memtype concio =
  testDejafusWayIO way memtype concio autocheckCases

-- | Predicates for the various autocheck functions.
autocheckCases :: Eq a => [(String, Predicate a)]
autocheckCases =
  [("Never Deadlocks", representative deadlocksNever)
  , ("No Exceptions", representative exceptionsNever)
  , ("Consistent Result", alwaysSame)
  ]

-- | Check that a predicate holds.
--
-- @since 0.2.0.0
testDejafu :: Show a
  => (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> String
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> Test
testDejafu = testDejafuWay defaultWay defaultMemType

-- | Variant of 'testDejafu' which takes a way to execute the program
-- and a memory model.
--
-- @since 0.5.0.0
testDejafuWay :: Show a
  => Way
  -- ^ How to execute the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> String
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> Test
testDejafuWay way memtype conc name p =
  testDejafusWay way memtype conc [(name, p)]

-- | Variant of 'testDejafu' which takes a collection of predicates to
-- test. This will share work between the predicates, rather than
-- running the concurrent computation many times for each predicate.
--
-- @since 0.2.0.0
testDejafus :: Show a
  => (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> Test
testDejafus = testDejafusWay defaultWay defaultMemType

-- | Variant of 'testDejafus' which takes a way to execute the program
-- and a memory model.
--
-- @since 0.5.0.0
testDejafusWay :: Show a
  => Way
  -- ^ How to execute the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> Test
testDejafusWay = testst

-- | Variant of 'testDejafu' for computations which do 'IO'.
--
-- @since 0.2.0.0
testDejafuIO :: Show a => Conc.ConcIO a -> String -> Predicate a -> Test
testDejafuIO = testDejafuWayIO defaultWay defaultMemType

-- | Variant of 'testDejafuWay' for computations which do 'IO'.
--
-- @since 0.5.0.0
testDejafuWayIO :: Show a
  => Way -> MemType -> Conc.ConcIO a -> String -> Predicate a -> Test
testDejafuWayIO way memtype concio name p =
  testDejafusWayIO way memtype concio [(name, p)]

-- | Variant of 'testDejafus' for computations which do 'IO'.
--
-- @since 0.2.0.0
testDejafusIO :: Show a => Conc.ConcIO a -> [(String, Predicate a)] -> Test
testDejafusIO = testDejafusWayIO defaultWay defaultMemType

-- | Variant of 'dejafusWay' for computations which do 'IO'.
--
-- @since 0.5.0.0
testDejafusWayIO :: Show a
  => Way -> MemType -> Conc.ConcIO a -> [(String, Predicate a)] -> Test
testDejafusWayIO = testio


-------------------------------------------------------------------------------
-- Refinement property testing

-- | Check a refinement property with a variety of seed values and
-- variable assignments.
--
-- @since unreleased
testProperty :: (R.Testable p, R.Listable (R.X p), Eq (R.X p), Show (R.X p), Show (R.O p))
  => String
  -- ^ The name of the test.
  -> p
  -- ^ The property to check.
  -> Test
testProperty = testprop


--------------------------------------------------------------------------------
-- HUnit integration

-- | Produce a HUnit 'Test' from a Deja Fu test.
testst :: Show a
  => Way -> MemType -> (forall t. Conc.ConcST t a) -> [(String, Predicate a)] -> Test
testst way memtype conc tests = case map toTest tests of
  [t] -> t
  ts  -> TestList ts

  where
    toTest (name, p) = TestLabel name . TestCase $
      assertString . showErr $ p traces

    traces = runSCTst way memtype conc

-- | Produce a HUnit 'Test' from an IO-using Deja Fu test.
testio :: Show a
  => Way -> MemType -> Conc.ConcIO a -> [(String, Predicate a)] -> Test
testio way memtype concio tests = case map toTest tests of
  [t] -> t
  ts  -> TestList ts

  where
    toTest (name, p) = TestLabel name . TestCase $ do
      -- Sharing of traces probably not possible (without something
      -- really unsafe) here, as 'test' doesn't allow side-effects
      -- (eg, constructing an 'MVar' to share the traces after one
      -- test computed them).
      traces <- runSCTio way memtype concio
      assertString . showErr $ p traces

-- | Produce a HUnit 'Test' from a Deja Fu refinement property test.
testprop :: (R.Testable p, R.Listable (R.X p), Eq (R.X p), Show (R.X p), Show (R.O p))
  => String -> p -> Test
testprop name p = TestLabel name . TestCase $ do
  ce <- R.check' p
  case ce of
    Just c -> assertFailure . init $ unlines
      [ "*** Failure: " ++
        (if null (R.failingArgs c) then "" else unwords (R.failingArgs c) ++ " ") ++
        "(seed " ++ show (R.failingSeed c) ++ ")"
      , "    left:  " ++ show (F.toList $ R.leftResults  c)
      , "    right: " ++ show (F.toList $ R.rightResults c)
      ]
    Nothing -> pure ()


--------------------------------------------------------------------------------
-- Utilities

-- | Convert a test result into an error message on failure (empty
-- string on success).
showErr :: Show a => Result a -> String
showErr res
  | _pass res = ""
  | otherwise = "Failed after " ++ show (_casesChecked res) ++ " cases:\n" ++ msg ++ unlines failures ++ rest where

  msg = if null (_failureMsg res) then "" else _failureMsg res ++ "\n"

  failures = intersperse "" . map (indent . showres) . take 5 $ _failures res

  showres (r, t) = either Conc.showFail show r ++ " " ++ Conc.showTrace t

  rest = if moreThan (_failures res) 5 then "\n\t..." else ""

-- | Check if a list has more than some number of elements.
moreThan :: [a] -> Int -> Bool
moreThan [] n = n < 0
moreThan _ 0  = True
moreThan (_:xs) n = moreThan xs (n-1)

-- | Indent every line of a string.
indent :: String -> String
indent = intercalate "\n" . map ('\t':) . lines

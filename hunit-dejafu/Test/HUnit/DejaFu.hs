{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : Test.HUnit.DejaFu
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : FlexibleContexts, FlexibleInstances, TypeSynonymInstances
--
-- This module allows using Deja Fu predicates with HUnit to test the
-- behaviour of concurrent systems.
module Test.HUnit.DejaFu
  ( -- * Unit testing

  -- | This is supported by the 'Assertable' and 'Testable' instances
  -- for 'ConcIO'.  These instances try all executions, reporting as
  -- failures the cases which throw an 'HUnitFailure' exception.
  --
  -- @instance Testable   (ConcIO ())@
  -- @instance Assertable (ConcIO ())@
  --
  -- These instances use 'defaultWay' and 'defaultMemType'.

  -- * Unit testing
    testAuto
  , testDejafu
  , testDejafus

  , testAutoWay
  , testDejafuWay
  , testDejafusWay

  , testDejafuDiscard

  -- ** Re-exports
  , Predicate
  , ProPredicate(..)
  , Way
  , defaultWay
  , systematically
  , randomly
  , uniformly
  , swarmy
  , Bounds(..)
  , defaultBounds
  , MemType(..)
  , defaultMemType
  , Discard(..)
  , defaultDiscarder

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
import qualified Data.Foldable          as F
import           Data.List              (intercalate, intersperse)
import           Test.DejaFu            hiding (Testable(..))
import qualified Test.DejaFu.Conc       as Conc
import qualified Test.DejaFu.Refinement as R
import qualified Test.DejaFu.SCT        as SCT
import           Test.HUnit             (Assertable(..), Test(..), Testable(..),
                                         assertFailure, assertString)
import           Test.HUnit.Lang        (HUnitFailure(..))

--------------------------------------------------------------------------------
-- HUnit-style unit testing

-- | @since 0.3.0.0
instance Testable (Conc.ConcIO ()) where
  test conc = TestCase (assert conc)

-- | @since 0.3.0.0
instance Assertable (Conc.ConcIO ()) where
  assert conc = do
    traces <- SCT.runSCTDiscard (const Nothing) defaultWay defaultMemType (try conc)
    assertString . showErr $ peval assertableP traces

assertableP :: Predicate (Either HUnitFailure ())
assertableP = alwaysTrue $ \r -> case r of
  Right (Left HUnitFailure {}) -> False
  _ -> True


--------------------------------------------------------------------------------
-- DejaFu-style unit testing

-- | Automatically test a computation. In particular, look for
-- deadlocks, uncaught exceptions, and multiple return values.
--
-- @since 0.8.0.0
testAuto :: (Eq a, Show a)
  => Conc.ConcIO a
  -- ^ The computation to test
  -> Test
testAuto = testAutoWay defaultWay defaultMemType

-- | Variant of 'testAuto' which tests a computation under a given
-- execution way and memory model.
--
-- @since 0.8.0.0
testAutoWay :: (Eq a, Show a)
  => Way
  -- ^ How to execute the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Conc.ConcIO a
  -- ^ The computation to test
  -> Test
testAutoWay way memtype conc =
  testDejafusWay way memtype conc autocheckCases

-- | Predicates for the various autocheck functions.
autocheckCases :: Eq a => [(String, Predicate a)]
autocheckCases =
  [("Never Deadlocks", representative deadlocksNever)
  , ("No Exceptions", representative exceptionsNever)
  , ("Consistent Result", alwaysSame)
  ]

-- | Check that a predicate holds.
--
-- @since 0.8.0.0
testDejafu :: Show b
  => Conc.ConcIO a
  -- ^ The computation to test
  -> String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check
  -> Test
testDejafu = testDejafuWay defaultWay defaultMemType

-- | Variant of 'testDejafu' which takes a way to execute the program
-- and a memory model.
--
-- @since 0.8.0.0
testDejafuWay :: Show b
  => Way
  -- ^ How to execute the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Conc.ConcIO a
  -- ^ The computation to test
  -> String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check
  -> Test
testDejafuWay = testDejafuDiscard (const Nothing)

-- | Variant of 'testDejafuWay' which can selectively discard results.
--
-- @since 0.8.0.0
testDejafuDiscard :: Show b
  => (Either Failure a -> Maybe Discard)
  -- ^ Selectively discard results.
  -> Way
  -- ^ How to execute the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Conc.ConcIO a
  -- ^ The computation to test
  -> String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check
  -> Test
testDejafuDiscard discard way memtype conc name test =
  testconc discard way memtype conc [(name, test)]

-- | Variant of 'testDejafu' which takes a collection of predicates to
-- test. This will share work between the predicates, rather than
-- running the concurrent computation many times for each predicate.
--
-- @since 0.8.0.0
testDejafus :: Show b
  => Conc.ConcIO a
  -- ^ The computation to test
  -> [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check
  -> Test
testDejafus = testDejafusWay defaultWay defaultMemType

-- | Variant of 'testDejafus' which takes a way to execute the program
-- and a memory model.
--
-- @since 0.8.0.0
testDejafusWay :: Show b
  => Way
  -- ^ How to execute the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Conc.ConcIO a
  -- ^ The computation to test
  -> [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check
  -> Test
testDejafusWay = testconc (const Nothing)


-------------------------------------------------------------------------------
-- Refinement property testing

-- | Check a refinement property with a variety of seed values and
-- variable assignments.
--
-- @since 0.6.0.0
testProperty :: (R.Testable p, R.Listable (R.X p), Eq (R.X p), Show (R.X p), Show (R.O p))
  => String
  -- ^ The name of the test.
  -> p
  -- ^ The property to check.
  -> Test
testProperty = testprop


--------------------------------------------------------------------------------
-- HUnit integration

-- | Produce a HUnit 'Test' from a Deja Fu unit test.
testconc :: Show b
  => (Either Failure a -> Maybe Discard)
  -> Way
  -> MemType
  -> Conc.ConcIO a
  -> [(String, ProPredicate a b)]
  -> Test
testconc discard way memtype concio tests = case map toTest tests of
  [t] -> t
  ts  -> TestList ts

  where
    toTest (name, p) = TestLabel name . TestCase $ do
      -- Sharing of traces probably not possible (without something
      -- really unsafe) here, as 'test' doesn't allow side-effects
      -- (eg, constructing an 'MVar' to share the traces after one
      -- test computed them).
      traces <- SCT.runSCTDiscard discard way memtype concio
      assertString . showErr $ peval p traces

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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Test.HUnit.DejaFu
-- Copyright   : (c) 2015--2019 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : FlexibleContexts, FlexibleInstances, LambdaCase
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

  , testAutoWithSettings
  , testDejafuWithSettings
  , testDejafusWithSettings

  -- ** Re-exports
  , Condition
  , Predicate
  , ProPredicate(..)
  -- *** Settings
  , module Test.DejaFu.Settings
  -- *** Expressing concurrent programs
  , Program
  , Basic
  , ConcT
  , ConcIO
  , WithSetup
  , WithSetupAndTeardown
  , withSetup
  , withTeardown
  , withSetupAndTeardown
  -- *** Invariants
  , Invariant
  , registerInvariant
  , inspectIORef
  , inspectMVar
  , inspectTVar

  -- * Refinement property testing
  , testProperty
  , testPropertyFor

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
import qualified Test.DejaFu.Settings
import qualified Test.DejaFu.Types      as D
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
    traces <- SCT.runSCTWithSettings (set ldiscard (Just (pdiscard assertableP)) defaultSettings) (try conc)
    assertString . showErr $ peval assertableP traces

assertableP :: Predicate (Either HUnitFailure ())
assertableP = alwaysTrue $ \case
  Right (Left HUnitFailure {}) -> False
  _ -> True


--------------------------------------------------------------------------------
-- DejaFu-style unit testing

-- | Automatically test a computation. In particular, look for
-- deadlocks, uncaught exceptions, and multiple return values.
--
-- @since 2.0.0.0
testAuto :: (Eq a, Show a)
  => Program pty IO a
  -- ^ The computation to test.
  -> Test
testAuto = testAutoWithSettings defaultSettings

-- | Variant of 'testAuto' which tests a computation under a given
-- execution way and memory model.
--
-- @since 2.0.0.0
testAutoWay :: (Eq a, Show a)
  => Way
  -- ^ How to execute the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @IORef@ operations.
  -> Program pty IO a
  -- ^ The computation to test.
  -> Test
testAutoWay way = testAutoWithSettings . fromWayAndMemType way

-- | Variant of 'testAuto' which takes a settings record.
--
-- @since 2.0.0.0
testAutoWithSettings :: (Eq a, Show a)
  => Settings IO a
  -- ^ The SCT settings.
  -> Program pty IO a
  -- ^ The computation to test.
  -> Test
testAutoWithSettings settings = testDejafusWithSettings settings
  [("Never Deadlocks", representative deadlocksNever)
  , ("No Exceptions", representative exceptionsNever)
  , ("Consistent Result", alwaysSame)
  ]

-- | Check that a predicate holds.
--
-- @since 2.0.0.0
testDejafu :: Show b
  => String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check.
  -> Program pty IO a
  -- ^ The computation to test.
  -> Test
testDejafu = testDejafuWithSettings defaultSettings

-- | Variant of 'testDejafu' which takes a way to execute the program
-- and a memory model.
--
-- @since 2.0.0.0
testDejafuWay :: Show b
  => Way
  -- ^ How to execute the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @IORef@ operations.
  -> String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check.
  -> Program pty IO a
  -- ^ The computation to test.
  -> Test
testDejafuWay way = testDejafuWithSettings . fromWayAndMemType way

-- | Variant of 'testDejafu' which takes a settings record.
--
-- @since 2.0.0.0
testDejafuWithSettings :: Show b
  => Settings IO a
  -- ^ The SCT settings.
  -> String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check.
  -> Program p IO a
  -- ^ The computation to test.
  -> Test
testDejafuWithSettings settings name p = testDejafusWithSettings settings [(name, p)]

-- | Variant of 'testDejafu' which takes a collection of predicates to
-- test. This will share work between the predicates, rather than
-- running the concurrent computation many times for each predicate.
--
-- @since 2.0.0.0
testDejafus :: Show b
  => [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check.
  -> Program pty IO a
  -- ^ The computation to test.
  -> Test
testDejafus = testDejafusWithSettings defaultSettings

-- | Variant of 'testDejafus' which takes a way to execute the program
-- and a memory model.
--
-- @since 2.0.0.0
testDejafusWay :: Show b
  => Way
  -- ^ How to execute the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @IORef@ operations.
  -> [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check.
  -> Program pty IO a
  -- ^ The computation to test.
  -> Test
testDejafusWay way = testDejafusWithSettings . fromWayAndMemType way

-- | Variant of 'testDejafus' which takes a settings record.
--
-- @since 2.0.0.0
testDejafusWithSettings :: Show b
  => Settings IO a
  -- ^ The SCT settings.
  -> [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check.
  -> Program pty IO a
  -- ^ The computation to test.
  -> Test
testDejafusWithSettings = testconc


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
testProperty = testPropertyFor 10 100

-- | Like 'testProperty', but takes a number of cases to check.
--
-- The maximum number of cases tried by @testPropertyFor n m@ will be
-- @n * m@.
--
-- @since 0.7.1.0
testPropertyFor :: (R.Testable p, R.Listable (R.X p), Eq (R.X p), Show (R.X p), Show (R.O p))
  => Int
  -- ^ The number of seed values to try.
  -> Int
  -- ^ The number of variable assignments per seed value to try.
  -> String
  -- ^ The name of the test.
  -> p
  -- ^ The property to check.
  -> Test
testPropertyFor = testprop


--------------------------------------------------------------------------------
-- HUnit integration

-- | Produce a HUnit 'Test' from a Deja Fu unit test.
testconc :: Show b
  => Settings IO a
  -> [(String, ProPredicate a b)]
  -> Program pty IO a
  -> Test
testconc settings tests concio = case map toTest tests of
  [t] -> t
  ts  -> TestList ts

  where
    toTest (name, p) = TestLabel name . TestCase $ do
      let discarder = maybe id D.strengthenDiscard (get ldiscard settings) (pdiscard p)
      traces <- SCT.runSCTWithSettings (set ldiscard (Just discarder) settings) concio
      assertString . showErr $ peval p traces

-- | Produce a HUnit 'Test' from a Deja Fu refinement property test.
testprop :: (R.Testable p, R.Listable (R.X p), Eq (R.X p), Show (R.X p), Show (R.O p))
  => Int -> Int -> String -> p -> Test
testprop sn vn name p = TestLabel name . TestCase $ do
  ce <- R.checkFor sn vn p
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
  | otherwise = "Failed:\n" ++ msg ++ unlines failures ++ rest where

  msg = if null (_failureMsg res) then "" else _failureMsg res ++ "\n"

  failures = intersperse "" . map (indent . showres) . take 5 $ _failures res

  showres (r, t) = either Conc.showCondition show r ++ " " ++ Conc.showTrace t

  rest = if moreThan (_failures res) 5 then "\n\t..." else ""

-- | Check if a list has more than some number of elements.
moreThan :: [a] -> Int -> Bool
moreThan [] n = n < 0
moreThan _ 0  = True
moreThan (_:xs) n = moreThan xs (n-1)

-- | Indent every line of a string.
indent :: String -> String
indent = intercalate "\n" . map ('\t':) . lines

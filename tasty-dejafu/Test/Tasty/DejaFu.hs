{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if MIN_TOOL_VERSION_ghc(8,0,0)
-- Impredicative polymorphism checks got stronger in GHC 8, breaking
-- the use of 'unsafeCoerce' below.
{-# LANGUAGE ImpredicativeTypes #-}
#endif

-- |
-- Module      : Test.Tasty.DejaFu
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : CPP, FlexibleContexts, FlexibleInstances, GADTs, ImpredicativeTypes, LambdaCase, RankNTypes, TypeSynonymInstances
--
-- This module allows using Deja Fu predicates with Tasty to test the
-- behaviour of concurrent systems.
module Test.Tasty.DejaFu
  ( -- * Unit testing

  -- | This is supported by the 'IsTest' instances for 'ConcST' and
  -- 'ConcIO'. These instances try all executions, reporting as
  -- failures the cases which return a 'Just' string.
  --
  -- @instance Typeable t => IsTest (ConcST t (Maybe String))@
  -- @instance               IsTest (ConcIO   (Maybe String))@
  -- @instance IsOption Bounds@
  -- @instance IsOption MemType@

  -- * Property testing
    testAuto
  , testDejafu
  , testDejafus

  , testAutoWay
  , testDejafuWay
  , testDejafusWay

  , testDejafuDiscard

  -- ** @IO@
  , testAutoIO
  , testDejafuIO
  , testDejafusIO

  , testAutoWayIO
  , testDejafuWayIO
  , testDejafusWayIO

  , testDejafuDiscardIO

  -- ** Re-exports
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

import           Control.Monad.ST       (runST)
import           Data.Char              (toUpper)
import qualified Data.Foldable          as F
import           Data.List              (intercalate, intersperse)
import           Data.Proxy             (Proxy(..))
import           Data.Tagged            (Tagged(..))
import           Data.Typeable          (Typeable)
import           System.Random          (mkStdGen)
import           Test.DejaFu            hiding (Testable(..))
import qualified Test.DejaFu.Conc       as Conc
import qualified Test.DejaFu.Refinement as R
import qualified Test.DejaFu.SCT        as SCT
import           Test.Tasty             (TestName, TestTree, testGroup)
import           Test.Tasty.Options     (IsOption(..), OptionDescription(..),
                                         lookupOption)
import           Test.Tasty.Providers   (IsTest(..), singleTest, testFailed,
                                         testPassed)

-- Can't put the necessary forall in the @IsTest ConcST t@
-- instance :(
import           Unsafe.Coerce          (unsafeCoerce)

runSCTst :: (Either Failure a -> Maybe Discard) -> Way -> MemType -> (forall t. Conc.ConcST t a) -> [(Either Failure a, Conc.Trace)]
runSCTst discard way memtype conc = runST (SCT.runSCTDiscard discard way memtype conc)

runSCTio :: (Either Failure a -> Maybe Discard) -> Way -> MemType -> Conc.ConcIO a -> IO [(Either Failure a, Conc.Trace)]
runSCTio = SCT.runSCTDiscard

--------------------------------------------------------------------------------
-- Tasty-style unit testing

-- | @since 0.3.0.0
instance Typeable t => IsTest (Conc.ConcST t (Maybe String)) where
  testOptions = Tagged concOptions

  run options conc callback = do
    let memtype = lookupOption options :: MemType
    let way     = lookupOption options :: Way
    let runSCTst' :: Conc.ConcST t (Maybe String) -> [(Either Failure (Maybe String), Conc.Trace)]
        runSCTst' = unsafeCoerce $ runSCTst (const Nothing) way memtype
    let traces = runSCTst' conc
    run options (ConcTest traces assertableP) callback

-- | @since 0.3.0.0
instance IsTest (Conc.ConcIO (Maybe String)) where
  testOptions = Tagged concOptions

  run options conc callback = do
    let memtype = lookupOption options
    let way     = lookupOption options
    let traces  = runSCTio (const Nothing) way memtype conc
    run options (ConcIOTest traces assertableP) callback

concOptions :: [OptionDescription]
concOptions =
  [ Option (Proxy :: Proxy MemType)
  , Option (Proxy :: Proxy Way)
  ]

assertableP :: Predicate (Maybe String)
assertableP = alwaysTrue $ \case
  Right (Just _) -> False
  _ -> True

-- | @since 0.3.0.0
instance IsOption MemType where
  defaultValue = defaultMemType
  parseValue = shortName . map toUpper where
    shortName "SC"  = Just SequentialConsistency
    shortName "TSO" = Just TotalStoreOrder
    shortName "PSO" = Just PartialStoreOrder
    shortName _ = Nothing
  optionName = Tagged "memory-model"
  optionHelp = Tagged "The memory model to use. This should be one of \"sc\", \"tso\", or \"pso\"."

-- | @since 0.5.0.0
instance IsOption Way where
  defaultValue = defaultWay
  parseValue = shortName . map toUpper where
    shortName "SYSTEMATICALLY" = Just (systematically defaultBounds)
    shortName "RANDOMLY"       = Just (randomly (mkStdGen 42) 100)
    shortName _ = Nothing
  optionName = Tagged "way"
  optionHelp = Tagged "The execution method to use. This should be one of \"systematically\" or \"randomly\"."


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
  -> TestTree
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
  -> TestTree
testAutoWay way memtype conc = testDejafusWay way memtype conc autocheckCases

-- | Variant of 'testAuto' for computations which do 'IO'.
--
-- @since 0.2.0.0
testAutoIO :: (Eq a, Show a) => Conc.ConcIO a -> TestTree
testAutoIO = testAutoWayIO defaultWay defaultMemType

-- | Variant of 'testAutoWay' for computations which do 'IO'.
--
-- @since 0.5.0.0
testAutoWayIO :: (Eq a, Show a)
  => Way -> MemType -> Conc.ConcIO a -> TestTree
testAutoWayIO way memtype concio =
  testDejafusWayIO way memtype  concio autocheckCases

-- | Predicates for the various autocheck functions.
autocheckCases :: Eq a => [(TestName, Predicate a)]
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
  -> TestName
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> TestTree
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
  -> TestName
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> TestTree
testDejafuWay = testDejafuDiscard (const Nothing)

-- | Variant of 'testDejafuWay' which can selectively discard results.
--
-- @since 0.7.0.0
testDejafuDiscard :: Show a
  => (Either Failure a -> Maybe Discard)
  -- ^ Selectively discard results.
  -> Way
  -- ^ How to execute the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> String
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> TestTree
testDejafuDiscard discard way memtype conc name test =
  testst discard way memtype conc [(name, test)]

-- | Variant of 'testDejafu' which takes a collection of predicates to
-- test. This will share work between the predicates, rather than
-- running the concurrent computation many times for each predicate.
--
-- @since 0.2.0.0
testDejafus :: Show a
  => (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> [(TestName, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> TestTree
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
  -> [(TestName, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> TestTree
testDejafusWay = testst (const Nothing)

-- | Variant of 'testDejafu' for computations which do 'IO'.
--
-- @since 0.2.0.0
testDejafuIO :: Show a => Conc.ConcIO a -> TestName -> Predicate a -> TestTree
testDejafuIO = testDejafuWayIO defaultWay defaultMemType

-- | Variant of 'testDejafuWay' for computations which do 'IO'.
--
-- @since 0.5.0.0
testDejafuWayIO :: Show a
  => Way -> MemType -> Conc.ConcIO a -> TestName -> Predicate a -> TestTree
testDejafuWayIO = testDejafuDiscardIO (const Nothing)

-- | Variant of 'testDejafuDiscard' for computations which do 'IO'.
--
-- @since 0.7.0.0
testDejafuDiscardIO :: Show a => (Either Failure a -> Maybe Discard) -> Way -> MemType -> Conc.ConcIO a -> String -> Predicate a -> TestTree
testDejafuDiscardIO discard way memtype concio name test =
  testio discard way memtype concio [(name, test)]

-- | Variant of 'testDejafus' for computations which do 'IO'.
--
-- @since 0.2.0.0
testDejafusIO :: Show a => Conc.ConcIO a -> [(TestName, Predicate a)] -> TestTree
testDejafusIO = testDejafusWayIO defaultWay defaultMemType

-- | Variant of 'dejafusWay' for computations which do 'IO'.
--
-- @since 0.5.0.0
testDejafusWayIO :: Show a
  => Way -> MemType -> Conc.ConcIO a -> [(TestName, Predicate a)] -> TestTree
testDejafusWayIO = testio (const Nothing)


-------------------------------------------------------------------------------
-- Refinement property testing

-- | Check a refinement property with a variety of seed values and
-- variable assignments.
--
-- @since 0.6.0.0
testProperty :: (R.Testable p, R.Listable (R.X p), Eq (R.X p), Show (R.X p), Show (R.O p))
  => TestName
  -- ^ The name of the test.
  -> p
  -- ^ The property to check.
  -> TestTree
testProperty = testPropertyFor 10 100

-- | Like 'testProperty', but takes a number of cases to check.
--
-- The maximum number of cases tried by @testPropertyFor n m@ will be
-- @n * m@.
--
-- @since unreleased
testPropertyFor :: (R.Testable p, R.Listable (R.X p), Eq (R.X p), Show (R.X p), Show (R.O p))
  => Int
  -- ^ The number of seed values to try.
  -> Int
  -- ^ The number of variable assignments per seed value to try.
  -> TestName
  -- ^ The name of the test.
  -> p
  -- ^ The property to check.
  -> TestTree
testPropertyFor = testprop


--------------------------------------------------------------------------------
-- Tasty integration

data ConcTest where
  ConcTest   :: Show a => [(Either Failure a, Conc.Trace)] -> Predicate a -> ConcTest
  deriving Typeable

data ConcIOTest where
  ConcIOTest :: Show a => IO [(Either Failure a, Conc.Trace)] -> Predicate a -> ConcIOTest
  deriving Typeable

data PropTest where
  PropTest :: (R.Testable p, R.Listable (R.X p), Eq (R.X p), Show (R.X p), Show (R.O p)) => Int -> Int -> p -> PropTest
  deriving Typeable

instance IsTest ConcTest where
  testOptions = pure []

  run _ (ConcTest traces p) _ =
    let err = showErr $ p traces
     in pure (if null err then testPassed "" else testFailed err)

instance IsTest ConcIOTest where
  testOptions = pure []

  run _ (ConcIOTest iotraces p) _ = do
    traces <- iotraces
    let err = showErr $ p traces
    pure (if null err then testPassed "" else testFailed err)

instance IsTest PropTest where
  testOptions = pure []

  run _ (PropTest sn vn p) _ = do
    ce <- R.checkFor sn vn p
    pure $ case ce of
      Just c -> testFailed . init $ unlines
        [ "*** Failure: " ++
          (if null (R.failingArgs c) then "" else unwords (R.failingArgs c) ++ " ") ++
          "(seed " ++ show (R.failingSeed c) ++ ")"
        , "    left:  " ++ show (F.toList $ R.leftResults  c)
        , "    right: " ++ show (F.toList $ R.rightResults c)
        ]
      Nothing -> testPassed ""

-- | Produce a Tasty 'TestTree' from a Deja Fu test.
testst :: Show a
  => (Either Failure a -> Maybe Discard)
  -> Way
  -> MemType
  -> (forall t. Conc.ConcST t a)
  -> [(TestName, Predicate a)]
  -> TestTree
testst discard way memtype conc tests = case map toTest tests of
  [t] -> t
  ts  -> testGroup "Deja Fu Tests" ts

  where
    toTest (name, p) = singleTest name $ ConcTest traces p

    traces = runSCTst discard way memtype conc

-- | Produce a Tasty 'Test' from an IO-using Deja Fu test.
testio :: Show a
  => (Either Failure a -> Maybe Discard)
  -> Way
  -> MemType
  -> Conc.ConcIO a
  -> [(TestName, Predicate a)]
  -> TestTree
testio discard way memtype concio tests = case map toTest tests of
  [t] -> t
  ts  -> testGroup "Deja Fu Tests" ts

  where
    toTest (name, p) = singleTest name $ ConcIOTest traces p

    -- As with HUnit, constructing a test is side-effect free, so
    -- sharing of traces can't happen here.
    traces = runSCTio discard way memtype concio

-- | Produce a Tasty 'TestTree' from a Deja Fu refinement property test.
testprop :: (R.Testable p, R.Listable (R.X p), Eq (R.X p), Show (R.X p), Show (R.O p))
  => Int -> Int -> TestName -> p -> TestTree
testprop sn vn name = singleTest name . PropTest sn vn

-- | Convert a test result into an error message on failure (empty
-- string on success).
showErr :: Show a => Result a -> String
showErr res
  | _pass res = ""
  | otherwise = "Failed after " ++ show (_casesChecked res) ++ " cases:\n" ++ msg ++ unlines failures ++ rest where

  msg = if null (_failureMsg res) then "" else _failureMsg res ++ "\n"

  failures = intersperse "" . map (\(r, t) -> indent $ either Conc.showFail show r ++ " " ++ Conc.showTrace t) . take 5 $ _failures res

  rest = if moreThan (_failures res) 5 then "\n\t..." else ""

-- | Check if a list has more than some number of elements.
moreThan :: [a] -> Int -> Bool
moreThan [] n = n < 0
moreThan _ 0  = True
moreThan (_:xs) n = moreThan xs (n-1)

-- | Indent every line of a string.
indent :: String -> String
indent = intercalate "\n" . map ('\t':) . lines

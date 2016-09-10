{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 800
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
-- Portability : CPP, FlexibleInstances, GADTs, ImpredicativeTypes, RankNTypes, TypeSynonymInstances
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
  , testUnsystematicRandom
  , testUnsystematicPCT

  , testAuto'
  , testDejafu'
  , testDejafus'
  , testUnsystematicRandom'
  , testUnsystematicPCT'

  -- ** @IO@
  , testAutoIO
  , testDejafuIO
  , testDejafusIO
  , testUnsystematicRandomIO
  , testUnsystematicPCTIO

  , testAutoIO'
  , testDejafuIO'
  , testDejafusIO'
  , testUnsystematicRandomIO'
  , testUnsystematicPCTIO'

  -- * Re-exports
  , Bounds(..)
  , MemType(..)

  -- * Building blocks
  , testPredicates
  ) where

import Control.Monad.ST (runST)
import Data.Char (toUpper)
import Data.List (intercalate, intersperse)
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..))
import Data.Typeable (Typeable)
import System.Random (RandomGen)
import Test.DejaFu
import qualified Test.DejaFu.SCT as SCT
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Options (OptionDescription(..), IsOption(..), lookupOption)
import Test.Tasty.Providers (IsTest(..), singleTest, testPassed, testFailed)
import qualified Test.DejaFu.Conc as Conc

-- Can't put the necessary forall in the @IsTest ConcST t@
-- instance :(
import Unsafe.Coerce (unsafeCoerce)

type Trc = Conc.Trace Conc.ThreadId Conc.ThreadAction Conc.Lookahead

sctBoundST :: MemType -> Bounds -> (forall t. Conc.ConcST t a) -> [(Either Failure a, Trc)]
sctBoundST memtype cb conc = runST (SCT.sctBound memtype cb conc)

sctBoundIO :: MemType -> Bounds -> Conc.ConcIO a -> IO [(Either Failure a, Trc)]
sctBoundIO = SCT.sctBound

--------------------------------------------------------------------------------
-- Unit testing

instance Typeable t => IsTest (Conc.ConcST t (Maybe String)) where
  testOptions = Tagged concOptions

  run options conc callback = do
    let memtype = lookupOption options :: MemType
    let bounds  = lookupOption options :: Bounds
    let sctBound' :: Conc.ConcST t (Maybe String) -> [(Either Failure (Maybe String), Trc)]
        sctBound' = unsafeCoerce $ sctBoundST memtype bounds
    let traces = sctBound' conc
    run options (ConcTest (pure traces) assertableP) callback

instance IsTest (Conc.ConcIO (Maybe String)) where
  testOptions = Tagged concOptions

  run options conc callback = do
    let memtype = lookupOption options
    let bounds  = lookupOption options
    let traces  = sctBoundIO memtype bounds conc
    run options (ConcTest traces assertableP) callback

concOptions :: [OptionDescription]
concOptions =
  [ Option (Proxy :: Proxy Bounds)
  , Option (Proxy :: Proxy MemType)
  ]

assertableP :: Predicate (Maybe String)
assertableP = alwaysTrue $ \r -> case r of
  Right (Just _) -> False
  _ -> True

instance IsOption Bounds where
  defaultValue = defaultBounds
  parseValue = const Nothing
  optionName = Tagged "schedule-bounds"
  optionHelp = Tagged "The schedule bounds to use. This cannot be set on the command line."

instance IsOption MemType where
  defaultValue = defaultMemType
  parseValue str = shortName (map toUpper str) where
    shortName "SC"  = Just SequentialConsistency
    shortName "TSO" = Just TotalStoreOrder
    shortName "PSO" = Just PartialStoreOrder
    shortName _ = Nothing
  optionName = Tagged "memory-model"
  optionHelp = Tagged "The memory model to use. This should be one of \"SC\", \"TSO\", or \"PSO\"."

--------------------------------------------------------------------------------
-- Property testing

-- | Automatically test a computation. In particular, look for
-- deadlocks, uncaught exceptions, and multiple return values.
--
-- This uses the 'Conc' monad for testing, which is an instance of
-- 'MonadConc'. If you need to test something which also uses
-- 'MonadIO', use 'testAutoIO'.
testAuto :: (Eq a, Show a)
  => (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> TestTree
testAuto = testAuto' defaultMemType

-- | Variant of 'testAuto' which tests a computation under a given
-- memory model.
testAuto' :: (Eq a, Show a)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> TestTree
testAuto' memtype conc = testDejafus' memtype defaultBounds conc autocheckCases

-- | Variant of 'testAuto' for computations which do 'IO'.
testAutoIO :: (Eq a, Show a) => Conc.ConcIO a -> TestTree
testAutoIO = testAutoIO' defaultMemType

-- | Variant of 'testAuto'' for computations which do 'IO'.
testAutoIO' :: (Eq a, Show a) => MemType -> Conc.ConcIO a -> TestTree
testAutoIO' memtype concio = testDejafusIO' memtype defaultBounds  concio autocheckCases

-- | Predicates for the various autocheck functions.
autocheckCases :: Eq a => [(TestName, Predicate a)]
autocheckCases =
  [("Never Deadlocks", representative deadlocksNever)
  , ("No Exceptions", representative exceptionsNever)
  , ("Consistent Result", alwaysSame)
  ]

-- | Check that a predicate holds.
testDejafu :: Show a
  => (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> TestName
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> TestTree
testDejafu = testDejafu' defaultMemType defaultBounds

-- | Variant of 'testDejafu' which takes a memory model and
-- pre-emption bound.
testDejafu' :: Show a
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bounds.
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> TestName
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> TestTree
testDejafu' memtype cb conc name p = testDejafus' memtype cb conc [(name, p)]

-- | Variant of 'testDejafu' which takes a collection of predicates to
-- test. This will share work between the predicates, rather than
-- running the concurrent computation many times for each predicate.
testDejafus :: Show a
  => (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> [(TestName, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> TestTree
testDejafus = testDejafus' defaultMemType defaultBounds

-- | Variant of 'testDejafus' which uses performs incomplete testing
-- using 'unsystematicRandom'.
testUnsystematicRandom :: (Show a, RandomGen g)
  => Int
  -- ^ Execution limit.
  -> g
  -- ^ Random number generator
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> [(TestName, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> TestTree
testUnsystematicRandom = testUnsystematicRandom' defaultMemType defaultBounds

-- | Variant of 'testDejafus' which uses performs incomplete testing
-- using 'unsystematicRandom'.
testUnsystematicPCT :: (Show a, RandomGen g)
  => Int
  -- ^ Execution limit.
  -> g
  -- ^ Random number generator
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> [(TestName, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> TestTree
testUnsystematicPCT = testUnsystematicPCT' defaultMemType defaultBounds

-- | Variant of 'testDejafus' which takes a memory model and pre-emption
-- bound.
testDejafus' :: Show a
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bounds.
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> [(TestName, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> TestTree
testDejafus' memty bs conc tests =
  testPredicates tests (pure $ sctBoundST memty bs conc)

-- | Variant of 'testUnsystematicRandom' which takes a memory model
-- and schedule bounds.
testUnsystematicRandom' :: (Show a, RandomGen g)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bounds
  -> Int
  -- ^ Execution limit.
  -> g
  -- ^ Random number generator
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> [(TestName, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> TestTree
testUnsystematicRandom' memty bs lim gen conc tests =
  testPredicates tests . pure $ runST $
    SCT.unsystematicRandom lim gen memty (SCT.cBound bs) conc

-- | Variant of 'testUnsystematicPCT' which takes a memory model and
-- schedule bounds.
testUnsystematicPCT' :: (Show a, RandomGen g)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bounds
  -> Int
  -- ^ Execution limit.
  -> g
  -- ^ Random number generator
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> [(TestName, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> TestTree
testUnsystematicPCT' memty bs lim gen conc tests =
  testPredicates tests . pure $ runST $
    SCT.unsystematicPCT lim gen memty (SCT.cBound bs) conc

-- | Variant of 'testDejafu' for computations which do 'IO'.
testDejafuIO :: Show a => Conc.ConcIO a -> TestName -> Predicate a -> TestTree
testDejafuIO = testDejafuIO' defaultMemType defaultBounds

-- | Variant of 'testDejafu'' for computations which do 'IO'.
testDejafuIO' :: Show a => MemType -> Bounds -> Conc.ConcIO a -> TestName -> Predicate a -> TestTree
testDejafuIO' memtype cb concio name p = testDejafusIO' memtype cb concio [(name, p)]

-- | Variant of 'testDejafus' for computations which do 'IO'.
testDejafusIO :: Show a => Conc.ConcIO a -> [(TestName, Predicate a)] -> TestTree
testDejafusIO = testDejafusIO' defaultMemType defaultBounds

-- | Variant of 'testUnsystematicRandom' for computations which do
-- 'IO'.
testUnsystematicRandomIO :: (Show a, RandomGen g) => Int -> g -> Conc.ConcIO a -> [(TestName, Predicate a)] -> TestTree
testUnsystematicRandomIO = testUnsystematicRandomIO' defaultMemType defaultBounds

-- | Variant of 'testUnsystematicPCT' for computations which do 'IO'.
testUnsystematicPCTIO :: (Show a, RandomGen g) => Int -> g -> Conc.ConcIO a -> [(TestName, Predicate a)] -> TestTree
testUnsystematicPCTIO = testUnsystematicPCTIO' defaultMemType defaultBounds

-- | Variant of 'dejafus'' for computations which do 'IO'.
testDejafusIO' :: Show a => MemType -> Bounds -> Conc.ConcIO a -> [(TestName, Predicate a)] -> TestTree
testDejafusIO' memty bs conc tests =
  testPredicates tests (sctBoundIO memty bs conc)

-- | Variant of 'testUnsystematicRandom'' for computations which do
-- 'IO'.
testUnsystematicRandomIO' :: (Show a, RandomGen g) => MemType -> Bounds -> Int -> g -> Conc.ConcIO a -> [(TestName, Predicate a)] -> TestTree
testUnsystematicRandomIO' memty bs lim gen conc tests =
  testPredicates tests $
    SCT.unsystematicRandom lim gen memty (SCT.cBound bs) conc

-- | Variant of 'testUnsystematicPCT'' for computations which do 'IO'.
testUnsystematicPCTIO' :: (Show a, RandomGen g) => MemType -> Bounds -> Int -> g -> Conc.ConcIO a -> [(TestName, Predicate a)] -> TestTree
testUnsystematicPCTIO' memty bs lim gen conc tests =
  testPredicates tests $
    SCT.unsystematicPCT lim gen memty (SCT.cBound bs) conc

--------------------------------------------------------------------------------
-- Building blocks

data ConcTest where
  ConcTest   :: Show a => IO [(Either Failure a, Trc)] -> Predicate a -> ConcTest
  deriving Typeable

instance IsTest ConcTest where
  testOptions = return []

  run _ (ConcTest iotraces p) _ = do
    traces <- iotraces
    let err = showErr $ p traces
    pure $ if null err then testPassed "" else testFailed err

-- | Turn a collection of predicates into a test which will fail if
-- any of the provided results don't pass.
testPredicates :: Show a
  => [(TestName, Predicate a)]
  -- ^ Predicates to check.
  -> IO [(Either Failure a,  Conc.Trace Conc.ThreadId Conc.ThreadAction Conc.Lookahead)]
  -- ^ Results. These are in 'IO' to make it easier to use for both
  -- the 'ST' and 'IO' runners.
  -> TestTree
testPredicates tests iotraces = case map toTest tests of
  [t] -> t
  ts  -> testGroup "Deja Fu Tests" ts

  where
    toTest (name, p) = singleTest name $ ConcTest iotraces p
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

{-# LANGUAGE CPP #-}
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
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : CPP, FlexibleInstances, ImpredicativeTypes, RankNTypes, ScopedTypeVariables, TypeSynonymInstances
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
  -- These instances use the default memory model and schedule bounds.

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

import Control.Monad.Catch (try)
import Control.Monad.ST (runST)
import Data.List (intercalate, intersperse)
import System.Random (RandomGen)
import Test.DejaFu
import qualified Test.DejaFu.SCT as SCT
import Test.HUnit (Assertable(..), Test(..), Testable(..), assertString)
import Test.HUnit.Lang (HUnitFailure(..))

import qualified Test.DejaFu.Conc as Conc

-- Can't put the necessary forall in the @Assertable Conc.ConcST t@
-- instance :(
import Unsafe.Coerce (unsafeCoerce)

type Trc = Conc.Trace Conc.ThreadId Conc.ThreadAction Conc.Lookahead

sctBoundST :: MemType -> Bounds -> (forall t. Conc.ConcST t a) -> [(Either Failure a, Trc)]
sctBoundST memtype cb conc = runST (SCT.sctBound memtype cb conc)

sctBoundIO :: MemType -> Bounds -> Conc.ConcIO a -> IO [(Either Failure a, Trc)]
sctBoundIO = SCT.sctBound

--------------------------------------------------------------------------------
-- Unit testing

instance Testable (Conc.ConcST t ()) where
  test conc = TestCase (assert conc)

instance Testable (Conc.ConcIO ()) where
  test conc = TestCase (assert conc)

instance Assertable (Conc.ConcST t ()) where
  assert conc = do
    let traces = sctBound' conc'
    assertString . showErr $ assertableP traces

    where
      conc' :: Conc.ConcST t (Either HUnitFailure ())
      conc' = try conc

      sctBound' :: Conc.ConcST t (Either HUnitFailure ()) -> [(Either Failure (Either HUnitFailure ()), Trc)]
      sctBound' = unsafeCoerce $ sctBoundST defaultMemType defaultBounds

instance Assertable (Conc.ConcIO ()) where
  assert conc = do
    traces <- sctBoundIO defaultMemType defaultBounds (try conc)
    assertString . showErr $ assertableP traces

assertableP :: Predicate (Either HUnitFailure ())
assertableP = alwaysTrue $ \r -> case r of
  Right (Left (HUnitFailure {})) -> False
  _ -> True

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
  -> Test
testAuto = testAuto' defaultMemType

-- | Variant of 'testAuto' which tests a computation under a given
-- memory model.
testAuto' :: (Eq a, Show a)
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> Test
testAuto' memtype conc = testDejafus' memtype defaultBounds conc autocheckCases

-- | Variant of 'testAuto' for computations which do 'IO'.
testAutoIO :: (Eq a, Show a) => Conc.ConcIO a -> Test
testAutoIO = testAutoIO' defaultMemType

-- | Variant of 'testAuto'' for computations which do 'IO'.
testAutoIO' :: (Eq a, Show a) => MemType -> Conc.ConcIO a -> Test
testAutoIO' memtype concio = testDejafusIO' memtype defaultBounds concio autocheckCases

-- | Predicates for the various autocheck functions.
autocheckCases :: Eq a => [(String, Predicate a)]
autocheckCases =
  [("Never Deadlocks", representative deadlocksNever)
  , ("No Exceptions", representative exceptionsNever)
  , ("Consistent Result", alwaysSame)
  ]

-- | Check that a predicate holds.
testDejafu :: Show a
  => (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> String
  -- ^ The name of the test.
  -> Predicate a
  -- ^ The predicate to check
  -> Test
testDejafu = testDejafu' defaultMemType defaultBounds

-- | Variant of 'testDejafu' which takes a memory model and
-- schedule bounds.
testDejafu' :: Show a
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bound.
  -> (forall t. Conc.ConcST t a)
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
  => (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> Test
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
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> Test
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
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> Test
testUnsystematicPCT = testUnsystematicPCT' defaultMemType defaultBounds

-- | Variant of 'testDejafus' which takes a memory model and schedule
-- bounds.
testDejafus' :: Show a
  => MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> Bounds
  -- ^ The schedule bounds
  -> (forall t. Conc.ConcST t a)
  -- ^ The computation to test
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> Test
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
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> Test
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
  -> [(String, Predicate a)]
  -- ^ The list of predicates (with names) to check
  -> Test
testUnsystematicPCT' memty bs lim gen conc tests =
  testPredicates tests . pure $ runST $
    SCT.unsystematicPCT lim gen memty (SCT.cBound bs) conc

-- | Variant of 'testDejafu' for computations which do 'IO'.
testDejafuIO :: Show a => Conc.ConcIO a -> String -> Predicate a -> Test
testDejafuIO = testDejafuIO' defaultMemType defaultBounds

-- | Variant of 'testDejafu'' for computations which do 'IO'.
testDejafuIO' :: Show a => MemType -> Bounds -> Conc.ConcIO a -> String -> Predicate a -> Test
testDejafuIO' memtype cb concio name p = testDejafusIO' memtype cb concio [(name, p)]

-- | Variant of 'testDejafus' for computations which do 'IO'.
testDejafusIO :: Show a => Conc.ConcIO a -> [(String, Predicate a)] -> Test
testDejafusIO = testDejafusIO' defaultMemType defaultBounds

-- | Variant of 'testUnsystematicRandom' for computations which do
-- 'IO'.
testUnsystematicRandomIO :: (Show a, RandomGen g) => Int -> g -> Conc.ConcIO a -> [(String, Predicate a)] -> Test
testUnsystematicRandomIO = testUnsystematicRandomIO' defaultMemType defaultBounds

-- | Variant of 'testUnsystematicPCT' for computations which do 'IO'.
testUnsystematicPCTIO :: (Show a, RandomGen g) => Int -> g -> Conc.ConcIO a -> [(String, Predicate a)] -> Test
testUnsystematicPCTIO = testUnsystematicPCTIO' defaultMemType defaultBounds

-- | Variant of 'dejafus'' for computations which do 'IO'.
testDejafusIO' :: Show a => MemType -> Bounds -> Conc.ConcIO a -> [(String, Predicate a)] -> Test
testDejafusIO' memty bs conc tests =
  testPredicates tests (sctBoundIO memty bs conc)

-- | Variant of 'testUnsystematicRandom'' for computations which do
-- 'IO'.
testUnsystematicRandomIO' :: (Show a, RandomGen g) => MemType -> Bounds -> Int -> g -> Conc.ConcIO a -> [(String, Predicate a)] -> Test
testUnsystematicRandomIO' memty bs lim gen conc tests =
  testPredicates tests $
    SCT.unsystematicRandom lim gen memty (SCT.cBound bs) conc

-- | Variant of 'testUnsystematicPCT'' for computations which do 'IO'.
testUnsystematicPCTIO' :: (Show a, RandomGen g) => MemType -> Bounds -> Int -> g -> Conc.ConcIO a -> [(String, Predicate a)] -> Test
testUnsystematicPCTIO' memty bs lim gen conc tests =
  testPredicates tests $
    SCT.unsystematicPCT lim gen memty (SCT.cBound bs) conc

--------------------------------------------------------------------------------
-- Building blocks

-- | Turn a collection of predicates into a test which will fail if
-- any of the provided results don't pass.
testPredicates :: Show a
  => [(String, Predicate a)]
  -- ^ Predicates to check.
  -> IO [(Either Failure a,  Conc.Trace Conc.ThreadId Conc.ThreadAction Conc.Lookahead)]
  -- ^ Results. These are in 'IO' because running an HUnit test is an
  -- 'IO' action anyway, and so this avoids awkwardness that would
  -- arise if it were pure.
  -> Test
testPredicates tests iotraces = case map toTest tests of
  [t] -> t
  ts  -> TestList ts

  where
    toTest (name, p) = TestLabel name . TestCase $ do
      traces <- iotraces
      assertString . showErr $ p traces

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

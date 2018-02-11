{-# LANGUAGE GADTs #-}

module Common
  ( module Common
  , TF.Test
  ) where

import Control.Exception (ArithException, ArrayException, SomeException)
import Control.Monad (void)
import qualified Control.Monad.Catch as C
import Control.Monad.Conc.Class
import Control.Monad.STM.Class
import System.Random (mkStdGen)
import Test.DejaFu (Predicate, ProPredicate(..), Failure, Result(..), Way, alwaysTrue)
import Test.DejaFu.Conc (ConcIO)
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import qualified Test.HUnit as TH
import Test.HUnit.DejaFu (Bounds, defaultBounds, defaultMemType, uniformly, randomly, swarmy, systematically, testDejafu, testDejafuWay)
import qualified Test.LeanCheck as LeanCheck

-------------------------------------------------------------------------------
-- Tests

class IsTest t where
  toTestList :: t -> [TF.Test]

instance IsTest TF.Test where
  toTestList t = [t]

instance IsTest TH.Test where
  toTestList = hUnitTestToTests

instance IsTest T where
  toTestList (T n c p) = toTestList (BT n c p defaultBounds)
  toTestList (W n c p w) = toTestList . testGroup n $
    [ testDejafuWay w defaultMemType "(way)" p c ]
  toTestList (BT n c p b) = toTestList . testGroup n $
    let mk way name = testDejafuWay way defaultMemType name p c
        g = mkStdGen 0
    in [ mk (systematically b) "systematically"
       , mk (uniformly g 100) "uniformly"
       , mk (randomly  g 100) "randomly"
       , mk (swarmy g 100 10) "swarmy"
       ]

instance IsTest t => IsTest [t] where
  toTestList = concatMap toTestList

data T where
  T  :: Show a => String -> ConcIO a -> Predicate a -> T
  W  :: Show a => String -> ConcIO a -> Predicate a -> Way -> T
  BT :: Show a => String -> ConcIO a -> Predicate a -> Bounds -> T

testGroup :: IsTest t => String -> t -> TF.Test
testGroup name = TF.testGroup name . toTestList

djfu :: Show a => String -> Predicate a -> ConcIO a -> TF.Test
djfu name p c = hunitTest $ testDejafu name p c

djfuT :: Show a => String -> Predicate a -> ConcIO a -> [TF.Test]
djfuT name p c = toTestList $ T name c p

alwaysFailsWith :: (Failure -> Bool) -> Predicate a
alwaysFailsWith p = alwaysTrue (either p (const False))

leancheck :: LeanCheck.Testable a => String -> a -> TF.Test
leancheck name = hunitTest . TH.TestLabel name . lcheck . LeanCheck.counterExamples 2500 where
  lcheck = TH.TestCase . TH.assertString . unlines . map showf
  showf xs = "Failed for " ++ unwords xs

hunitTest :: TH.Test -> TF.Test
hunitTest = head . toTestList

-------------------------------------------------------------------------------
-- Exceptions

catchArithException :: C.MonadCatch m => m a -> (ArithException -> m a) -> m a
catchArithException = C.catch

catchArrayException :: C.MonadCatch m => m a -> (ArrayException -> m a) -> m a
catchArrayException = C.catch

catchSomeException :: C.MonadCatch m => m a -> (SomeException -> m a) -> m a
catchSomeException = C.catch

-------------------------------------------------------------------------------
-- Utilities

(|||) :: MonadConc m => m a -> m b -> m ()
a ||| b = do
  j <- spawn a
  void b
  void (readMVar j)

-- | Create an empty monomorphic @MVar@.
newEmptyMVarInt :: MonadConc m => m (MVar m Int)
newEmptyMVarInt = newEmptyMVar

-- | Create a full monomorphic @MVar@.
newMVarInt :: MonadConc m => Int -> m (MVar m Int)
newMVarInt = newMVar

-- | Create a monomorphic @CRef@.
newCRefInt :: MonadConc m => Int -> m (CRef m Int)
newCRefInt = newCRef

-- | Create a monomorphic @TVar@.
newTVarInt :: MonadSTM stm => Int -> stm (TVar stm Int)
newTVarInt = newTVar

-- | A test which should fail.
failing :: Predicate a -> Predicate a
failing p = p
  { peval = \xs ->
      let result = peval p xs
      in result { _pass = not (_pass result) }
  }

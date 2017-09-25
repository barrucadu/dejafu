{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

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
import Test.DejaFu (Predicate, Result(..))
import Test.DejaFu.Conc (ConcST)
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import qualified Test.HUnit as TH
import Test.HUnit.DejaFu (Bounds, defaultBounds, defaultMemType, uniformly, randomly, swarmy, systematically, testDejafu, testDejafuWay)

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
  toTestList (BT n c p b) = toTestList . testGroup n $
    let mk way name = testDejafuWay way defaultMemType c name p
        g = mkStdGen 0
    in [ mk (systematically b) "systematically"
       , mk (uniformly g 100) "uniformly"
       , mk (randomly  g 100) "randomly"
       , mk (swarmy g 100 10) "swarmy"
       ]

instance IsTest t => IsTest [t] where
  toTestList = concatMap toTestList

data T where
  T  :: Show a => String -> (forall t. ConcST t a) -> Predicate a -> T
  BT :: Show a => String -> (forall t. ConcST t a) -> Predicate a -> Bounds -> T

testGroup :: IsTest t => String -> t -> TF.Test
testGroup name = TF.testGroup name . toTestList

djfu :: Show a => String -> Predicate a -> (forall t. ConcST t a) -> TF.Test
djfu name p c = head . toTestList $ testDejafu c name p

djfuT :: Show a => String -> Predicate a -> (forall t. ConcST t a) -> [TF.Test]
djfuT name p c = toTestList $ T name c p

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
failing p as =
  let result = p as
  in result { _pass = not (_pass result) }

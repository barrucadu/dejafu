{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Utils where

import Control.Exception (ArithException, ArrayException, SomeException)
import Control.Monad (void)
import qualified Control.Monad.Catch as C
import Control.Monad.Conc.Class (MonadConc, readMVar, spawn)
import System.Random (mkStdGen)
import Test.DejaFu (Predicate)
import Test.DejaFu.Conc (ConcST)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (test)
import Test.HUnit.DejaFu (Bounds, defaultBounds, defaultMemType, uniformly, randomly, swarmy, systematically, testDejafuWay)

-- | Wrap up a test
data T where
  T  :: Show a => String -> (forall t. ConcST t a) -> Predicate a -> T
  BT :: Show a => String -> (forall t. ConcST t a) -> Predicate a -> Bounds -> T

-- | Run a test group with different execution ways.
tg :: String -> [T] -> Test
tg name ts = testGroup name
    [ testGroup "Systematic"  . hUnitTestToTests . test . useWay $ systematically
     , testGroup "Uniform"    . hUnitTestToTests . test . useWay . const $ uniformly (mkStdGen 0) 100
     , testGroup "Weighted"   . hUnitTestToTests . test . useWay . const $ randomly  (mkStdGen 0) 100
     , testGroup "Swarm (10)" . hUnitTestToTests . test . useWay . const $ swarmy    (mkStdGen 0) 100 10
     ]
  where
    useWay wayf = map (go wayf) ts
    go wayf (T  n c p)   = go wayf (BT n c p defaultBounds)
    go wayf (BT n c p b) = testDejafuWay (wayf b) defaultMemType c n p

catchArithException :: C.MonadCatch m => m a -> (ArithException -> m a) -> m a
catchArithException = C.catch

catchArrayException :: C.MonadCatch m => m a -> (ArrayException -> m a) -> m a
catchArrayException = C.catch

catchSomeException :: C.MonadCatch m => m a -> (SomeException -> m a) -> m a
catchSomeException = C.catch

(|||) :: MonadConc m => m a -> m b -> m ()
a ||| b = do
  j <- spawn a
  void b
  void (readMVar j)

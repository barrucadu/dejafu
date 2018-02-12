{-# LANGUAGE GADTs #-}

module Common (module Common, module Test.Tasty.DejaFu, T.TestTree) where

import Control.Exception (ArithException, ArrayException, SomeException)
import Control.Monad (void)
import qualified Control.Monad.Catch as C
import Control.Monad.Conc.Class
import Control.Monad.STM.Class
import System.Random (mkStdGen)
import Test.DejaFu (Predicate, ProPredicate(..), Failure, Result(..), Way, alwaysTrue)
import Test.DejaFu.Conc (ConcIO)
import qualified Test.Tasty as T
import Test.Tasty.DejaFu hiding (testProperty)
import qualified Test.Tasty.LeanCheck as T

-------------------------------------------------------------------------------
-- Tests

class IsTest t where
  toTestList :: t -> [T.TestTree]

instance IsTest T.TestTree where
  toTestList t = [t]

instance IsTest T where
  toTestList (T n c p) = toTestList (TEST n c p defaultWays)
  toTestList (W n c p w) = toTestList (TEST n c p [w])
  toTestList (B n c p b) = toTestList (TEST n c p (defaultWaysFor b))
  toTestList (TEST n c p w) = toTestList . testGroup n $
    let mk (name, way) = testDejafuWay way defaultMemType name p c
    in map mk w

instance IsTest t => IsTest [t] where
  toTestList = concatMap toTestList

data T where
  T :: Show a => String -> ConcIO a -> Predicate a -> T
  W :: Show a => String -> ConcIO a -> Predicate a -> (String, Way) -> T
  B :: Show a => String -> ConcIO a -> Predicate a -> Bounds -> T
  TEST :: Show a => String -> ConcIO a -> Predicate a -> [(String, Way)] -> T

defaultWays :: [(String, Way)]
defaultWays = defaultWaysFor defaultBounds

defaultWaysFor :: Bounds -> [(String, Way)]
defaultWaysFor b =
  [ ("systematically", systematically b)
  , ("uniformly", uniformly (mkStdGen 0) 100)
  , ("randomly", randomly (mkStdGen 0) 100)
  , ("swarmy", swarmy (mkStdGen 0) 100 10)
  ]

testGroup :: IsTest t => String -> t -> T.TestTree
testGroup name = T.testGroup name . toTestList

djfu :: Show a => String -> Predicate a -> ConcIO a -> T.TestTree
djfu name p c = testDejafu name p c

djfuT :: Show a => String -> Predicate a -> ConcIO a -> [T.TestTree]
djfuT name p c = toTestList $ T name c p

alwaysFailsWith :: (Failure -> Bool) -> Predicate a
alwaysFailsWith p = alwaysTrue (either p (const False))

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

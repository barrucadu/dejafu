{-# LANGUAGE GADTs #-}

module Common (module Common, module Test.Tasty.DejaFu, T.TestTree, T.expectFail) where

import           Control.Exception             (ArithException, ArrayException,
                                                SomeException, displayException)
import           Control.Monad                 (void)
import qualified Control.Monad.Catch           as C
import           Control.Monad.Conc.Class
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.STM.Class
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Hedgehog                      as H
import qualified Hedgehog.Gen                  as HGen
import qualified Hedgehog.Range                as HRange
import           System.Random                 (mkStdGen)
import           Test.DejaFu                   (Failure, Predicate,
                                                ProPredicate(..), Result(..),
                                                Way, alwaysTrue, somewhereTrue)
import           Test.DejaFu.Conc              (ConcIO, Scheduler(..),
                                                randomSched, runConcurrent)
import           Test.DejaFu.SCT.Internal.DPOR
import           Test.DejaFu.Types
import           Test.DejaFu.Utils
import qualified Test.Tasty                    as T
import           Test.Tasty.DejaFu             hiding (testProperty)
import qualified Test.Tasty.ExpectedFailure    as T
import qualified Test.Tasty.Hedgehog           as H

-------------------------------------------------------------------------------
-- Tests

class IsTest t where
  toTestList :: t -> [T.TestTree]

instance IsTest T.TestTree where
  toTestList t = [t]

instance IsTest T where
  toTestList (T n c p) = toTestList (TEST n c p defaultWays True)
  toTestList (W n c p w) = toTestList (TEST n c p [w] True)
  toTestList (B n c p b) = toTestList (TEST n c p (defaultWaysFor b) True)
  toTestList (TEST n c p w subc) = toTestList . testGroup n $
    let mk (name, way) = testDejafuWay way defaultMemType name p c
    in map mk w ++ [H.testProperty "dependency func." (prop_dep_fun c) | subc]

instance IsTest t => IsTest [t] where
  toTestList = concatMap toTestList

data T where
  T :: (Eq a, Show a) => String -> ConcIO a -> Predicate a -> T
  W :: (Eq a, Show a) => String -> ConcIO a -> Predicate a -> (String, Way) -> T
  B :: (Eq a, Show a) => String -> ConcIO a -> Predicate a -> Bounds -> T
  TEST :: (Eq a, Show a) => String -> ConcIO a -> Predicate a -> [(String, Way)] -> Bool -> T

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

djfu :: (Eq a, Show a) => String -> Predicate a -> ConcIO a -> [T.TestTree]
djfu name p c = toTestList $ W name c p ("systematically", systematically defaultBounds)

djfuS :: (Eq a, Show a) => String -> Predicate a -> ConcIO a -> [T.TestTree]
djfuS name p c = toTestList $ TEST name c p [("systematically", systematically defaultBounds)] False

djfuT :: (Eq a, Show a) => String -> Predicate a -> ConcIO a -> [T.TestTree]
djfuT name p c = toTestList $ T name c p

djfuTS :: (Eq a, Show a) => String -> Predicate a -> ConcIO a -> [T.TestTree]
djfuTS name p c = toTestList $ TEST name c p defaultWays False

alwaysFailsWith :: (Failure -> Bool) -> Predicate a
alwaysFailsWith p = alwaysTrue (either p (const False))

sometimesFailsWith :: (Failure -> Bool) -> Predicate a
sometimesFailsWith p = somewhereTrue (either p (const False))

testProperty :: String -> H.PropertyT IO () -> T.TestTree
testProperty name = H.testProperty name . H.property

-------------------------------------------------------------------------------
-- Dependency function

-- https://memo.barrucadu.co.uk/hedgehog-dejafu.html

-- | Check that the independence function correctly decides
-- commutativity for this program.
prop_dep_fun :: (Eq a, Show a) => ConcIO a -> H.Property
prop_dep_fun conc = H.property $ do
    mem <- H.forAll HGen.enumBounded
    seed <- H.forAll genInt
    fs <- H.forAll $ genList HGen.bool

    (efa1, tids1, efa2, tids2) <- liftIO $ runNorm seed (shuffle fs) mem
    H.footnote ("            to: " ++ show tids2)
    H.footnote ("rewritten from: " ++ show tids1)
    efa1 H.=== efa2
  where
    shuffle = go initialDepState where
      go ds (f:fs) (t1@(tid1, ta1):t2@(tid2, ta2):trc)
        | independent ds tid1 ta1 tid2 ta2 && f = go' ds fs t2 (t1 : trc)
        | otherwise = go' ds fs t1 (t2 : trc)
      go _ _ trc = trc

      go' ds fs t@(tid, ta) trc = t : go (updateDepState ds tid ta) fs trc

    runNorm seed norm memtype = do
      let g = mkStdGen seed
      (efa1, _, trc1) <- runConcurrent randomSched memtype g conc
      let tids1 = toTIdTrace trc1
      (efa2, _, trc2) <- play memtype (norm tids1) conc
      let tids2 = toTIdTrace trc2
      pure (efa1, map fst tids1, efa2, map fst tids2)

    play = runConcurrent (Scheduler sched) where
      sched prior runnable ((t, Stop):ts)
        | any ((==t) . fst) runnable = (Just t, ts)
        | otherwise = sched prior runnable ts
      sched _ _ ((t, _):ts) = (Just t, ts)
      sched _ _ _ = (Nothing, [])

    toTIdTrace =
      tail . scanl (\(t, _) (d, _, a) -> (tidOf t d, a)) (initialThread, undefined)

-------------------------------------------------------------------------------
-- Exceptions

instance Eq SomeException where
  e1 == e2 = displayException e1 == displayException e2

catchArithException :: C.MonadCatch m => m a -> (ArithException -> m a) -> m a
catchArithException = C.catch

catchArrayException :: C.MonadCatch m => m a -> (ArrayException -> m a) -> m a
catchArrayException = C.catch

catchSomeException :: C.MonadCatch m => m a -> (SomeException -> m a) -> m a
catchSomeException = C.catch

-------------------------------------------------------------------------------
-- Generators

genSmallInt :: H.Gen Int
genSmallInt = genIntFromTo 0 10

genInt :: H.Gen Int
genInt = genIntFromTo 0 100

genIntFromTo :: Int -> Int -> H.Gen Int
genIntFromTo from = HGen.int . HRange.linear from

genMap :: Ord k => H.Gen k -> H.Gen v -> H.Gen (Map.Map k v)
genMap genKey genVal = HGen.map (HRange.linear 0 100) ((,) <$> genKey <*> genVal)

genSmallMap :: Ord k => H.Gen k -> H.Gen v -> H.Gen (Map.Map k v)
genSmallMap genKey genVal = HGen.map (HRange.linear 0 10) ((,) <$> genKey <*> genVal)

genSet :: Ord a => H.Gen a -> H.Gen (Set.Set a)
genSet = HGen.set (HRange.linear 0 100)

genSmallSet :: Ord a => H.Gen a -> H.Gen (Set.Set a)
genSmallSet = HGen.set (HRange.linear 0 10)

genString :: H.Gen String
genString = genSmallList HGen.enumBounded

genList :: H.Gen a -> H.Gen [a]
genList = genListUpTo 100

genSmallList :: H.Gen a -> H.Gen [a]
genSmallList = genListUpTo 10

genListUpTo :: Int -> H.Gen a -> H.Gen [a]
genListUpTo = HGen.list . HRange.linear 0

type Function k v = (v, Map.Map k v)

genFunction :: Ord k => H.Gen k -> H.Gen v -> H.Gen (Function k v)
genFunction genKey genVal = (,) <$> genVal <*> genSmallMap genKey genVal

applyFunction :: Ord k => (v, Map.Map k v) -> k -> v
applyFunction (def, assocs) k = Map.findWithDefault def k assocs

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

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GADTs #-}

module Common (module Common, module Test.Tasty.DejaFu, T.TestTree, T.expectFail) where

import           Control.Arrow              (second)
import           Control.Exception          (ArithException, ArrayException,
                                             SomeException, displayException)
import           Control.Monad              (unless, void)
import qualified Control.Monad.Catch        as C
import           Control.Monad.Conc.Class
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.STM.Class
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Hedgehog                   as H
import qualified Hedgehog.Gen               as HGen
import qualified Hedgehog.Range             as HRange
import           System.Random              (mkStdGen)
import           Test.DejaFu                (Condition, Predicate,
                                             ProPredicate(..), Result(..), Way,
                                             alwaysTrue, somewhereTrue)
import           Test.DejaFu.Conc           (randomSched, runConcurrent)
import qualified Test.DejaFu.SCT            as SCT
import           Test.DejaFu.SCT.Internal
import           Test.DejaFu.Types
import           Test.DejaFu.Utils
import qualified Test.Tasty                 as T
import           Test.Tasty.DejaFu          hiding (testProperty)
import qualified Test.Tasty.ExpectedFailure as T
import qualified Test.Tasty.Hedgehog        as H
import qualified Test.Tasty.HUnit           as TH

-------------------------------------------------------------------------------
-- Tests

class IsTest t where
  toTestList :: t -> [T.TestTree]

instance IsTest T.TestTree where
  toTestList t = [t]

instance IsTest T where
  toTestList (T n c p) = toTestList (TEST n c p (map (second toSettings) defaultWays) True)
  toTestList (W n c p w) = toTestList (TEST n c p [second toSettings w] True)
  toTestList (B n c p b) = toTestList (TEST n c p (map (second toSettings) (defaultWaysFor b)) True)
  toTestList (TEST n c p ss subc) = toTestList (TEST' False n c p ss subc)
  toTestList (TEST' b n c p ss subc) = toTestList . testGroup n $
    let mk (name, settings) = testDejafuWithSettings (set lsafeIO b settings) name p c
    in map mk ss ++ [H.testProperty "dependency func." (prop_dep_fun b c) | subc]

instance IsTest t => IsTest [t] where
  toTestList = concatMap toTestList

data T where
  T :: (Program p, Eq a, Show a) => String -> p IO a -> Predicate a -> T
  W :: (Program p, Eq a, Show a) => String -> p IO a -> Predicate a -> (String, Way) -> T
  B :: (Program p, Eq a, Show a) => String -> p IO a -> Predicate a -> Bounds -> T
  TEST :: (Program p, Eq a, Show a) => String -> p IO a -> Predicate a -> [(String, Settings IO a)] -> Bool -> T
  TEST' :: (Program p, Eq a, Show a) => Bool -> String -> p IO a -> Predicate a -> [(String, Settings IO a)] -> Bool -> T

toSettings :: (Applicative f, Eq a, Show a) => Way -> Settings f a
toSettings w
  = set ldebugFatal True
  . set ldebugShow (Just show)
  . set lequality (Just (==))
  . set lsimplify True
  $ fromWayAndMemType w defaultMemType

defaultWays :: [(String, Way)]
defaultWays = defaultWaysFor defaultBounds

defaultWaysFor :: Bounds -> [(String, Way)]
defaultWaysFor b =
  [ ("systematically", systematically b)
  , ("uniformly", uniformly (mkStdGen 0) 100)
  , ("randomly", randomly (mkStdGen 0) 100)
  ]

testGroup :: IsTest t => String -> t -> T.TestTree
testGroup name = T.testGroup name . toTestList

djfu :: (Program p, Eq a, Show a) => String -> Predicate a -> p IO a -> [T.TestTree]
djfu name p c = toTestList $ W name c p ("systematically", systematically defaultBounds)

djfuS :: (Program p, Eq a, Show a) => String -> Predicate a -> p IO a -> [T.TestTree]
djfuS name p c = toTestList $ TEST name c p [("systematically", toSettings (systematically defaultBounds))] False

djfuT :: (Program p, Eq a, Show a) => String -> Predicate a -> p IO a -> [T.TestTree]
djfuT name p c = toTestList $ T name c p

djfuTS :: (Program p, Eq a, Show a) => String -> Predicate a -> p IO a -> [T.TestTree]
djfuTS name p c = toTestList $ TEST name c p (map (second toSettings) defaultWays) False

djfuE :: Program p => String -> Error -> p IO a -> [T.TestTree]
djfuE name e0 c = toTestList . TH.testCase name $ C.catch
    (SCT.runSCT defaultWay defaultMemType c >> TH.assertFailure msg)
    (\e -> unless (e == e0) $ TH.assertFailure (err e))
  where
    msg = "expected " ++ displayException e0
    err e = msg ++ " got " ++ displayException e

alwaysFailsWith :: (Condition -> Bool) -> Predicate a
alwaysFailsWith p = alwaysTrue (either p (const False))

sometimesFailsWith :: (Condition -> Bool) -> Predicate a
sometimesFailsWith p = somewhereTrue (either p (const False))

testProperty :: String -> H.PropertyT IO () -> T.TestTree
testProperty name = H.testProperty name . H.property

-------------------------------------------------------------------------------
-- Dependency function

-- https://memo.barrucadu.co.uk/hedgehog-dejafu.html

-- | Check that the independence function correctly decides
-- commutativity for this program.
prop_dep_fun :: (Program p, Eq a, Show a) => Bool -> p IO a -> H.Property
prop_dep_fun safeIO conc = H.property $ do
    mem <- H.forAll HGen.enumBounded
    seed <- H.forAll genInt
    fs <- H.forAll $ genList HGen.bool

    -- todo: 1 1 is not right if a snapshot is restored
    (efa1, tids1, efa2, tids2) <- liftIO $ runNorm
      seed
      (renumber mem 1 1 . permuteBy safeIO mem (map (\f _ _ -> f) fs))
      mem
    H.footnote ("            to: " ++ show tids2)
    H.footnote ("rewritten from: " ++ show tids1)
    efa1 H.=== efa2
  where
    runNorm seed norm memtype = do
      let g = mkStdGen seed
      (efa1, _, trc1) <- runConcurrent randomSched memtype g conc
      let tids1 = toTIdTrace trc1
      (efa2, _, trc2) <- replay (play memtype conc) (norm tids1)
      let tids2 = toTIdTrace trc2
      pure (efa1, map fst tids1, efa2, map fst tids2)

    play memtype c s g = runConcurrent s memtype g c

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

genPair :: H.Gen a -> H.Gen (a, a)
genPair g = (,) <$> g <*> g

genEither :: H.Gen l -> H.Gen r -> H.Gen (Either l r)
genEither l r = HGen.choice [Left <$> l, Right <$> r]

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

-- | Create a monomorphic @IORef@.
newIORefInt :: MonadConc m => Int -> m (IORef m Int)
newIORefInt = newIORef

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

{-# LANGUAGE RankNTypes #-}
module Unit.Properties where

import qualified Control.Exception                as E
import           Control.Monad                    (zipWithM)
import qualified Control.Monad.ST                 as ST
import qualified Data.Foldable                    as F
import qualified Data.Map                         as M
import qualified Data.Sequence                    as S
import qualified Data.Set                         as Set
import qualified Data.STRef                       as ST
import qualified Hedgehog                         as H
import qualified Hedgehog.Gen                     as HGen
import qualified Test.DejaFu.Conc.Internal.Common as D
import qualified Test.DejaFu.Conc.Internal.Memory as Mem
import qualified Test.DejaFu.Internal             as D
import qualified Test.DejaFu.SCT.Internal.DPOR    as SCT
import qualified Test.DejaFu.Types                as D
import qualified Test.Tasty.Hedgehog              as H

import           Common

tests :: [TestTree]
tests =
  [ testGroup "ClassLaw" classLawProps
  , testGroup "Common"   commonProps
  , testGroup "Memory"   memoryProps
  , testGroup "SCT"      sctProps
  ]

-------------------------------------------------------------------------------

classLawProps :: [TestTree]
classLawProps = toTestList
    [ testGroup "Id"      (eqord genId)
    , testGroup "Failure" (eqord genFailure)
    ]
  where
    eqord gen =
      [ H.testProperty "Reflexivity (==)" . H.property $ do
          x <- H.forAll gen
          x H.=== x

      , H.testProperty "Symmetry (==)" . H.property $ do
          x <- H.forAll gen
          y <- H.forAll gen
          (x == y) H.=== (y == x)

      , H.testProperty "Transitivity (==)" . H.property $ do
          x <- H.forAll gen
          y <- H.forAll (HGen.filter (==x) gen)
          z <- H.forAll (HGen.filter (==y) gen)
          x H.=== z

      , H.testProperty "Reflexivity (<=)" . H.property $ do
          x <- H.forAll gen
          H.assert (x <= x)

      , H.testProperty "Antisymmetry (<=)" . H.property $ do
          x <- H.forAll gen
          y <- H.forAll (HGen.filter (\y -> x <= y && y <= x) gen)
          x H.=== y

      , H.testProperty "Transitivity (<=)" . H.property $ do
          x <- H.forAll gen
          y <- H.forAll (HGen.filter (x<=) gen)
          z <- H.forAll (HGen.filter (y<=) gen)
          H.assert (x <= z)

      , H.testProperty "Eq / Ord Consistency" . H.property $ do
          x <- H.forAll gen
          y <- H.forAll (HGen.filter (==x) gen)
          H.assert (x <= y)
          H.assert (y <= x)
      ]

-------------------------------------------------------------------------------

commonProps :: [TestTree]
commonProps = toTestList
  [ H.testProperty "simplifyAction a == simplifyLookahead (rewind a)" . H.property $ do
      act <- H.forAll genThreadAction
      case D.rewind act of
        Just lh -> D.simplifyAction act H.=== D.simplifyLookahead lh
        Nothing -> H.discard

  , H.testProperty "isBarrier a ==> synchronises a r" . H.property $ do
      a <- H.forAll (HGen.filter D.isBarrier genActionType)
      r <- H.forAll genCRefId
      H.assert (D.synchronises a r)

  , H.testProperty "isCommit a r ==> synchronises a r" . H.property $ do
      a <- H.forAll genPartiallySynchronisedActionType
      case D.crefOf a of
        Just r -> H.assert (D.synchronises a r)
        _ -> H.discard
  ]

-------------------------------------------------------------------------------

memoryProps :: [TestTree]
memoryProps = toTestList
    [ H.testProperty "bufferWrite emptyBuffer k c a /= emptyBuffer" . H.property $ do
        k <- H.forAll genWBKey
        a <- H.forAll genInt
        res <- crefProp $ \cref -> do
          wb <- Mem.bufferWrite Mem.emptyBuffer k cref a
          wb `eqWB` Mem.emptyBuffer
        H.assert (not res)

    , H.testProperty "commitWrite emptyBuffer k == emptyBuffer" . H.property $ do
        k <- H.forAll genWBKey
        H.assert $ ST.runST $ do
          wb <- Mem.commitWrite Mem.emptyBuffer k
          wb `eqWB` Mem.emptyBuffer

    , H.testProperty "commitWrite (bufferWrite emptyBuffer k a) k == emptyBuffer" . H.property $ do
        k <- H.forAll genWBKey
        a <- H.forAll genInt
        res <- crefProp $ \cref -> do
          wb1 <- Mem.bufferWrite Mem.emptyBuffer k cref a
          wb2 <- Mem.commitWrite wb1 k
          wb2 `eqWB` Mem.emptyBuffer
        H.assert res

    , H.testProperty "Single buffered write/read from same thread" . H.property $ do
        k@(tid, _) <- H.forAll genWBKey
        a <- H.forAll genInt
        res <- crefProp $ \cref -> do
          _ <- Mem.bufferWrite Mem.emptyBuffer k cref a
          Mem.readCRef cref tid
        a H.=== res

    , H.testProperty "Overriding buffered write/read from same thread" . H.property $ do
        k@(tid, _) <- H.forAll genWBKey
        a1 <- H.forAll genInt
        a2 <- H.forAll genInt
        res <- crefProp $ \cref -> do
          _ <- Mem.bufferWrite Mem.emptyBuffer k cref a1
          _ <- Mem.bufferWrite Mem.emptyBuffer k cref a2
          Mem.readCRef cref tid
        a2 H.=== res

    , H.testProperty "Buffered write/read from different thread" . H.property $ do
        k1@(tid, _) <- H.forAll genWBKey
        k2 <- H.forAll (HGen.filter ((/=tid) . fst) genWBKey)
        a1 <- H.forAll genInt
        a2 <- H.forAll genInt
        res <- crefProp $ \cref -> do
          _ <- Mem.bufferWrite Mem.emptyBuffer k1 cref a1
          _ <- Mem.bufferWrite Mem.emptyBuffer k2 cref a2
          Mem.readCRef cref tid
        a1 H.=== res
    ]
  where
    crefProp
      :: (Monad m, Show a)
      => (forall s. D.CRef (ST.STRef s) Int -> ST.ST s a)
      -> H.PropertyT m a
    crefProp p = do
      crefId <- H.forAll genCRefId
      pure $ ST.runST $ do
        cref <- makeCRef crefId
        p cref

-------------------------------------------------------------------------------

sctProps :: [TestTree]
sctProps = toTestList
  [ H.testProperty "canInterrupt ==> canInterruptL" . H.property $ do
      ds <- H.forAll genDepState
      tid <- H.forAll genThreadId
      act <- H.forAll (HGen.filter (SCT.canInterrupt ds tid) genThreadAction)
      case D.rewind act of
        Just lh -> H.assert (SCT.canInterruptL ds tid lh)
        Nothing -> H.discard

  , H.testProperty "dependent ==> dependent'" . H.property $ do
      ds <- H.forAll genDepState
      tid1 <- H.forAll genThreadId
      tid2 <- H.forAll genThreadId
      ta1 <- H.forAll genThreadAction
      ta2 <- H.forAll (HGen.filter (SCT.dependent ds tid1 ta1 tid2) genThreadAction)
      case D.rewind ta2 of
        Just lh -> H.assert (SCT.dependent' ds tid1 ta1 tid2 lh)
        Nothing -> H.discard

  , H.testProperty "dependent x y == dependent y x" . H.property $ do
      ds <- H.forAll genDepState
      tid1 <- H.forAll genThreadId
      tid2 <- H.forAll genThreadId
      ta1 <- H.forAll genThreadAction
      ta2 <- H.forAll genThreadAction
      SCT.dependent ds tid1 ta1 tid2 ta2 H.=== SCT.dependent ds tid2 ta2 tid1 ta1

  , H.testProperty "dependentActions x y == dependentActions y x" . H.property $ do
      ds <- H.forAll genDepState
      a1 <- H.forAll genActionType
      a2 <- H.forAll genActionType
      SCT.dependentActions ds a1 a2 H.=== SCT.dependentActions ds a2 a1
  ]

-------------------------------------------------------------------------------
-- Utils

makeCRef :: D.CRefId -> ST.ST t (D.CRef (ST.STRef t) Int)
makeCRef crid = D.CRef crid <$> ST.newSTRef (M.empty, 0, 42)

-- equality for writebuffers is a little tricky as we can't directly
-- compare the buffered values, so we compare everything else:
--  - the sets of nonempty buffers must be equal
--  - each pair of buffers for the same key must have the same size
--  - each pair of buffers for the same key must have an equal sequence of writes
--
-- individual writes are compared like so:
--  - the threadid and crefid must be the same
--  - the cache and number of writes inside the ref must be the same
eqWB :: Mem.WriteBuffer (ST.STRef t) -> Mem.WriteBuffer (ST.STRef t) -> ST.ST t Bool
eqWB (Mem.WriteBuffer wb1) (Mem.WriteBuffer wb2) = andM (pure (ks1 == ks2) :
    [ (&&) (S.length ws1 == S.length ws2) <$> (and <$> zipWithM eqBW (F.toList ws1) (F.toList ws2))
    | k <- ks1
    , let (Just ws1) = M.lookup k wb1
    , let (Just ws2) = M.lookup k wb2
    ])
  where
    ks1 = M.keys $ M.filter (not . S.null) wb1
    ks2 = M.keys $ M.filter (not . S.null) wb2

    eqBW (Mem.BufferedWrite t1 (D.CRef crid1 ref1) _) (Mem.BufferedWrite t2 (D.CRef crid2 ref2) _) = do
      d1 <- (\(m,i,_) -> (M.keys m, i)) <$> ST.readSTRef ref1
      d2 <- (\(m,i,_) -> (M.keys m, i)) <$> ST.readSTRef ref2
      pure (t1 == t2 && crid1 == crid2 && d1 == d2)

    andM [] = pure True
    andM (p:ps) = do
      q <- p
      if q then andM ps else pure False

(==>) :: Monad m => Bool -> H.PropertyT m a -> H.PropertyT m a
False ==> _ = H.discard
True  ==> p = p
infixr 0 ==>

-------------------------------------------------------------------------------
-- Generators

genThreadId :: H.Gen D.ThreadId
genThreadId = D.ThreadId <$> genId

genCRefId :: H.Gen D.CRefId
genCRefId = D.CRefId <$> genId

genMVarId :: H.Gen D.MVarId
genMVarId = D.MVarId <$> genId

genTVarId :: H.Gen D.TVarId
genTVarId = D.TVarId <$> genId

genId :: H.Gen D.Id
genId = D.Id <$> HGen.maybe genString <*> genSmallInt

genFailure :: H.Gen D.Failure
genFailure = HGen.element $
  [ D.InternalError
  , D.Abort
  , D.Deadlock
  , D.STMDeadlock
  , D.IllegalSubconcurrency
  ] ++ map D.UncaughtException -- have a few different exception types
  [ E.toException E.Overflow
  , E.toException E.ThreadKilled
  , E.toException E.NonTermination
  ]

genWBKey :: H.Gen (D.ThreadId, Maybe D.CRefId)
genWBKey = (,) <$> genThreadId <*> HGen.maybe genCRefId

genThreadAction :: H.Gen D.ThreadAction
genThreadAction = HGen.choice
  [ D.Fork <$> genThreadId
  , pure D.MyThreadId
  , D.GetNumCapabilities <$> genSmallInt
  , D.SetNumCapabilities <$> genSmallInt
  , pure D.Yield
  , D.ThreadDelay <$> genSmallInt
  , D.NewMVar <$> genMVarId
  , D.PutMVar <$> genMVarId <*> genSmallList genThreadId
  , D.BlockedPutMVar <$> genMVarId
  , D.TryPutMVar <$> genMVarId <*> HGen.bool <*> genSmallList genThreadId
  , D.ReadMVar <$> genMVarId
  , D.TryReadMVar <$> genMVarId <*> HGen.bool
  , D.BlockedReadMVar <$> genMVarId
  , D.TakeMVar <$> genMVarId <*> genSmallList genThreadId
  , D.BlockedTakeMVar <$> genMVarId
  , D.TryTakeMVar <$> genMVarId <*> HGen.bool <*> genSmallList genThreadId
  , D.NewCRef <$> genCRefId
  , D.ReadCRef <$> genCRefId
  , D.ReadCRefCas <$> genCRefId
  , D.ModCRef <$> genCRefId
  , D.ModCRefCas <$> genCRefId
  , D.WriteCRef <$> genCRefId
  , D.CasCRef <$> genCRefId <*> HGen.bool
  , D.CommitCRef <$> genThreadId <*> genCRefId
  , D.STM <$> genSmallList genTAction <*> genSmallList genThreadId
  , D.BlockedSTM <$> genSmallList genTAction
  , pure D.Catching
  , pure D.PopCatching
  , pure D.Throw
  , D.ThrowTo <$> genThreadId
  , D.BlockedThrowTo <$> genThreadId
  , pure D.Killed
  , D.SetMasking <$> HGen.bool <*> genMaskingState
  , D.ResetMasking <$> HGen.bool <*> genMaskingState
  , pure D.LiftIO
  , pure D.Return
  , pure D.Stop
  , pure D.Subconcurrency
  , pure D.StopSubconcurrency
  ]

genTAction :: H.Gen D.TAction
genTAction = HGen.choice
  [ D.TNew <$> genTVarId
  , D.TRead <$> genTVarId
  , D.TWrite <$> genTVarId
  , pure D.TRetry
  , HGen.small $ D.TOrElse <$> genSmallList genTAction <*> HGen.maybe (genSmallList genTAction)
  , pure D.TThrow
  , HGen.small $ D.TCatch <$> genSmallList genTAction <*> HGen.maybe (genSmallList genTAction)
  , pure D.TStop
  ]

genMaskingState :: H.Gen E.MaskingState
genMaskingState = HGen.element
  [ E.Unmasked
  , E.MaskedInterruptible
  , E.MaskedUninterruptible
  ]

genActionType :: H.Gen D.ActionType
genActionType = HGen.choice
  [ genUnsynchronisedActionType
  , genPartiallySynchronisedActionType
  , genSynchronisedActionType
  ]

genUnsynchronisedActionType :: H.Gen D.ActionType
genUnsynchronisedActionType = HGen.choice
  [ D.UnsynchronisedRead <$> genCRefId
  , D.UnsynchronisedWrite <$> genCRefId
  , pure D.UnsynchronisedOther
  ]

genPartiallySynchronisedActionType :: H.Gen D.ActionType
genPartiallySynchronisedActionType = HGen.choice
  [ D.PartiallySynchronisedCommit <$> genCRefId
  , D.PartiallySynchronisedWrite <$> genCRefId
  , D.PartiallySynchronisedModify <$> genCRefId
  ]

genSynchronisedActionType :: H.Gen D.ActionType
genSynchronisedActionType = HGen.choice
  [ D.SynchronisedModify <$> genCRefId
  , D.SynchronisedRead <$> genMVarId
  , D.SynchronisedWrite <$> genMVarId
  , pure D.SynchronisedOther
  ]

genDepState :: H.Gen SCT.DepState
genDepState = SCT.DepState
  <$> genMap genCRefId HGen.bool
  <*> genSet genMVarId
  <*> genMap genThreadId genMaskingState

-------------------------------------------------------------------------------
-- Utility generators

genSmallInt :: H.Gen Int
genSmallInt = genIntFromTo 0 10

genInt :: H.Gen Int
genInt = genIntFromTo 0 100

genIntFromTo :: Int -> Int -> H.Gen Int
genIntFromTo from = HGen.int . HRange.linear from

genMap :: Ord k => H.Gen k -> H.Gen v -> H.Gen (M.Map k v)
genMap genKey genVal = M.fromList <$> genList ((,) <$> genKey <*> genVal)

genSet :: Ord a => H.Gen a -> H.Gen (Set.Set a)
genSet gen = Set.fromList <$> genList gen

genString :: H.Gen String
genString = genSmallList HGen.enumBounded

genList :: H.Gen a -> H.Gen [a]
genList = genListUpTo 100

genSmallList :: H.Gen a -> H.Gen [a]
genSmallList = genListUpTo 10

genListUpTo :: Int -> H.Gen a -> H.Gen [a]
genListUpTo = HGen.list . HRange.linear 0

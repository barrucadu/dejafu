{-# LANGUAGE TypeFamilies #-}
module Unit.Properties where

import qualified Control.Exception                as E
import           Control.Monad                    (zipWithM)
import qualified Control.Monad.Conc.Class         as C
import           Control.Monad.IO.Class           (liftIO)
import qualified Control.Monad.ST                 as ST
import qualified Data.Foldable                    as F
import qualified Data.Map                         as M
import qualified Data.Sequence                    as S
import qualified Hedgehog                         as H
import qualified Hedgehog.Gen                     as HGen
import qualified Test.DejaFu.Conc.Internal.Common as D
import qualified Test.DejaFu.Conc.Internal.Memory as Mem
import qualified Test.DejaFu.Internal             as D
import qualified Test.DejaFu.SCT.Internal.DPOR    as SCT
import qualified Test.DejaFu.Types                as D

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
      [ testProperty "Reflexivity (==)" $ do
          x <- H.forAll gen
          x H.=== x

      , testProperty "Symmetry (==)" $ do
          x <- H.forAll gen
          y <- H.forAll gen
          (x == y) H.=== (y == x)

      , testProperty "Transitivity (==)" $ do
          x <- H.forAll gen
          y <- H.forAll (HGen.filter (==x) gen)
          z <- H.forAll (HGen.filter (==y) gen)
          x H.=== z

      , testProperty "Reflexivity (<=)" $ do
          x <- H.forAll gen
          H.assert (x <= x)

      , testProperty "Antisymmetry (<=)" $ do
          x <- H.forAll gen
          y <- H.forAll (HGen.filter (\y -> x <= y && y <= x) gen)
          x H.=== y

      , testProperty "Transitivity (<=)" $ do
          x <- H.forAll gen
          y <- H.forAll (HGen.filter (x<=) gen)
          z <- H.forAll (HGen.filter (y<=) gen)
          H.assert (x <= z)

      , testProperty "Eq / Ord Consistency" $ do
          x <- H.forAll gen
          y <- H.forAll (HGen.filter (==x) gen)
          H.assert (x <= y)
          H.assert (y <= x)
      ]

-------------------------------------------------------------------------------

commonProps :: [TestTree]
commonProps = toTestList
  [ testProperty "simplifyAction a == simplifyLookahead (rewind a)" $ do
      act <- H.forAll genThreadAction
      case D.rewind act of
        Just lh -> D.simplifyAction act H.=== D.simplifyLookahead lh
        Nothing -> H.discard

  , testProperty "isBarrier a ==> synchronises a r" $ do
      a <- H.forAll (HGen.filter D.isBarrier genActionType)
      r <- H.forAll genCRefId
      H.assert (D.synchronises a r)

  , testProperty "isCommit a r ==> synchronises a r" $ do
      a <- H.forAll genPartiallySynchronisedActionType
      case D.crefOf a of
        Just r -> H.assert (D.synchronises a r)
        _ -> H.discard
  ]

-------------------------------------------------------------------------------

memoryProps :: [TestTree]
memoryProps = toTestList
    [ testProperty "bufferWrite emptyBuffer k c a /= emptyBuffer" $ do
        k <- H.forAll genWBKey
        a <- H.forAll genInt
        res <- crefProp $ \cref -> do
          wb <- Mem.bufferWrite Mem.emptyBuffer k cref a
          wb `eqWB` Mem.emptyBuffer
        H.assert (not res)

    , testProperty "commitWrite emptyBuffer k == emptyBuffer" $ do
        k <- H.forAll genWBKey
        res <- liftIO $ do
          wb <- Mem.commitWrite Mem.emptyBuffer k
          wb `eqWB` Mem.emptyBuffer
        H.assert res

    , testProperty "commitWrite (bufferWrite emptyBuffer k a) k == emptyBuffer" $ do
        k <- H.forAll genWBKey
        a <- H.forAll genInt
        res <- crefProp $ \cref -> do
          wb1 <- Mem.bufferWrite Mem.emptyBuffer k cref a
          wb2 <- Mem.commitWrite wb1 k
          wb2 `eqWB` Mem.emptyBuffer
        H.assert res

    , testProperty "Single buffered write/read from same thread" $ do
        k@(tid, _) <- H.forAll genWBKey
        a <- H.forAll genInt
        res <- crefProp $ \cref -> do
          _ <- Mem.bufferWrite Mem.emptyBuffer k cref a
          Mem.readCRef cref tid
        a H.=== res

    , testProperty "Overriding buffered write/read from same thread" $ do
        k@(tid, _) <- H.forAll genWBKey
        a1 <- H.forAll genInt
        a2 <- H.forAll genInt
        res <- crefProp $ \cref -> do
          _ <- Mem.bufferWrite Mem.emptyBuffer k cref a1
          _ <- Mem.bufferWrite Mem.emptyBuffer k cref a2
          Mem.readCRef cref tid
        a2 H.=== res

    , testProperty "Buffered write/read from different thread" $ do
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
      :: Show a
      => (D.ModelCRef IO Int -> IO a)
      -> H.PropertyT IO a
    crefProp p = do
      crefId <- H.forAll genCRefId
      liftIO $ do
        cref <- makeCRef crefId
        p cref

-------------------------------------------------------------------------------

sctProps :: [TestTree]
sctProps = toTestList
  [ testProperty "canInterrupt ==> canInterruptL" $ do
      ds <- H.forAll genDepState
      tid <- H.forAll genThreadId
      act <- H.forAll (HGen.filter (SCT.canInterrupt ds tid) genThreadAction)
      case D.rewind act of
        Just lh -> H.assert (SCT.canInterruptL ds tid lh)
        Nothing -> H.discard

  , testProperty "dependent ==> dependent'" $ do
      ds <- H.forAll genDepState
      tid1 <- H.forAll genThreadId
      tid2 <- H.forAll genThreadId
      ta1 <- H.forAll genThreadAction
      ta2 <- H.forAll (HGen.filter (SCT.dependent ds tid1 ta1 tid2) genThreadAction)
      case D.rewind ta2 of
        Just lh -> H.assert (SCT.dependent' ds tid1 ta1 tid2 lh)
        Nothing -> H.discard

  , testProperty "dependent x y == dependent y x" $ do
      ds <- H.forAll genDepState
      tid1 <- H.forAll genThreadId
      tid2 <- H.forAll genThreadId
      ta1 <- H.forAll genThreadAction
      ta2 <- H.forAll genThreadAction
      SCT.dependent ds tid1 ta1 tid2 ta2 H.=== SCT.dependent ds tid2 ta2 tid1 ta1

  , testProperty "dependentActions x y == dependentActions y x" $ do
      ds <- H.forAll genDepState
      a1 <- H.forAll genActionType
      a2 <- H.forAll genActionType
      SCT.dependentActions ds a1 a2 H.=== SCT.dependentActions ds a2 a1
  ]

-------------------------------------------------------------------------------
-- Utils

makeCRef :: D.CRefId -> IO (D.ModelCRef IO Int)
makeCRef crid = D.ModelCRef crid <$> C.newCRef (M.empty, 0, 42)

-- equality for writebuffers is a little tricky as we can't directly
-- compare the buffered values, so we compare everything else:
--  - the sets of nonempty buffers must be equal
--  - each pair of buffers for the same key must have the same size
--  - each pair of buffers for the same key must have an equal sequence of writes
--
-- individual writes are compared like so:
--  - the threadid and crefid must be the same
--  - the cache and number of writes inside the ref must be the same
eqWB :: Mem.WriteBuffer IO -> Mem.WriteBuffer IO -> IO Bool
eqWB (Mem.WriteBuffer wb1) (Mem.WriteBuffer wb2) = andM (pure (ks1 == ks2) :
    [ (&&) (S.length ws1 == S.length ws2) <$> (and <$> zipWithM eqBW (F.toList ws1) (F.toList ws2))
    | k <- ks1
    , let (Just ws1) = M.lookup k wb1
    , let (Just ws2) = M.lookup k wb2
    ])
  where
    ks1 = M.keys $ M.filter (not . S.null) wb1
    ks2 = M.keys $ M.filter (not . S.null) wb2

    eqBW (Mem.BufferedWrite t1 (D.ModelCRef crid1 ref1) _) (Mem.BufferedWrite t2 (D.ModelCRef crid2 ref2) _) = do
      d1 <- (\(m,i,_) -> (M.keys m, i)) <$> C.readCRef ref1
      d2 <- (\(m,i,_) -> (M.keys m, i)) <$> C.readCRef ref2
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
  <$> genSmallMap genCRefId HGen.bool
  <*> genSmallSet genMVarId
  <*> genSmallMap genThreadId genMaskingState

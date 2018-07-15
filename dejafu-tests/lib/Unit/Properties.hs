{-# LANGUAGE TypeFamilies #-}
module Unit.Properties where

import qualified Control.Exception                    as E
import           Control.Monad                        (zipWithM)
import qualified Control.Monad.Conc.Class             as C
import           Control.Monad.IO.Class               (liftIO)
import           Data.Coerce                          (coerce)
import qualified Data.Foldable                        as F
import           Data.Functor.Contravariant           (contramap)
import           Data.Functor.Contravariant.Divisible (conquer, divide)
import qualified Data.Map                             as M
import           Data.Semigroup                       ((<>))
import qualified Data.Sequence                        as S
import qualified Hedgehog                             as H
import qualified Hedgehog.Gen                         as HGen
import qualified Test.DejaFu.Conc.Internal.Common     as D
import qualified Test.DejaFu.Conc.Internal.Memory     as Mem
import qualified Test.DejaFu.Internal                 as D
import qualified Test.DejaFu.SCT.Internal.DPOR        as SCT
import qualified Test.DejaFu.Types                    as D

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
    , testGroup "Weaken"  (discardf min D.getWeakDiscarder)
    , testGroup "Strengthen" (discardf max D.getStrongDiscarder)
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

    discardf choose extract =
        [ testProperty "Associativity (<>)" $ do
            x <- coerce . applyFunction <$> H.forAll genDiscarder
            y <- coerce . applyFunction <$> H.forAll genDiscarder
            z <- coerce . applyFunction <$> H.forAll genDiscarder
            x <> (y <> z) ==== (x <> y) <> z

        , testProperty "Commutativity (<>)" $ do
            x <- coerce . applyFunction <$> H.forAll genDiscarder
            y <- coerce . applyFunction <$> H.forAll genDiscarder
            x <> y ==== y <> x

        , testProperty "Unit (<>)" $ do
            x <- coerce . applyFunction <$> H.forAll genDiscarder
            x <> mempty ==== x

        , testProperty "Homomorphism (<>)" $ do
            let o d1 d2 efa = d1 efa `choose` d2 efa
            x <- coerce . applyFunction <$> H.forAll genDiscarder
            y <- coerce . applyFunction <$> H.forAll genDiscarder
            efa <- H.forAll genEfa
            extract (x <> y) efa H.=== (extract x `o` extract y) efa

        , testProperty "Identity (contramap)" $ do
            x <- coerce . applyFunction <$> H.forAll genDiscarder
            contramap id x ==== x

        , testProperty "Associativity (divide + delta)" $ do
            x <- coerce . applyFunction <$> H.forAll genDiscarder
            y <- coerce . applyFunction <$> H.forAll genDiscarder
            z <- coerce . applyFunction <$> H.forAll genDiscarder
            divide delta (divide delta x y) z ==== divide delta x (divide delta y z)

        , testProperty "Commutativity (divide + delta)" $ do
            x <- coerce . applyFunction <$> H.forAll genDiscarder
            y <- coerce . applyFunction <$> H.forAll genDiscarder
            divide delta x y ==== divide delta y x

        , testProperty "Unit (divide + delta)" $ do
            x <- coerce . applyFunction <$> H.forAll genDiscarder
            divide delta x conquer ==== x

        , testProperty "Generalised Associativity (divide)" $ do
            let genF = genFunc (genPair genSmallInt)
            f <- applyFunction <$> H.forAll genF
            g <- applyFunction <$> H.forAll genF
            x <- coerce . applyFunction <$> H.forAll genDiscarder
            y <- coerce . applyFunction <$> H.forAll genDiscarder
            z <- coerce . applyFunction <$> H.forAll genDiscarder
            let f' a = let (bc, d) = f a; (b, c) = g bc in (b, (c, d))
            divide f (divide g x y) z ==== divide f' x (divide id y z)

        , testProperty "Divisible / Contravariant Consistency (fst)" $ do
            let genF = genFunc (genPair genSmallInt)
            f <- applyFunction <$> H.forAll genF
            x <- coerce . applyFunction <$> H.forAll genDiscarder
            divide f x conquer ==== contramap (fst . f) x

        , testProperty "Divisible / Contravariant Consistency (snd)" $ do
            let genF = genFunc (genPair genSmallInt)
            f <- applyFunction <$> H.forAll genF
            x <- coerce . applyFunction <$> H.forAll genDiscarder
            divide f conquer x ==== contramap (snd . f) x
        ]
      where
        genFunc = genFunction genSmallInt

        d1 ==== d2 = do
          efa <- H.forAll genEfa
          extract d1 efa H.=== extract d2 efa
        infix 4 ====

        delta x = (x, x)

-------------------------------------------------------------------------------

commonProps :: [TestTree]
commonProps = toTestList
  [ testProperty "simplifyAction a == simplifyLookahead (rewind a)" $ do
      act <- H.forAll genThreadAction
      D.simplifyAction act H.=== D.simplifyLookahead (D.rewind act)

  , testProperty "isBarrier a ==> synchronises a r" $ do
      a <- H.forAll (HGen.filter D.isBarrier genActionType)
      r <- H.forAll genIORefId
      H.assert (D.synchronises a r)

  , testProperty "isCommit a r ==> synchronises a r" $ do
      a <- H.forAll genPartiallySynchronisedActionType
      case D.iorefOf a of
        Just r -> H.assert (D.synchronises a r)
        _ -> H.discard
  ]

-------------------------------------------------------------------------------

memoryProps :: [TestTree]
memoryProps = toTestList
    [ testProperty "bufferWrite emptyBuffer k c a /= emptyBuffer" $ do
        k <- H.forAll genWBKey
        a <- H.forAll genInt
        res <- iorefProp $ \ioref -> do
          wb <- Mem.bufferWrite Mem.emptyBuffer k ioref a
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
        res <- iorefProp $ \ioref -> do
          wb1 <- Mem.bufferWrite Mem.emptyBuffer k ioref a
          wb2 <- Mem.commitWrite wb1 k
          wb2 `eqWB` Mem.emptyBuffer
        H.assert res

    , testProperty "Single buffered write/read from same thread" $ do
        k@(tid, _) <- H.forAll genWBKey
        a <- H.forAll genInt
        res <- iorefProp $ \ioref -> do
          _ <- Mem.bufferWrite Mem.emptyBuffer k ioref a
          Mem.readIORef ioref tid
        a H.=== res

    , testProperty "Overriding buffered write/read from same thread" $ do
        k@(tid, _) <- H.forAll genWBKey
        a1 <- H.forAll genInt
        a2 <- H.forAll genInt
        res <- iorefProp $ \ioref -> do
          _ <- Mem.bufferWrite Mem.emptyBuffer k ioref a1
          _ <- Mem.bufferWrite Mem.emptyBuffer k ioref a2
          Mem.readIORef ioref tid
        a2 H.=== res

    , testProperty "Buffered write/read from different thread" $ do
        k1@(tid, _) <- H.forAll genWBKey
        k2 <- H.forAll (HGen.filter ((/=tid) . fst) genWBKey)
        a1 <- H.forAll genInt
        a2 <- H.forAll genInt
        res <- iorefProp $ \ioref -> do
          _ <- Mem.bufferWrite Mem.emptyBuffer k1 ioref a1
          _ <- Mem.bufferWrite Mem.emptyBuffer k2 ioref a2
          Mem.readIORef ioref tid
        a1 H.=== res
    ]
  where
    iorefProp
      :: Show a
      => (D.ModelIORef IO Int -> IO a)
      -> H.PropertyT IO a
    iorefProp p = do
      iorefId <- H.forAll genIORefId
      liftIO $ do
        ioref <- makeIORef iorefId
        p ioref

-------------------------------------------------------------------------------

sctProps :: [TestTree]
sctProps = toTestList
  [ testProperty "canInterrupt ==> canInterruptL" $ do
      ds <- H.forAll genDepState
      tid <- H.forAll genThreadId
      act <- H.forAll (HGen.filter (SCT.canInterrupt ds tid) genThreadAction)
      H.assert (SCT.canInterruptL ds tid (D.rewind act))

  , testProperty "dependent ==> dependent'" $ do
      safeIO <- H.forAll HGen.bool
      ds <- H.forAll genDepState
      tid1 <- H.forAll genThreadId
      tid2 <- H.forAll genThreadId
      ta1 <- H.forAll genThreadAction
      ta2 <- H.forAll (HGen.filter (SCT.dependent safeIO ds tid1 ta1 tid2) genThreadAction)
      H.assert (SCT.dependent' safeIO ds tid1 ta1 tid2 (D.rewind ta2))

  , testProperty "dependent x y == dependent y x" $ do
      safeIO <- H.forAll HGen.bool
      ds <- H.forAll genDepState
      tid1 <- H.forAll genThreadId
      tid2 <- H.forAll genThreadId
      ta1 <- H.forAll genThreadAction
      ta2 <- H.forAll genThreadAction
      SCT.dependent safeIO ds tid1 ta1 tid2 ta2 H.=== SCT.dependent safeIO ds tid2 ta2 tid1 ta1

  , testProperty "dependentActions x y == dependentActions y x" $ do
      ds <- H.forAll genDepState
      a1 <- H.forAll genActionType
      a2 <- H.forAll genActionType
      SCT.dependentActions ds a1 a2 H.=== SCT.dependentActions ds a2 a1
  ]

-------------------------------------------------------------------------------
-- Utils

makeIORef :: D.IORefId -> IO (D.ModelIORef IO Int)
makeIORef iorid = D.ModelIORef iorid <$> C.newIORef (M.empty, 0, 42)

-- equality for writebuffers is a little tricky as we can't directly
-- compare the buffered values, so we compare everything else:
--  - the sets of nonempty buffers must be equal
--  - each pair of buffers for the same key must have the same size
--  - each pair of buffers for the same key must have an equal sequence of writes
--
-- individual writes are compared like so:
--  - the threadid and iorefid must be the same
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

    eqBW (Mem.BufferedWrite t1 (D.ModelIORef iorid1 ref1) _) (Mem.BufferedWrite t2 (D.ModelIORef iorid2 ref2) _) = do
      d1 <- (\(m,i,_) -> (M.keys m, i)) <$> C.readIORef ref1
      d2 <- (\(m,i,_) -> (M.keys m, i)) <$> C.readIORef ref2
      pure (t1 == t2 && iorid1 == iorid2 && d1 == d2)

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

genIORefId :: H.Gen D.IORefId
genIORefId = D.IORefId <$> genId

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

genWBKey :: H.Gen (D.ThreadId, Maybe D.IORefId)
genWBKey = (,) <$> genThreadId <*> HGen.maybe genIORefId

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
  , D.NewIORef <$> genIORefId
  , D.ReadIORef <$> genIORefId
  , D.ReadIORefCas <$> genIORefId
  , D.ModIORef <$> genIORefId
  , D.ModIORefCas <$> genIORefId
  , D.WriteIORef <$> genIORefId
  , D.CasIORef <$> genIORefId <*> HGen.bool
  , D.CommitIORef <$> genThreadId <*> genIORefId
  , D.STM <$> genSmallList genTAction <*> genSmallList genThreadId
  , D.BlockedSTM <$> genSmallList genTAction
  , pure D.Catching
  , pure D.PopCatching
  , D.Throw <$> HGen.bool
  , D.ThrowTo <$> genThreadId <*> HGen.bool
  , D.BlockedThrowTo <$> genThreadId
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
  [ D.UnsynchronisedRead <$> genIORefId
  , D.UnsynchronisedWrite <$> genIORefId
  , pure D.UnsynchronisedOther
  ]

genPartiallySynchronisedActionType :: H.Gen D.ActionType
genPartiallySynchronisedActionType = HGen.choice
  [ D.PartiallySynchronisedCommit <$> genIORefId
  , D.PartiallySynchronisedWrite <$> genIORefId
  , D.PartiallySynchronisedModify <$> genIORefId
  ]

genSynchronisedActionType :: H.Gen D.ActionType
genSynchronisedActionType = HGen.choice
  [ D.SynchronisedModify <$> genIORefId
  , D.SynchronisedRead <$> genMVarId
  , D.SynchronisedWrite <$> genMVarId
  , pure D.SynchronisedOther
  ]

genDepState :: H.Gen SCT.DepState
genDepState = SCT.DepState
  <$> genSmallMap genIORefId genSmallInt
  <*> genSmallSet genMVarId
  <*> genSmallMap genThreadId genMaskingState

genDiscarder :: H.Gen (Function (Either D.Failure Int) (Maybe D.Discard))
genDiscarder = genFunction genEfa (HGen.maybe genDiscard)

genEfa :: H.Gen (Either D.Failure Int)
genEfa = genEither genFailure genSmallInt

genDiscard :: H.Gen D.Discard
genDiscard = HGen.element
  [ D.DiscardTrace
  , D.DiscardResultAndTrace
  ]

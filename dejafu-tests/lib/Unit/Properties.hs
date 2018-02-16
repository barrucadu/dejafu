{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Unit.Properties where

import qualified Control.Exception                as E
import           Control.Monad                    (zipWithM)
import qualified Control.Monad.ST                 as ST
import qualified Data.Foldable                    as F
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust, isJust)
import           Data.Proxy                       (Proxy(..))
import qualified Data.Sequence                    as S
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import qualified Data.STRef                       as ST
import qualified Test.DejaFu.Conc.Internal.Common as D
import qualified Test.DejaFu.Conc.Internal.Memory as Mem
import qualified Test.DejaFu.Internal             as D
import qualified Test.DejaFu.SCT.Internal.DPOR    as SCT
import           Test.DejaFu.Types                (Lookahead, ThreadAction)
import qualified Test.DejaFu.Types                as D
import           Test.LeanCheck                   (Listable(..), cons0, cons1,
                                                   cons2, cons3, mapT, (==>),
                                                   (><), (\/))
import           Test.Tasty.LeanCheck             (testProperty)

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
    [ testGroup "Id"      (eqord (Proxy :: Proxy D.Id))
    , testGroup "Failure" (eqord (Proxy :: Proxy D.Failure))
    ]
  where
    eqord :: forall a. (Eq a, Ord a, Listable a, Show a) => Proxy a -> [TestTree]
    eqord _ =
      [ testProperty "Reflexivity (==)"     $ \(x :: a)     -> x == x
      , testProperty "Symmetry (==)"        $ \(x :: a) y   -> (x == y) == (y == x)
      , testProperty "Transitivity (==)"    $ \(x :: a) y z -> x == y && y == z ==> x == z
      , testProperty "Reflexivity (<=)"     $ \(x :: a)     -> x <= x
      , testProperty "Antisymmetry (<=)"    $ \(x :: a) y   -> x <= y && y <= x ==> x == y
      , testProperty "Transitivity (<=)"    $ \(x :: a) y z -> x <= y && y <= z ==> x <= z
      , testProperty "Eq / Ord Consistency" $ \(x :: a) y   -> x == y ==> x <= y
      ]

-------------------------------------------------------------------------------

commonProps :: [TestTree]
commonProps = toTestList
  [ testProperty "simplifyAction a == simplifyLookahead (rewind a)" $
    \act -> canRewind act ==>
    D.simplifyAction act == D.simplifyLookahead (rewind' act)

  , testProperty "isBarrier a ==> synchronises a r" $
    \a r -> D.isBarrier a ==> D.synchronises a r

  , testProperty "isCommit a r ==> synchronises a r" $
    \a r -> D.isCommit a r ==> D.synchronises a r
  ]

-------------------------------------------------------------------------------

memoryProps :: [TestTree]
memoryProps = toTestList
    [ testProperty "bufferWrite emptyBuffer k c a /= emptyBuffer" $
      \k a -> crefProp $ \cref -> do
        wb <- Mem.bufferWrite Mem.emptyBuffer k cref a
        not <$> wb `eqWB` Mem.emptyBuffer

    , testProperty "commitWrite emptyBuffer k == emptyBuffer" $
      \k -> ST.runST $ do
        wb <- Mem.commitWrite Mem.emptyBuffer k
        wb `eqWB` Mem.emptyBuffer

    , testProperty "commitWrite (bufferWrite emptyBuffer k a) k == emptyBuffer" $
      \k a -> crefProp $ \cref -> do
        wb1 <- Mem.bufferWrite Mem.emptyBuffer k cref a
        wb2 <- Mem.commitWrite wb1 k
        wb2 `eqWB` Mem.emptyBuffer

    , testProperty "Single buffered write/read from same thread" $
      \k@(tid, _) a -> crefProp $ \cref -> do
        _ <- Mem.bufferWrite Mem.emptyBuffer k cref a
        (a ==) <$> Mem.readCRef cref tid

    , testProperty "Overriding buffered write/read from same thread" $
      \k@(tid, _) a1 a2 -> crefProp $ \cref -> do
        _ <- Mem.bufferWrite Mem.emptyBuffer k cref a1
        _ <- Mem.bufferWrite Mem.emptyBuffer k cref a2
        (a2 ==) <$> Mem.readCRef cref tid

    , testProperty "Buffered write/read from different thread" $
      \k1@(tid1, _) k2@(tid2, _) a1 a2 -> crefProp $ \cref -> do
        _ <- Mem.bufferWrite Mem.emptyBuffer k1 cref a1
        _ <- Mem.bufferWrite Mem.emptyBuffer k2 cref a2
        a' <- Mem.readCRef cref tid1
        pure (tid1 /= tid2 ==> a' == a1)
    ]
  where
    crefProp :: (forall s. D.CRef (ST.STRef s) Int -> ST.ST s Bool) -> D.CRefId -> Bool
    crefProp prop crid = ST.runST $ makeCRef crid >>= prop

-------------------------------------------------------------------------------

sctProps :: [TestTree]
sctProps = toTestList
  [ testProperty "canInterrupt ==> canInterruptL" $
    \ds tid act ->
      canRewind act && SCT.canInterrupt ds tid act ==>
      SCT.canInterruptL ds tid (rewind' act)

  , testProperty "dependent ==> dependent'" $
    \ds tid1 tid2 ta1 ta2 ->
      canRewind ta2 && SCT.dependent ds tid1 ta1 tid2 ta2 ==>
      SCT.dependent' ds tid1 ta1 tid2 (rewind' ta2)

  , testProperty "dependent x y == dependent y x" $
    \ds tid1 tid2 ta1 ta2 ->
      SCT.dependent ds tid1 ta1 tid2 ta2 ==
      SCT.dependent ds tid2 ta2 tid1 ta1

  , testProperty "dependentActions x y == dependentActions y x" $
    \ds a1 a2 ->
      SCT.dependentActions ds a1 a2 == SCT.dependentActions ds a2 a1
  ]

-------------------------------------------------------------------------------
-- Utils

canRewind :: ThreadAction -> Bool
canRewind = isJust . D.rewind

rewind' :: ThreadAction -> Lookahead
rewind' = fromJust . D.rewind

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

-------------------------------------------------------------------------------
-- Typeclass instances

instance Listable D.ThreadId where
  tiers = mapT D.ThreadId tiers

instance Listable D.CRefId where
  tiers = mapT D.CRefId tiers

instance Listable D.MVarId where
  tiers = mapT D.MVarId tiers

instance Listable D.TVarId where
  tiers = mapT D.TVarId tiers

instance Listable D.Id where
  tiers = mapT (D.Id Nothing) tiers

instance Listable D.ThreadAction where
  tiers =
       cons1 D.Fork
    \/ cons0 D.MyThreadId
    \/ cons1 D.GetNumCapabilities
    \/ cons1 D.SetNumCapabilities
    \/ cons0 D.Yield
    \/ cons1 D.ThreadDelay
    \/ cons1 D.NewMVar
    \/ cons2 D.PutMVar
    \/ cons1 D.BlockedPutMVar
    \/ cons3 D.TryPutMVar
    \/ cons1 D.ReadMVar
    \/ cons2 D.TryReadMVar
    \/ cons1 D.BlockedReadMVar
    \/ cons2 D.TakeMVar
    \/ cons1 D.BlockedTakeMVar
    \/ cons3 D.TryTakeMVar
    \/ cons1 D.NewCRef
    \/ cons1 D.ReadCRef
    \/ cons1 D.ReadCRefCas
    \/ cons1 D.ModCRef
    \/ cons1 D.ModCRefCas
    \/ cons1 D.WriteCRef
    \/ cons2 D.CasCRef
    \/ cons2 D.CommitCRef
    \/ cons2 D.STM
    \/ cons1 D.BlockedSTM
    \/ cons0 D.Catching
    \/ cons0 D.PopCatching
    \/ cons0 D.Throw
    \/ cons1 D.ThrowTo
    \/ cons1 D.BlockedThrowTo
    \/ cons0 D.Killed
    \/ cons2 D.SetMasking
    \/ cons2 D.ResetMasking
    \/ cons0 D.LiftIO
    \/ cons0 D.Return
    \/ cons0 D.Stop
    \/ cons0 D.Subconcurrency
    \/ cons0 D.StopSubconcurrency

instance Listable D.TAction where
  tiers =
       cons1 D.TNew
    \/ cons1 D.TRead
    \/ cons1 D.TWrite
    \/ cons0 D.TRetry
    \/ cons2 D.TOrElse
    \/ cons0 D.TThrow
    \/ cons2 D.TCatch
    \/ cons0 D.TStop

instance Listable E.MaskingState where
  list =
    [ E.Unmasked
    , E.MaskedInterruptible
    , E.MaskedUninterruptible
    ]

instance Listable D.ActionType where
  tiers =
       cons1 D.UnsynchronisedRead
    \/ cons1 D.UnsynchronisedWrite
    \/ cons0 D.UnsynchronisedOther
    \/ cons1 D.PartiallySynchronisedCommit
    \/ cons1 D.PartiallySynchronisedWrite
    \/ cons1 D.PartiallySynchronisedModify
    \/ cons1 D.SynchronisedModify
    \/ cons1 D.SynchronisedRead
    \/ cons1 D.SynchronisedWrite
    \/ cons0 D.SynchronisedOther

instance Listable SCT.DepState where
  tiers = mapT (\(a,(b,c)) -> SCT.DepState a b c) (tiers >< tiers >< tiers)

instance (Ord k, Listable k, Listable v) => Listable (Map k v) where
  tiers = mapT M.fromList tiers

instance (Ord v, Listable v) => Listable (Set v) where
  tiers = mapT Set.fromList tiers

instance Listable D.Failure where
  list =
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

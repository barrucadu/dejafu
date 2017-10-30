{-# LANGUAGE ScopedTypeVariables #-}
module Cases.Properties where

import qualified Control.Exception as E
import Data.Map (Map, fromList)
import Data.Maybe (fromJust, isJust)
import Data.Proxy (Proxy(..))
import Test.DejaFu.Common (ThreadAction, Lookahead)
import qualified Test.DejaFu.Common as D
import qualified Test.DejaFu.SCT.Internal as SCT
import Test.Framework (Test)
import Test.LeanCheck (Listable(..), (\/), (><), (==>), cons0, cons1, cons2, cons3, mapT)

import Common

tests :: [Test]
tests =
  [ testGroup "Class Laws"
    [ testGroup "ThreadId" (eqord (Proxy :: Proxy D.ThreadId))
    , testGroup "CRefId"   (eqord (Proxy :: Proxy D.CRefId))
    , testGroup "MVarId"   (eqord (Proxy :: Proxy D.MVarId))
    , testGroup "TVarId"   (eqord (Proxy :: Proxy D.TVarId))
    , testGroup "Failure"  (eqord (Proxy :: Proxy D.Failure))
    ]

  , testGroup "Common"
    [ leancheck "simplifyAction a == simplifyLookahead (rewind a)" $
      \act -> canRewind act ==>
      D.simplifyAction act == D.simplifyLookahead (rewind' act)

    , leancheck "isBarrier a ==> synchronises a r" $
      \a r -> D.isBarrier a ==> D.synchronises a r

    , leancheck "isCommit a r ==> synchronises a r" $
      \a r -> D.isCommit a r ==> D.synchronises a r
    ]

  , testGroup "SCT"
    [ leancheck "canInterrupt ==> canInterruptL" $
      \ds tid act ->
        canRewind act && SCT.canInterrupt ds tid act ==>
        SCT.canInterruptL ds tid (rewind' act)

    , leancheck "dependent ==> dependent'" $
      \mem ds tid1 tid2 ta1 ta2 ->
        canRewind ta2 && SCT.dependent mem ds tid1 ta1 tid2 ta2 ==>
        SCT.dependent' mem ds tid1 ta1 tid2 (rewind' ta2)

    , leancheck "dependent x y == dependent y x" $
      \mem ds tid1 tid2 ta1 ta2 ->
        SCT.dependent mem ds tid1 ta1 tid2 ta2 ==
        SCT.dependent mem ds tid2 ta2 tid1 ta1

    , leancheck "dependentActions x y == dependentActions y x" $
      \mem ds a1 a2 ->
        SCT.dependentActions mem ds a1 a2 == SCT.dependentActions mem ds a2 a1
    ]
  ]
  where
    eqord :: forall a. (Eq a, Ord a, Listable a, Show a) => Proxy a -> [Test]
    eqord _ =
      [ leancheck "Reflexivity (==)"     $ \(x :: a)     -> x == x
      , leancheck "Symmetry (==)"        $ \(x :: a) y   -> (x == y) == (y == x)
      , leancheck "Transitivity (==)"    $ \(x :: a) y z -> x == y && y == z ==> x == z
      , leancheck "Reflexivity (<=)"     $ \(x :: a)     -> x <= x
      , leancheck "Antisymmetry (<=)"    $ \(x :: a) y   -> x <= y && y <= x ==> x == y
      , leancheck "Transitivity (<=)"    $ \(x :: a) y z -> x <= y && y <= z ==> x <= z
      , leancheck "Eq / Ord Consistency" $ \(x :: a) y   -> x == y ==> x <= y
      ]

-------------------------------------------------------------------------------
-- Utils

canRewind :: ThreadAction -> Bool
canRewind = isJust . D.rewind

rewind' :: ThreadAction -> Lookahead
rewind' = fromJust . D.rewind

-------------------------------------------------------------------------------
-- Arbitrary instances

instance Listable D.ThreadId where
  tiers = mapT (D.ThreadId Nothing) tiers

instance Listable D.CRefId where
  tiers = mapT (D.CRefId Nothing) tiers

instance Listable D.MVarId where
  tiers = mapT (D.MVarId Nothing) tiers

instance Listable D.TVarId where
  tiers = mapT (D.TVarId Nothing) tiers

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

instance Listable D.MemType where
  list =
    [ D.SequentialConsistency
    , D.TotalStoreOrder
    , D.PartialStoreOrder
    ]

instance Listable SCT.DepState where
  tiers = mapT (uncurry SCT.DepState) (tiers >< tiers)

instance (Ord k, Listable k, Listable v) => Listable (Map k v) where
  tiers = mapT fromList tiers

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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

-- |
-- Module      : Test.DejaFu.Deterministic.Internal.Common
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : ExistentialQuantification, RankNTypes
--
-- Common types and utility functions for deterministic execution of
-- 'MonadConc' implementations. This module is NOT considered to form
module Test.DejaFu.Deterministic.Internal.Common where

import Control.Exception (Exception, MaskingState(..))
import Data.Dynamic (Dynamic)
import Data.Map.Strict (Map)
import Data.List.NonEmpty (NonEmpty, fromList)
import Test.DejaFu.Common

{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}

--------------------------------------------------------------------------------
-- * The @Conc@ Monad

-- | The underlying monad is based on continuations over 'Action's.
--
-- One might wonder why the return type isn't reflected in 'Action',
-- and a free monad formulation used. This would remove the need for a
-- @AStop@ actions having their parameter. However, this makes the
-- current expression of threads and exception handlers very difficult
-- (perhaps even not possible without significant reworking), so I
-- abandoned the attempt.
newtype M n r s a = M { runM :: (a -> Action n r s) -> Action n r s }

instance Functor (M n r s) where
    fmap f m = M $ \ c -> runM m (c . f)

instance Applicative (M n r s) where
    pure x  = M $ \c -> AReturn $ c x
    f <*> v = M $ \c -> runM f (\g -> runM v (c . g))

instance Monad (M n r s) where
    return  = pure
    m >>= k = M $ \c -> runM m (\x -> runM (k x) c)

-- | The concurrent variable type used with the 'Conc' monad. One
-- notable difference between these and 'MVar's is that 'MVar's are
-- single-wakeup, and wake up in a FIFO order. Writing to a @MVar@
-- wakes up all threads blocked on reading it, and it is up to the
-- scheduler which one runs next. Taking from a @MVar@ behaves
-- analogously.
data MVar r a = MVar
  { _cvarId   :: MVarId
  , _cvarVal  :: r (Maybe a)
  }

-- | The mutable non-blocking reference type. These are like 'IORef's.
--
-- @CRef@s are represented as a unique numeric identifier and a
-- reference containing (a) any thread-local non-synchronised writes
-- (so each thread sees its latest write), (b) a commit count (used in
-- compare-and-swaps), and (c) the current value visible to all
-- threads.
data CRef r a = CRef
  { _crefId   :: CRefId
  , _crefVal  :: r (Map ThreadId a, Integer, a)
  }

-- | The compare-and-swap proof type.
--
-- @Ticket@s are represented as just a wrapper around the identifier
-- of the 'CRef' it came from, the commit count at the time it was
-- produced, and an @a@ value. This doesn't work in the source package
-- (atomic-primops) because of the need to use pointer equality. Here
-- we can just pack extra information into 'CRef' to avoid that need.
data Ticket a = Ticket
  { _ticketCRef   :: CRefId
  , _ticketWrites :: Integer
  , _ticketVal    :: a
  }

-- | Construct a continuation-passing operation from a function.
cont :: ((a -> Action n r s) -> Action n r s) -> M n r s a
cont = M

-- | Run a CPS computation with the given final computation.
runCont :: M n r s a -> (a -> Action n r s) -> Action n r s
runCont = runM

--------------------------------------------------------------------------------
-- * Primitive Actions

-- | Scheduling is done in terms of a trace of 'Action's. Blocking can
-- only occur as a result of an action, and they cover (most of) the
-- primitives of the concurrency. 'spawn' is absent as it is
-- implemented in terms of 'newEmptyMVar', 'fork', and 'putMVar'.
data Action n r s =
    AFork  String ((forall b. M n r s b -> M n r s b) -> Action n r s) (ThreadId -> Action n r s)
  | AMyTId (ThreadId -> Action n r s)

  | AGetNumCapabilities (Int -> Action n r s)
  | ASetNumCapabilities Int (Action n r s)

  | forall a. ANewVar String (MVar r a -> Action n r s)
  | forall a. APutVar     (MVar r a) a (Action n r s)
  | forall a. ATryPutVar  (MVar r a) a (Bool -> Action n r s)
  | forall a. AReadVar    (MVar r a) (a -> Action n r s)
  | forall a. ATakeVar    (MVar r a) (a -> Action n r s)
  | forall a. ATryTakeVar (MVar r a) (Maybe a -> Action n r s)

  | forall a.   ANewRef String a (CRef r a -> Action n r s)
  | forall a.   AReadRef    (CRef r a) (a -> Action n r s)
  | forall a.   AReadRefCas (CRef r a) (Ticket a -> Action n r s)
  | forall a.   APeekTicket (Ticket a) (a -> Action n r s)
  | forall a b. AModRef     (CRef r a) (a -> (a, b)) (b -> Action n r s)
  | forall a b. AModRefCas  (CRef r a) (a -> (a, b)) (b -> Action n r s)
  | forall a.   AWriteRef   (CRef r a) a (Action n r s)
  | forall a.   ACasRef     (CRef r a) (Ticket a) a ((Bool, Ticket a) -> Action n r s)

  | forall e.   Exception e => AThrow e
  | forall e.   Exception e => AThrowTo ThreadId e (Action n r s)
  | forall a e. Exception e => ACatching (e -> M n r s a) (M n r s a) (a -> Action n r s)
  | APopCatching (Action n r s)
  | forall a. AMasking MaskingState ((forall b. M n r s b -> M n r s b) -> M n r s a) (a -> Action n r s)
  | AResetMask Bool Bool MaskingState (Action n r s)

  | AKnowsAbout (Either MVarId TVarId) (Action n r s)
  | AForgets    (Either MVarId TVarId) (Action n r s)
  | AAllKnown   (Action n r s)
  | AMessage    Dynamic (Action n r s)

  | forall a. AAtom (s a) (a -> Action n r s)
  | ALift (n (Action n r s))
  | AYield  (Action n r s)
  | AReturn (Action n r s)
  | ACommit ThreadId CRefId
  | AStop (n ())

--------------------------------------------------------------------------------
-- * Scheduling & Traces

-- | Look as far ahead in the given continuation as possible.
lookahead :: Action n r s -> NonEmpty Lookahead
lookahead = fromList . lookahead' where
  lookahead' (AFork _ _ _)           = [WillFork]
  lookahead' (AMyTId _)              = [WillMyThreadId]
  lookahead' (AGetNumCapabilities _) = [WillGetNumCapabilities]
  lookahead' (ASetNumCapabilities i k) = WillSetNumCapabilities i : lookahead' k
  lookahead' (ANewVar _ _)           = [WillNewVar]
  lookahead' (APutVar (MVar c _) _ k)    = WillPutVar c : lookahead' k
  lookahead' (ATryPutVar (MVar c _) _ _) = [WillTryPutVar c]
  lookahead' (AReadVar (MVar c _) _)     = [WillReadVar c]
  lookahead' (ATakeVar (MVar c _) _)     = [WillTakeVar c]
  lookahead' (ATryTakeVar (MVar c _) _)  = [WillTryTakeVar c]
  lookahead' (ANewRef _ _ _)         = [WillNewRef]
  lookahead' (AReadRef (CRef r _) _)     = [WillReadRef r]
  lookahead' (AReadRefCas (CRef r _) _)  = [WillReadRefCas r]
  lookahead' (APeekTicket (Ticket r _ _) _) = [WillPeekTicket r]
  lookahead' (AModRef (CRef r _) _ _)    = [WillModRef r]
  lookahead' (AModRefCas (CRef r _) _ _) = [WillModRefCas r]
  lookahead' (AWriteRef (CRef r _) _ k) = WillWriteRef r : lookahead' k
  lookahead' (ACasRef (CRef r _) _ _ _) = [WillCasRef r]
  lookahead' (ACommit t c)           = [WillCommitRef t c]
  lookahead' (AAtom _ _)             = [WillSTM]
  lookahead' (AThrow _)              = [WillThrow]
  lookahead' (AThrowTo tid _ k)      = WillThrowTo tid : lookahead' k
  lookahead' (ACatching _ _ _)       = [WillCatching]
  lookahead' (APopCatching k)        = WillPopCatching : lookahead' k
  lookahead' (AMasking ms _ _)       = [WillSetMasking False ms]
  lookahead' (AResetMask b1 b2 ms k) = (if b1 then WillSetMasking else WillResetMasking) b2 ms : lookahead' k
  lookahead' (ALift _)               = [WillLiftIO]
  lookahead' (AKnowsAbout _ k)       = WillKnowsAbout : lookahead' k
  lookahead' (AForgets _ k)          = WillForgets : lookahead' k
  lookahead' (AAllKnown k)           = WillAllKnown : lookahead' k
  lookahead' (AMessage m k)          = WillMessage m : lookahead' k
  lookahead' (AYield k)              = WillYield : lookahead' k
  lookahead' (AReturn k)             = WillReturn : lookahead' k
  lookahead' (AStop _)               = [WillStop]

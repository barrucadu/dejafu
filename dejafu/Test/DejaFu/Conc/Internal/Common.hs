{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal.Common
-- Copyright   : (c) 2016--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, ExistentialQuantification, RankNTypes
--
-- Common types and utility functions for deterministic execution of
-- 'MonadConc' implementations. This module is NOT considered to form
-- part of the public interface of this library.
module Test.DejaFu.Conc.Internal.Common where

import           Control.Exception             (Exception, MaskingState(..))
import           Data.Map.Strict               (Map)
import           Test.DejaFu.Conc.Internal.STM (ModelSTM)
import           Test.DejaFu.Types

#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail            as Fail
#endif

--------------------------------------------------------------------------------
-- * The @ModelConc@ Monad

-- | The underlying monad is based on continuations over 'Action's.
--
-- One might wonder why the return type isn't reflected in 'Action',
-- and a free monad formulation used. This would remove the need for a
-- @AStop@ actions having their parameter. However, this makes the
-- current expression of threads and exception handlers very difficult
-- (perhaps even not possible without significant reworking), so I
-- abandoned the attempt.
newtype ModelConc n r a = ModelConc { runModelConc :: (a -> Action n r) -> Action n r }

instance Functor (ModelConc n r) where
    fmap f m = ModelConc $ \ c -> runModelConc m (c . f)

instance Applicative (ModelConc n r) where
    -- without the @AReturn@, a thread could lock up testing by
    -- entering an infinite loop (eg: @forever (return ())@)
    pure x  = ModelConc $ \c -> AReturn $ c x
    f <*> v = ModelConc $ \c -> runModelConc f (\g -> runModelConc v (c . g))

instance Monad (ModelConc n r) where
    return  = pure
    m >>= k = ModelConc $ \c -> runModelConc m (\x -> runModelConc (k x) c)

#if MIN_VERSION_base(4,9,0)
    fail = Fail.fail

-- | @since 0.7.1.2
instance Fail.MonadFail (ModelConc n r) where
#endif
    fail e = ModelConc $ \_ -> AThrow (MonadFailException e)

-- | An @MVar@ is modelled as a unique ID and a reference holding a
-- @Maybe@ value.
data ModelMVar r a = ModelMVar
  { _mvarId  :: MVarId
  , _mvarRef :: r (Maybe a)
  }

-- | A @CRef@ is modelled as a unique ID and a reference holding
-- thread-local values, the number of commits, and the most recent
-- committed value.
data ModelCRef r a = ModelCRef
  { _crefId  :: CRefId
  , _crefRef :: r (Map ThreadId a, Integer, a)
  }

-- | A @Ticket@ is modelled as the ID of the @ModelCRef@ it came from,
-- the commits to the @ModelCRef@ at the time it was produced, and the
-- value observed.
data ModelTicket a = ModelTicket
  { _ticketCRef   :: CRefId
  , _ticketWrites :: Integer
  , _ticketVal    :: a
  }

--------------------------------------------------------------------------------
-- * Primitive Actions

-- | Scheduling is done in terms of a trace of 'Action's. Blocking can
-- only occur as a result of an action, and they cover (most of) the
-- primitives of the concurrency. 'spawn' is absent as it is
-- implemented in terms of 'newEmptyMVar', 'fork', and 'putMVar'.
data Action n r =
    AFork   String ((forall b. ModelConc n r b -> ModelConc n r b) -> Action n r) (ThreadId -> Action n r)
  | AForkOS String ((forall b. ModelConc n r b -> ModelConc n r b) -> Action n r) (ThreadId -> Action n r)
  | AIsBound (Bool -> Action n r)
  | AMyTId (ThreadId -> Action n r)

  | AGetNumCapabilities (Int -> Action n r)
  | ASetNumCapabilities Int (Action n r)

  | forall a. ANewMVar String (ModelMVar r a -> Action n r)
  | forall a. APutMVar     (ModelMVar r a) a (Action n r)
  | forall a. ATryPutMVar  (ModelMVar r a) a (Bool -> Action n r)
  | forall a. AReadMVar    (ModelMVar r a) (a -> Action n r)
  | forall a. ATryReadMVar (ModelMVar r a) (Maybe a -> Action n r)
  | forall a. ATakeMVar    (ModelMVar r a) (a -> Action n r)
  | forall a. ATryTakeMVar (ModelMVar r a) (Maybe a -> Action n r)

  | forall a.   ANewCRef String a (ModelCRef r a -> Action n r)
  | forall a.   AReadCRef    (ModelCRef r a) (a -> Action n r)
  | forall a.   AReadCRefCas (ModelCRef r a) (ModelTicket a -> Action n r)
  | forall a b. AModCRef     (ModelCRef r a) (a -> (a, b)) (b -> Action n r)
  | forall a b. AModCRefCas  (ModelCRef r a) (a -> (a, b)) (b -> Action n r)
  | forall a.   AWriteCRef   (ModelCRef r a) a (Action n r)
  | forall a.   ACasCRef     (ModelCRef r a) (ModelTicket a) a ((Bool, ModelTicket a) -> Action n r)

  | forall e.   Exception e => AThrow e
  | forall e.   Exception e => AThrowTo ThreadId e (Action n r)
  | forall a e. Exception e => ACatching (e -> ModelConc n r a) (ModelConc n r a) (a -> Action n r)
  | APopCatching (Action n r)
  | forall a. AMasking MaskingState ((forall b. ModelConc n r b -> ModelConc n r b) -> ModelConc n r a) (a -> Action n r)
  | AResetMask Bool Bool MaskingState (Action n r)

  | forall a. AAtom (ModelSTM n r a) (a -> Action n r)
  | ALift (n (Action n r))
  | AYield  (Action n r)
  | ADelay Int (Action n r)
  | AReturn (Action n r)
  | ACommit ThreadId CRefId
  | AStop (n ())

  | forall a. ASub (ModelConc n r a) (Either Failure a -> Action n r)
  | AStopSub (Action n r)
  | forall a. ADontCheck (Maybe Int) (ModelConc n r a) (a -> Action n r)

--------------------------------------------------------------------------------
-- * Scheduling & Traces

-- | Look as far ahead in the given continuation as possible.
lookahead :: Action n r -> Lookahead
lookahead (AFork _ _ _) = WillFork
lookahead (AForkOS _ _ _) = WillForkOS
lookahead (AIsBound _) = WillIsCurrentThreadBound
lookahead (AMyTId _) = WillMyThreadId
lookahead (AGetNumCapabilities _) = WillGetNumCapabilities
lookahead (ASetNumCapabilities i _) = WillSetNumCapabilities i
lookahead (ANewMVar _ _) = WillNewMVar
lookahead (APutMVar (ModelMVar m _) _ _) = WillPutMVar m
lookahead (ATryPutMVar (ModelMVar m _) _ _) = WillTryPutMVar m
lookahead (AReadMVar (ModelMVar m _) _) = WillReadMVar m
lookahead (ATryReadMVar (ModelMVar m _) _) = WillTryReadMVar m
lookahead (ATakeMVar (ModelMVar m _) _) = WillTakeMVar m
lookahead (ATryTakeMVar (ModelMVar m _) _) = WillTryTakeMVar m
lookahead (ANewCRef _ _ _) = WillNewCRef
lookahead (AReadCRef (ModelCRef r _) _) = WillReadCRef r
lookahead (AReadCRefCas (ModelCRef r _) _) = WillReadCRefCas r
lookahead (AModCRef (ModelCRef r _) _ _) = WillModCRef r
lookahead (AModCRefCas (ModelCRef r _) _ _) = WillModCRefCas r
lookahead (AWriteCRef (ModelCRef r _) _ _) = WillWriteCRef r
lookahead (ACasCRef (ModelCRef r _) _ _ _) = WillCasCRef r
lookahead (ACommit t c) = WillCommitCRef t c
lookahead (AAtom _ _) = WillSTM
lookahead (AThrow _) = WillThrow
lookahead (AThrowTo tid _ _) = WillThrowTo tid
lookahead (ACatching _ _ _) = WillCatching
lookahead (APopCatching _) = WillPopCatching
lookahead (AMasking ms _ _) = WillSetMasking False ms
lookahead (AResetMask b1 b2 ms _) = (if b1 then WillSetMasking else WillResetMasking) b2 ms
lookahead (ALift _) = WillLiftIO
lookahead (AYield _) = WillYield
lookahead (ADelay n _) = WillThreadDelay n
lookahead (AReturn _) = WillReturn
lookahead (AStop _) = WillStop
lookahead (ASub _ _) = WillSubconcurrency
lookahead (AStopSub _) = WillStopSubconcurrency
lookahead (ADontCheck _ _ _) = WillDontCheck

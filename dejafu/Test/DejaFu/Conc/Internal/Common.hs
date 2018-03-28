{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal.Common
-- Copyright   : (c) 2016--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : ExistentialQuantification, RankNTypes
--
-- Common types and utility functions for deterministic execution of
-- 'MonadConc' implementations. This module is NOT considered to form
-- part of the public interface of this library.
module Test.DejaFu.Conc.Internal.Common where

import           Control.Exception             (Exception, MaskingState(..))
import qualified Control.Monad.Conc.Class      as C
import qualified Control.Monad.Fail            as Fail
import           Data.Map.Strict               (Map)
import           Test.DejaFu.Conc.Internal.STM (ModelSTM)
import           Test.DejaFu.Types

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
newtype ModelConc n a = ModelConc { runModelConc :: (a -> Action n) -> Action n }

instance Functor (ModelConc n) where
    fmap f m = ModelConc $ \c -> runModelConc m (c . f)

instance Applicative (ModelConc n) where
    -- without the @AReturn@, a thread could lock up testing by
    -- entering an infinite loop (eg: @forever (return ())@)
    pure x  = ModelConc $ \c -> AReturn $ c x
    f <*> v = ModelConc $ \c -> runModelConc f (\g -> runModelConc v (c . g))

instance Monad (ModelConc n) where
    return  = pure
    m >>= k = ModelConc $ \c -> runModelConc m (\x -> runModelConc (k x) c)

    fail = Fail.fail

instance Fail.MonadFail (ModelConc n) where
    fail e = ModelConc $ \_ -> AThrow (MonadFailException e)

-- | An @MVar@ is modelled as a unique ID and a reference holding a
-- @Maybe@ value.
data ModelMVar n a = ModelMVar
  { mvarId  :: MVarId
  , mvarRef :: C.CRef n (Maybe a)
  }

-- | A @CRef@ is modelled as a unique ID and a reference holding
-- thread-local values, the number of commits, and the most recent
-- committed value.
data ModelCRef n a = ModelCRef
  { crefId  :: CRefId
  , crefRef :: C.CRef n (Map ThreadId a, Integer, a)
  }

-- | A @Ticket@ is modelled as the ID of the @ModelCRef@ it came from,
-- the commits to the @ModelCRef@ at the time it was produced, and the
-- value observed.
data ModelTicket a = ModelTicket
  { ticketCRef   :: CRefId
  , ticketWrites :: Integer
  , ticketVal    :: a
  }

--------------------------------------------------------------------------------
-- * Primitive Actions

-- | Scheduling is done in terms of a trace of 'Action's. Blocking can
-- only occur as a result of an action, and they cover (most of) the
-- primitives of the concurrency. 'spawn' is absent as it is
-- implemented in terms of 'newEmptyMVar', 'fork', and 'putMVar'.
data Action n =
    AFork   String ((forall b. ModelConc n b -> ModelConc n b) -> Action n) (ThreadId -> Action n)
  | AForkOS String ((forall b. ModelConc n b -> ModelConc n b) -> Action n) (ThreadId -> Action n)
  | AIsBound (Bool -> Action n)
  | AMyTId (ThreadId -> Action n)

  | AGetNumCapabilities (Int -> Action n)
  | ASetNumCapabilities Int (Action n)

  | forall a. ANewMVar String (ModelMVar n a -> Action n)
  | forall a. APutMVar     (ModelMVar n a) a (Action n)
  | forall a. ATryPutMVar  (ModelMVar n a) a (Bool -> Action n)
  | forall a. AReadMVar    (ModelMVar n a) (a -> Action n)
  | forall a. ATryReadMVar (ModelMVar n a) (Maybe a -> Action n)
  | forall a. ATakeMVar    (ModelMVar n a) (a -> Action n)
  | forall a. ATryTakeMVar (ModelMVar n a) (Maybe a -> Action n)

  | forall a.   ANewCRef String a (ModelCRef n a -> Action n)
  | forall a.   AReadCRef    (ModelCRef n a) (a -> Action n)
  | forall a.   AReadCRefCas (ModelCRef n a) (ModelTicket a -> Action n)
  | forall a b. AModCRef     (ModelCRef n a) (a -> (a, b)) (b -> Action n)
  | forall a b. AModCRefCas  (ModelCRef n a) (a -> (a, b)) (b -> Action n)
  | forall a.   AWriteCRef   (ModelCRef n a) a (Action n)
  | forall a.   ACasCRef     (ModelCRef n a) (ModelTicket a) a ((Bool, ModelTicket a) -> Action n)

  | forall e.   Exception e => AThrow e
  | forall e.   Exception e => AThrowTo ThreadId e (Action n)
  | forall a e. Exception e => ACatching (e -> ModelConc n a) (ModelConc n a) (a -> Action n)
  | APopCatching (Action n)
  | forall a. AMasking MaskingState ((forall b. ModelConc n b -> ModelConc n b) -> ModelConc n a) (a -> Action n)
  | AResetMask Bool Bool MaskingState (Action n)

  | forall a. AAtom (ModelSTM n a) (a -> Action n)
  | ALift (n (Action n))
  | AYield  (Action n)
  | ADelay Int (Action n)
  | AReturn (Action n)
  | ACommit ThreadId CRefId
  | AStop (n ())

  | forall a. ASub (ModelConc n a) (Either Failure a -> Action n)
  | AStopSub (Action n)
  | forall a. ADontCheck (Maybe Int) (ModelConc n a) (a -> Action n)

--------------------------------------------------------------------------------
-- * Scheduling & Traces

-- | Look as far ahead in the given continuation as possible.
lookahead :: Action n -> Lookahead
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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal.Common
-- Copyright   : (c) 2016--2019 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : ExistentialQuantification, GADTs, RankNTypes
--
-- Common types and utility functions for deterministic execution of
-- 'MonadConc' implementations. This module is NOT considered to form
-- part of the public interface of this library.
module Test.DejaFu.Conc.Internal.Common where

import           Control.Exception             (Exception, MaskingState(..))
import           Control.Monad.Catch           (MonadCatch(..), MonadThrow(..))
import qualified Control.Monad.Conc.Class      as C
import qualified Control.Monad.Fail            as Fail
import           Data.Map.Strict               (Map)

import           Test.DejaFu.Conc.Internal.STM (ModelSTM, ModelTVar)
import           Test.DejaFu.Types

--------------------------------------------------------------------------------
-- * Types for Modelling Concurrency

-- | The underlying monad is based on continuations over 'Action's.
--
-- One might wonder why the return type isn't reflected in 'Action',
-- and a free monad formulation used. This would remove the need for a
-- @AStop@ actions having their parameter. However, this makes the
-- current expression of threads and exception handlers very difficult
-- (perhaps even not possible without significant reworking), so I
-- abandoned the attempt.
type ModelConc = Program Basic

-- | A representation of a concurrent program for testing.
--
-- To construct these, use the 'C.MonadConc' instance, or see
-- 'Test.DejaFu.Conc.withSetup', 'Test.DejaFu.Conc.withTeardown', and
-- 'Test.DejaFu.Conc.withSetupAndTeardown'.
--
-- @since unreleased
data Program pty n a where
  ModelConc ::
    { runModelConc :: (a -> Action n) -> Action n
    } -> Program Basic n a
  WithSetup ::
    { wsSetup   :: ModelConc n x
    , wsProgram :: x -> ModelConc n a
    } -> Program (WithSetup x) n a
  WithSetupAndTeardown ::
    { wstSetup    :: ModelConc n x
    , wstProgram  :: x -> ModelConc n y
    , wstTeardown :: x -> Either Condition y -> ModelConc n a
    } -> Program (WithSetupAndTeardown x y) n a

-- | A type used to constrain 'Program': a @Program Basic@ is a
-- \"basic\" program with no set-up or teardown.
--
-- Construct with the 'MonadConc' instance or with 'basic'.
--
-- @since unreleased
data Basic

-- | A type used to constrain 'Program': a @Program (WithSetup x)@ is
-- a program with some set-up action producing a value of type @x@.
--
-- Construct with 'Test.DejaFu.Conc.withSetup'.
--
-- @since unreleased
data WithSetup x

-- | A type used to constrain 'Program': a @Program
-- (WithSetupAndTeardown x y)@ is a program producing a value of type
-- @y@ with some set-up action producing a value of type @x@ and a
-- teardown action producing the final result.
--
-- Construct with 'Test.DejaFu.Conc.withTeardown' or
-- 'Test.DejaFu.Conc.withSetupAndTeardown'.
--
-- @since unreleased
data WithSetupAndTeardown x y

instance (pty ~ Basic) => Functor (Program pty n) where
  fmap f m = ModelConc $ \c -> runModelConc m (c . f)

instance (pty ~ Basic) => Applicative (Program pty n) where
  -- without the @AReturn@, a thread could lock up testing by entering
  -- an infinite loop (eg: @forever (return ())@)
  pure x  = ModelConc $ \c -> AReturn $ c x
  f <*> v = ModelConc $ \c -> runModelConc f (\g -> runModelConc v (c . g))

instance (pty ~ Basic) => Monad (Program pty n) where
  return  = pure
  fail    = Fail.fail
  m >>= k = ModelConc $ \c -> runModelConc m (\x -> runModelConc (k x) c)

instance (pty ~ Basic) => Fail.MonadFail (Program pty n) where
  fail e = ModelConc $ \_ -> AThrow (MonadFailException e)

-- | An @MVar@ is modelled as a unique ID and a reference holding a
-- @Maybe@ value.
data ModelMVar n a = ModelMVar
  { mvarId  :: MVarId
  , mvarRef :: C.IORef n (Maybe a)
  }

-- | A @IORef@ is modelled as a unique ID and a reference holding
-- thread-local values, the number of commits, and the most recent
-- committed value.
data ModelIORef n a = ModelIORef
  { iorefId  :: IORefId
  , iorefRef :: C.IORef n (Map ThreadId a, Integer, a)
  }

-- | A @Ticket@ is modelled as the ID of the @ModelIORef@ it came from,
-- the commits to the @ModelIORef@ at the time it was produced, and the
-- value observed.
data ModelTicket a = ModelTicket
  { ticketIORef  :: IORefId
  , ticketWrites :: Integer
  , ticketVal    :: a
  }

--------------------------------------------------------------------------------
-- ** Primitive Actions

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

  | forall a.   ANewIORef String a (ModelIORef n a -> Action n)
  | forall a.   AReadIORef    (ModelIORef n a) (a -> Action n)
  | forall a.   AReadIORefCas (ModelIORef n a) (ModelTicket a -> Action n)
  | forall a b. AModIORef     (ModelIORef n a) (a -> (a, b)) (b -> Action n)
  | forall a b. AModIORefCas  (ModelIORef n a) (a -> (a, b)) (b -> Action n)
  | forall a.   AWriteIORef   (ModelIORef n a) a (Action n)
  | forall a.   ACasIORef     (ModelIORef n a) (ModelTicket a) a ((Bool, ModelTicket a) -> Action n)

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
  | ACommit ThreadId IORefId
  | AStop (n ())

  | ANewInvariant (Invariant n ()) (Action n)

--------------------------------------------------------------------------------
-- ** Scheduling & Traces

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
lookahead (ANewIORef _ _ _) = WillNewIORef
lookahead (AReadIORef (ModelIORef r _) _) = WillReadIORef r
lookahead (AReadIORefCas (ModelIORef r _) _) = WillReadIORefCas r
lookahead (AModIORef (ModelIORef r _) _ _) = WillModIORef r
lookahead (AModIORefCas (ModelIORef r _) _ _) = WillModIORefCas r
lookahead (AWriteIORef (ModelIORef r _) _ _) = WillWriteIORef r
lookahead (ACasIORef (ModelIORef r _) _ _ _) = WillCasIORef r
lookahead (ACommit t c) = WillCommitIORef t c
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
lookahead (ANewInvariant _ _) = WillRegisterInvariant

-------------------------------------------------------------------------------
-- * Invariants

-- | Invariants are atomic actions which can inspect the shared state
-- of your computation, and terminate it on failure.  Invariants have
-- no visible effects, and are checked after each scheduling point.
--
-- To be checked, an invariant must be created during the setup phase
-- of your 'Program', using 'Test.DejaFu.Conc.registerInvariant'.  The
-- invariant will then be checked in the main phase (but not in the
-- setup or teardown phase).  As a consequence of this, any shared
-- state you want your invariant to check must also be created in the
-- setup phase, and passed into the main phase as a parameter.
--
-- @since unreleased
newtype Invariant n a = Invariant { runInvariant :: (a -> IAction n) -> IAction n }

instance Functor (Invariant n) where
  fmap f m = Invariant $ \c -> runInvariant m (c . f)

instance Applicative (Invariant n) where
  pure x  = Invariant $ \c -> c x
  f <*> v = Invariant $ \c -> runInvariant f (\g -> runInvariant v (c . g))

instance Monad (Invariant n) where
  return  = pure
  fail    = Fail.fail
  m >>= k = Invariant $ \c -> runInvariant m (\x -> runInvariant (k x) c)

instance Fail.MonadFail (Invariant n) where
  fail e = Invariant $ \_ -> IThrow (MonadFailException e)

instance MonadThrow (Invariant n) where
  throwM e = Invariant $ \_ -> IThrow e

instance MonadCatch (Invariant n) where
  catch stm handler = Invariant $ ICatch handler stm

-- | Invariants are represented as a sequence of primitive actions.
data IAction n
  = forall a. IInspectIORef (ModelIORef n a) (a -> IAction n)
  | forall a. IInspectMVar  (ModelMVar  n a) (Maybe a -> IAction n)
  | forall a. IInspectTVar  (ModelTVar  n a) (a -> IAction n)
  | forall a e. Exception e => ICatch (e -> Invariant n a) (Invariant n a) (a -> IAction n)
  | forall e. Exception e => IThrow e
  | IStop (n ())

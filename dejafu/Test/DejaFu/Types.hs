{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Test.DejaFu.Types
-- Copyright   : (c) 2017--2020 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, RankNTypes, StandaloneDeriving, TypeFamilies
--
-- Common types and functions used throughout DejaFu.
module Test.DejaFu.Types where

import qualified Control.Concurrent                   as IO
import           Control.DeepSeq                      (NFData(..))
import           Control.Exception                    (Exception(..),
                                                       MaskingState(..),
                                                       SomeException)
import           Control.Monad                        (forever)
import           Control.Monad.Catch                  (MonadThrow)
import           Control.Monad.Catch.Pure             (CatchT)
import qualified Control.Monad.ST                     as ST
import           Control.Monad.Trans.Class            (lift)
import           Data.Function                        (on)
import           Data.Functor.Contravariant           (Contravariant(..))
import           Data.Functor.Contravariant.Divisible (Divisible(..))
import qualified Data.IORef                           as IO
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as M
import           Data.Semigroup                       (Semigroup(..))
import           Data.Set                             (Set)
import qualified Data.Set                             as S
import qualified Data.STRef                           as ST
import           GHC.Generics                         (Generic, V1)

-------------------------------------------------------------------------------
-- * The @MonadDejaFu@ typeclass

-- | The @MonadDejaFu@ class captures the two things needed to run a
-- concurrent program which we can't implement in normal Haskell:
-- mutable references, and the ability to create a bound thread in
-- @IO@.
--
-- In addition to needing the operations in this class, dejafu also
-- needs the ability to throw exceptions, as these are used to
-- communicate 'Error's, so there is a 'MonadThrow' constraint.
--
-- @since 2.1.0.0
class MonadThrow m => MonadDejaFu m where
  -- | The type of mutable references.  These references will always
  -- contain a value, and so don't need to handle emptiness (like
  -- @MVar@ does).
  --
  -- These references are always used from the same Haskell thread, so
  -- it's safe to implement these using unsynchronised primitives with
  -- relaxed-memory behaviours (like @IORef@s).
  type Ref m :: * -> *

  -- | Create a new reference holding a given initial value.
  newRef :: a -> m (Ref m a)

  -- | Read the current value in the reference.
  readRef :: Ref m a -> m a

  -- | Replace the value in the reference.
  writeRef :: Ref m a -> a -> m ()

  -- | A handle to a bound thread.  If the monad doesn't support bound
  -- threads (for example, if it's not based on @IO@), then this
  -- should be some type which can't be constructed, like 'V1'.
  type BoundThread m :: * -> *

  -- | Fork a new bound thread, if the monad supports them.
  forkBoundThread :: Maybe (m (BoundThread m a))

  -- | Run an action in a previously created bound thread.
  runInBoundThread :: BoundThread m a -> m a -> m a

  -- | Terminate a previously created bound thread.
  --
  -- After termination, 'runInBoundThread' and 'killBoundThread' will
  -- never be called on this @BoundThread m a@ value again.
  killBoundThread :: BoundThread m a -> m ()

-- | A bound thread in @IO@.
--
-- @since 2.1.0.0
data IOBoundThread a = IOBoundThread
  { iobtRunInBoundThread :: IO a -> IO a
    -- ^ Pass an action to the bound thread, run it, and return the
    -- result to this thread.
  , iobtKillBoundThread  :: IO ()
    -- ^ Terminate the bound thread.
  }

-- | @since 2.1.0.0
instance MonadDejaFu IO where
  type Ref IO = IO.IORef

  newRef   = IO.newIORef
  readRef  = IO.readIORef
  writeRef = IO.writeIORef

  type BoundThread IO = IOBoundThread

  forkBoundThread = Just $ do
      runboundIO <- IO.newEmptyMVar
      getboundIO <- IO.newEmptyMVar
      tid <- IO.forkOS (go runboundIO getboundIO)
      pure IOBoundThread
        { iobtRunInBoundThread = run runboundIO getboundIO
        , iobtKillBoundThread  = IO.killThread tid
        }
    where
      go runboundIO getboundIO = forever $ do
        na <- IO.takeMVar runboundIO
        IO.putMVar getboundIO =<< na

      run runboundIO getboundIO ma = do
        IO.putMVar runboundIO ma
        IO.takeMVar getboundIO

  runInBoundThread = iobtRunInBoundThread
  killBoundThread  = iobtKillBoundThread

-- | This instance does not support bound threads.
--
-- @since 2.1.0.0
instance MonadDejaFu (CatchT (ST.ST t)) where
  type Ref (CatchT (ST.ST t)) = ST.STRef t

  newRef     = lift . ST.newSTRef
  readRef    = lift . ST.readSTRef
  writeRef r = lift . ST.writeSTRef r

  -- V1 has no constructors
  type BoundThread (CatchT (ST.ST t)) = V1

  forkBoundThread  = Nothing
  runInBoundThread = undefined
  killBoundThread  = undefined

-------------------------------------------------------------------------------
-- * Identifiers

-- | Every thread has a unique identitifer.
--
-- @since 1.0.0.0
newtype ThreadId = ThreadId Id
  deriving (Eq, Ord, NFData)

instance Show ThreadId where
  show (ThreadId id_) = show id_

-- | @since 1.3.1.0
deriving instance Generic ThreadId

-- | Every @IORef@ has a unique identifier.
--
-- @since 1.11.0.0
newtype IORefId = IORefId Id
  deriving (Eq, Ord, NFData, Generic)

instance Show IORefId where
  show (IORefId id_) = show id_

-- | Every @MVar@ has a unique identifier.
--
-- @since 1.0.0.0
newtype MVarId = MVarId Id
  deriving (Eq, Ord, NFData)

instance Show MVarId where
  show (MVarId id_) = show id_

-- | @since 1.3.1.0
deriving instance Generic MVarId

-- | Every @TVar@ has a unique identifier.
--
-- @since 1.0.0.0
newtype TVarId = TVarId Id
  deriving (Eq, Ord, NFData)

instance Show TVarId where
  show (TVarId id_) = show id_

-- | @since 1.3.1.0
deriving instance Generic TVarId

-- | An identifier for a thread, @MVar@, @IORef@, or @TVar@.
--
-- The number is the important bit.  The string is to make execution
-- traces easier to read, but is meaningless.
--
-- @since 1.0.0.0
data Id = Id (Maybe String) {-# UNPACK #-} !Int

instance Eq Id where
  (Id _ i) == (Id _ j) = i == j

instance Ord Id where
  compare (Id _ i) (Id _ j) = compare i j

instance Show Id where
  show (Id (Just n) _) = n
  show (Id _ i) = show i

-- | @since 1.3.1.0
deriving instance Generic Id

instance NFData Id

-- | The ID of the initial thread.
--
-- @since 0.4.0.0
initialThread :: ThreadId
initialThread = ThreadId (Id (Just "main") 0)

-------------------------------------------------------------------------------
-- * Actions

-- | All the actions that a thread can perform.
--
-- @since 2.2.0.0
data ThreadAction =
    Fork ThreadId
  -- ^ Start a new thread.
  | ForkOS ThreadId
  -- ^ Start a new bound thread.
  | SupportsBoundThreads Bool
  -- ^ Check if bound threads are supported.
  | IsCurrentThreadBound Bool
  -- ^ Check if the current thread is bound.
  | MyThreadId
  -- ^ Get the 'ThreadId' of the current thread.
  | GetNumCapabilities Int
  -- ^ Get the number of Haskell threads that can run simultaneously.
  | SetNumCapabilities Int
  -- ^ Set the number of Haskell threads that can run simultaneously.
  | Yield
  -- ^ Yield the current thread.
  | ThreadDelay Int
  -- ^ Yield/delay the current thread.
  | NewMVar MVarId
  -- ^ Create a new 'MVar'.
  | PutMVar MVarId [ThreadId]
  -- ^ Put into a 'MVar', possibly waking up some threads.
  | BlockedPutMVar MVarId
  -- ^ Get blocked on a put.
  | TryPutMVar MVarId Bool [ThreadId]
  -- ^ Try to put into a 'MVar', possibly waking up some threads.
  | ReadMVar MVarId
  -- ^ Read from a 'MVar'.
  | TryReadMVar MVarId Bool
  -- ^ Try to read from a 'MVar'.
  | BlockedReadMVar MVarId
  -- ^ Get blocked on a read.
  | TakeMVar MVarId [ThreadId]
  -- ^ Take from a 'MVar', possibly waking up some threads.
  | BlockedTakeMVar MVarId
  -- ^ Get blocked on a take.
  | TryTakeMVar MVarId Bool [ThreadId]
  -- ^ Try to take from a 'MVar', possibly waking up some threads.
  | NewIORef IORefId
  -- ^ Create a new 'IORef'.
  | ReadIORef IORefId
  -- ^ Read from a 'IORef'.
  | ReadIORefCas IORefId
  -- ^ Read from a 'IORef' for a future compare-and-swap.
  | ModIORef IORefId
  -- ^ Modify a 'IORef'.
  | ModIORefCas IORefId
  -- ^ Modify a 'IORef' using a compare-and-swap.
  | WriteIORef IORefId
  -- ^ Write to a 'IORef' without synchronising.
  | CasIORef IORefId Bool
  -- ^ Attempt to to a 'IORef' using a compare-and-swap, synchronising
  -- it.
  | CommitIORef ThreadId IORefId
  -- ^ Commit the last write to the given 'IORef' by the given thread,
  -- so that all threads can see the updated value.
  | STM [TAction] [ThreadId]
  -- ^ An STM transaction was executed, possibly waking up some
  -- threads.
  | ThrownSTM [TAction] (Maybe MaskingState) Bool
  -- ^ An STM transaction threw an exception.  Give the resultant
  -- masking state after jumping to the exception handler (if it
  -- changed).  If the 'Bool' is @True@, then this killed the thread.
  | BlockedSTM [TAction]
  -- ^ Got blocked in an STM transaction.
  | Catching
  -- ^ Register a new exception handler
  | PopCatching
  -- ^ Pop the innermost exception handler from the stack.
  | Throw (Maybe MaskingState) Bool
  -- ^ Throw an exception, and give the resultant masking state after
  -- jumping to the exception handler (if it changed).  If the 'Bool'
  -- is @True@, then this killed the thread.
  | ThrowTo ThreadId (Maybe MaskingState) Bool
  -- ^ Throw an exception to a thread, and give the resultant masking
  -- state after jumping to the exception handler (if it changed).  If
  -- the 'Bool' is @True@, then this killed the thread.
  | BlockedThrowTo ThreadId
  -- ^ Get blocked on a 'throwTo'.
  | SetMasking Bool MaskingState
  -- ^ Set the masking state. If 'True', this is being used to set the
  -- masking state to the original state in the argument passed to a
  -- 'mask'ed function.
  | ResetMasking Bool MaskingState
  -- ^ Return to an earlier masking state.  If 'True', this is being
  -- used to return to the state of the masked block in the argument
  -- passed to a 'mask'ed function.
  | GetMaskingState MaskingState
  -- ^ Get the current masking state.
  | LiftIO
  -- ^ Lift an IO action. Note that this can only happen with
  -- 'ConcIO'.
  | Return
  -- ^ A 'return' or 'pure' action was executed.
  | Stop
  -- ^ Cease execution and terminate.
  | RegisterInvariant
  -- ^ Register an invariant.
  deriving (Eq, Generic, Show)

-- this makes me sad
instance NFData ThreadAction where
  rnf (Fork t) = rnf t
  rnf (ForkOS t) = rnf t
  rnf (SupportsBoundThreads b) = rnf b
  rnf (IsCurrentThreadBound b) = rnf b
  rnf MyThreadId = ()
  rnf (GetNumCapabilities i) = rnf i
  rnf (SetNumCapabilities i) = rnf i
  rnf Yield = ()
  rnf (ThreadDelay i) = rnf i
  rnf (NewMVar m) = rnf m
  rnf (PutMVar m ts) = rnf (m, ts)
  rnf (BlockedPutMVar m) = rnf m
  rnf (TryPutMVar m b ts) = rnf (m, b, ts)
  rnf (ReadMVar m) = rnf m
  rnf (TryReadMVar m b) = rnf (m, b)
  rnf (BlockedReadMVar m) = rnf m
  rnf (TakeMVar m ts) = rnf (m, ts)
  rnf (BlockedTakeMVar m) = rnf m
  rnf (TryTakeMVar m b ts) = rnf (m, b, ts)
  rnf (NewIORef c) = rnf c
  rnf (ReadIORef c) = rnf c
  rnf (ReadIORefCas c) = rnf c
  rnf (ModIORef c) = rnf c
  rnf (ModIORefCas c) = rnf c
  rnf (WriteIORef c) = rnf c
  rnf (CasIORef c b) = rnf (c, b)
  rnf (CommitIORef t c) = rnf (t, c)
  rnf (STM as ts) = rnf (as, ts)
  rnf (ThrownSTM as (Just m) b) = m `seq` rnf (as, b)
  rnf (ThrownSTM as Nothing b) = rnf (as, b)
  rnf (BlockedSTM as) = rnf as
  rnf Catching = ()
  rnf PopCatching = ()
  rnf (Throw (Just m) b) = m `seq` rnf b
  rnf (Throw Nothing b) = rnf b
  rnf (ThrowTo t (Just m) b) = m `seq` rnf (t, b)
  rnf (ThrowTo t Nothing b) = rnf (t, b)
  rnf (BlockedThrowTo t) = rnf t
  rnf (SetMasking b m) = rnf (b, show m)
  rnf (ResetMasking b m) = rnf (b, show m)
  -- deepseq<1.4.4.0 doesn't have an instance for MaskingState
  rnf (GetMaskingState m) = m `seq` ()
  rnf LiftIO = ()
  rnf Return = ()
  rnf Stop = ()
  rnf RegisterInvariant = ()

-- | A one-step look-ahead at what a thread will do next.
--
-- @since 2.2.0.0
data Lookahead =
    WillFork
  -- ^ Will start a new thread.
  | WillForkOS
  -- ^ Will start a new bound thread.
  | WillSupportsBoundThreads
  -- ^ Will check if bound threads are supported.
  | WillIsCurrentThreadBound
  -- ^ Will check if the current thread is bound.
  | WillMyThreadId
  -- ^ Will get the 'ThreadId'.
  | WillGetNumCapabilities
  -- ^ Will get the number of Haskell threads that can run
  -- simultaneously.
  | WillSetNumCapabilities Int
  -- ^ Will set the number of Haskell threads that can run
  -- simultaneously.
  | WillYield
  -- ^ Will yield the current thread.
  | WillThreadDelay Int
  -- ^ Will yield/delay the current thread.
  | WillNewMVar
  -- ^ Will create a new 'MVar'.
  | WillPutMVar MVarId
  -- ^ Will put into a 'MVar', possibly waking up some threads.
  | WillTryPutMVar MVarId
  -- ^ Will try to put into a 'MVar', possibly waking up some threads.
  | WillReadMVar MVarId
  -- ^ Will read from a 'MVar'.
  | WillTryReadMVar MVarId
  -- ^ Will try to read from a 'MVar'.
  | WillTakeMVar MVarId
  -- ^ Will take from a 'MVar', possibly waking up some threads.
  | WillTryTakeMVar MVarId
  -- ^ Will try to take from a 'MVar', possibly waking up some threads.
  | WillNewIORef
  -- ^ Will create a new 'IORef'.
  | WillReadIORef IORefId
  -- ^ Will read from a 'IORef'.
  | WillReadIORefCas IORefId
  -- ^ Will read from a 'IORef' for a future compare-and-swap.
  | WillModIORef IORefId
  -- ^ Will modify a 'IORef'.
  | WillModIORefCas IORefId
  -- ^ Will modify a 'IORef' using a compare-and-swap.
  | WillWriteIORef IORefId
  -- ^ Will write to a 'IORef' without synchronising.
  | WillCasIORef IORefId
  -- ^ Will attempt to to a 'IORef' using a compare-and-swap,
  -- synchronising it.
  | WillCommitIORef ThreadId IORefId
  -- ^ Will commit the last write by the given thread to the 'IORef'.
  | WillSTM
  -- ^ Will execute an STM transaction, possibly waking up some
  -- threads.
  | WillCatching
  -- ^ Will register a new exception handler
  | WillPopCatching
  -- ^ Will pop the innermost exception handler from the stack.
  | WillThrow
  -- ^ Will throw an exception.
  | WillThrowTo ThreadId
  -- ^ Will throw an exception to a thread.
  | WillSetMasking Bool MaskingState
  -- ^ Will set the masking state. If 'True', this is being used to
  -- set the masking state to the original state in the argument
  -- passed to a 'mask'ed function.
  | WillResetMasking Bool MaskingState
  -- ^ Will return to an earlier masking state.  If 'True', this is
  -- being used to return to the state of the masked block in the
  -- argument passed to a 'mask'ed function.
  | WillGetMaskingState
  -- ^ Will get the masking state.
  | WillLiftIO
  -- ^ Will lift an IO action. Note that this can only happen with
  -- 'ConcIO'.
  | WillReturn
  -- ^ Will execute a 'return' or 'pure' action.
  | WillStop
  -- ^ Will cease execution and terminate.
  | WillRegisterInvariant
  -- ^ Will register an invariant
  deriving (Eq, Generic, Show)

-- this also makes me sad
instance NFData Lookahead where
  rnf WillFork = ()
  rnf WillForkOS = ()
  rnf WillSupportsBoundThreads = ()
  rnf WillIsCurrentThreadBound = ()
  rnf WillMyThreadId = ()
  rnf WillGetNumCapabilities = ()
  rnf (WillSetNumCapabilities i) = rnf i
  rnf WillYield = ()
  rnf (WillThreadDelay i) = rnf i
  rnf WillNewMVar = ()
  rnf (WillPutMVar m) = rnf m
  rnf (WillTryPutMVar m) = rnf m
  rnf (WillReadMVar m) = rnf m
  rnf (WillTryReadMVar m) = rnf m
  rnf (WillTakeMVar m) = rnf m
  rnf (WillTryTakeMVar m) = rnf m
  rnf WillNewIORef = ()
  rnf (WillReadIORef c) = rnf c
  rnf (WillReadIORefCas c) = rnf c
  rnf (WillModIORef c) = rnf c
  rnf (WillModIORefCas c) = rnf c
  rnf (WillWriteIORef c) = rnf c
  rnf (WillCasIORef c) = rnf c
  rnf (WillCommitIORef t c) = rnf (t, c)
  rnf WillSTM = ()
  rnf WillCatching = ()
  rnf WillPopCatching = ()
  rnf WillThrow = ()
  rnf (WillThrowTo t) = rnf t
  rnf (WillSetMasking b m) = rnf (b, show m)
  rnf (WillResetMasking b m) = rnf (b, show m)
  rnf WillGetMaskingState = ()
  rnf WillLiftIO = ()
  rnf WillReturn = ()
  rnf WillStop = ()
  rnf WillRegisterInvariant = ()

-- | All the actions that an STM transaction can perform.
--
-- @since 0.8.0.0
data TAction =
    TNew TVarId
  -- ^ Create a new @TVar@
  | TRead  TVarId
  -- ^ Read from a @TVar@.
  | TWrite TVarId
  -- ^ Write to a @TVar@.
  | TRetry
  -- ^ Abort and discard effects.
  | TOrElse [TAction] (Maybe [TAction])
  -- ^ Execute a transaction.  If the transaction aborts by calling
  -- @retry@, execute the other transaction.
  | TThrow
  -- ^ Throw an exception, abort, and discard effects.
  | TCatch [TAction] (Maybe [TAction])
  -- ^ Execute a transaction.  If the transaction aborts by throwing
  -- an exception of the appropriate type, it is handled and execution
  -- continues; otherwise aborts, propagating the exception upwards.
  | TStop
  -- ^ Terminate successfully and commit effects.
  deriving (Eq, Show)

-- | @since 1.3.1.0
deriving instance Generic TAction

-- | @since 0.5.1.0
instance NFData TAction

-------------------------------------------------------------------------------
-- * Traces

-- | One of the outputs of the runner is a @Trace@, which is a log of
-- decisions made, all the alternative unblocked threads and what they
-- would do, and the action a thread took in its step.
--
-- @since 0.8.0.0
type Trace
  = [(Decision, [(ThreadId, Lookahead)], ThreadAction)]

-- | Scheduling decisions are based on the state of the running
-- program, and so we can capture some of that state in recording what
-- specific decision we made.
--
-- @since 0.5.0.0
data Decision =
    Start ThreadId
  -- ^ Start a new thread, because the last was blocked (or it's the
  -- start of computation).
  | Continue
  -- ^ Continue running the last thread for another step.
  | SwitchTo ThreadId
  -- ^ Pre-empt the running thread, and switch to another.
  deriving (Eq, Show)

-- | @since 1.3.1.0
deriving instance Generic Decision

-- | @since 0.5.1.0
instance NFData Decision

-------------------------------------------------------------------------------
-- * Conditions

-- | An indication of how a concurrent computation terminated, if it
-- didn't produce a value.
--
-- The @Eq@, @Ord@, and @NFData@ instances compare/evaluate the
-- exception with @show@ in the @UncaughtException@ and
-- @InvariantFailure@ cases.
--
-- @since 2.0.0.0
data Condition
  = Abort
  -- ^ The scheduler chose to abort execution. This will be produced
  -- if, for example, all possible decisions exceed the specified
  -- bounds (there have been too many pre-emptions, the computation
  -- has executed for too long, or there have been too many yields).
  | Deadlock
  -- ^ Every thread is blocked
  | UncaughtException SomeException
  -- ^ An uncaught exception bubbled to the top of the computation.
  | InvariantFailure SomeException
  -- ^ An uncaught exception caused an invariant to fail.
  deriving (Show, Generic)

instance Eq Condition where
  Abort                  == Abort                  = True
  Deadlock               == Deadlock               = True
  (UncaughtException e1) == (UncaughtException e2) = show e1 == show e2
  (InvariantFailure  e1) == (InvariantFailure  e2) = show e1 == show e2
  _ == _ = False

instance Ord Condition where
  compare = compare `on` transform where
    transform :: Condition -> (Int, Maybe String)
    transform Abort = (1, Nothing)
    transform Deadlock = (2, Nothing)
    transform (UncaughtException e) = (3, Just (show e))
    transform (InvariantFailure  e) = (4, Just (show e))

instance NFData Condition where
  rnf (UncaughtException e) = rnf (show e)
  rnf (InvariantFailure  e) = rnf (show e)
  rnf f = f `seq` ()

-- | Check if a condition is an @Abort@.
--
-- @since 0.9.0.0
isAbort :: Condition -> Bool
isAbort Abort = True
isAbort _ = False

-- | Check if a condition is a @Deadlock@.
--
-- @since 0.9.0.0
isDeadlock :: Condition -> Bool
isDeadlock Deadlock = True
isDeadlock _ = False

-- | Check if a condition is an @UncaughtException@
--
-- @since 0.9.0.0
isUncaughtException :: Condition -> Bool
isUncaughtException (UncaughtException _) = True
isUncaughtException _ = False

-- | Check if a condition is an @InvariantFailure@
--
-- @since 2.0.0.0
isInvariantFailure :: Condition -> Bool
isInvariantFailure (InvariantFailure _) = True
isInvariantFailure _ = False

-------------------------------------------------------------------------------
-- * Errors

-- | An indication that there is a bug in dejafu or you are using it
-- incorrectly.
--
-- @since 2.0.0.0
data Error
  = ScheduledBlockedThread
  -- ^ Raised as an exception if the scheduler attempts to schedule a
  -- blocked thread.
  | ScheduledMissingThread
  -- ^ Raised as an exception if the scheduler attempts to schedule a
  -- nonexistent thread.
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance Exception Error

-- | Check if an error is a scheduler error.
--
-- @since 1.12.0.0
isSchedulerError :: Error -> Bool
isSchedulerError _ = True

-------------------------------------------------------------------------------
-- * Schedule bounding

-- | @since 2.0.0.0
data Bounds = Bounds
  { boundPreemp :: Maybe PreemptionBound
  , boundFair   :: Maybe FairBound
  } deriving (Eq, Ord, Read, Show, Generic)

instance NFData Bounds

-- | Restrict the number of pre-emptive context switches allowed in an
-- execution.
--
-- A pre-emption bound of zero disables pre-emptions entirely.
--
-- @since 0.2.0.0
newtype PreemptionBound = PreemptionBound Int
  deriving (Enum, Eq, Ord, Num, Real, Integral, Read, Show)

-- | @since 1.3.1.0
deriving instance Generic PreemptionBound

-- | @since 0.5.1.0
instance NFData PreemptionBound

-- | Restrict the maximum difference between the number of yield or
-- delay operations different threads have performed.
--
-- A fair bound of zero disables yields and delays entirely.
--
-- @since 0.2.0.0
newtype FairBound = FairBound Int
  deriving (Enum, Eq, Ord, Num, Real, Integral, Read, Show)

-- | @since 1.3.1.0
deriving instance Generic FairBound

-- | @since 0.5.1.0
instance NFData FairBound

-- | Restrict the maximum length (in terms of primitive actions) of an
-- execution.
--
-- A length bound of zero immediately aborts the execution.
--
-- @since 0.2.0.0
newtype LengthBound = LengthBound Int
  deriving (Enum, Eq, Ord, Num, Real, Integral, Read, Show)

-- | @since 1.3.1.0
deriving instance Generic LengthBound

-- | @since 0.5.1.0
instance NFData LengthBound

-------------------------------------------------------------------------------
-- * Discarding results and traces

-- | An @Either Condition a -> Maybe Discard@ value can be used to
-- selectively discard results.
--
-- @since 0.7.1.0
data Discard
  = DiscardTrace
  -- ^ Discard the trace but keep the result.  The result will appear
  -- to have an empty trace.
  | DiscardResultAndTrace
  -- ^ Discard the result and the trace.  It will simply not be
  -- reported as a possible behaviour of the program.
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- | @since 1.3.1.0
deriving instance Generic Discard

instance NFData Discard

-- | A monoid for discard functions: combines two functions, keeping
-- the weaker.
--
-- @Nothing@ is weaker than @Just DiscardTrace@, which is weaker than
-- @Just DiscardResultAndTrace@.  This forms a commutative monoid
-- where the unit is @const (Just DiscardResultAndTrace)@.
--
-- @since 1.5.1.0
newtype Weaken a = Weaken
  { getWeakDiscarder :: Either Condition a -> Maybe Discard }

instance Semigroup (Weaken a) where
  (<>) = divide (\efa -> (efa, efa))

instance Monoid (Weaken a) where
  mempty = conquer
  mappend = (<>)

instance Contravariant Weaken where
  contramap f (Weaken d) = Weaken (d . fmap f)

instance Divisible Weaken where
  divide f (Weaken d1) (Weaken d2) = Weaken $ \case
    Right a ->
      let (b, c) = f a
      in min (d1 (Right b)) (d2 (Right c))
    Left e -> min (d1 (Left e)) (d2 (Left e))

  conquer = Weaken (const (Just DiscardResultAndTrace))

-- | Combine two discard functions, keeping the weaker.
--
-- @since 1.0.0.0
weakenDiscard ::
     (Either Condition a -> Maybe Discard)
  -> (Either Condition a -> Maybe Discard)
  -> Either Condition a -> Maybe Discard
weakenDiscard d1 d2 =
  getWeakDiscarder (Weaken d1 <> Weaken d2)

-- | A monoid for discard functions: combines two functions, keeping
-- the stronger.
--
-- @Just DiscardResultAndTrace@ is stronger than @Just DiscardTrace@,
-- which is stronger than @Nothing@.  This forms a commutative monoid
-- where the unit is @const Nothing@.
--
-- @since 1.5.1.0
newtype Strengthen a = Strengthen
  { getStrongDiscarder :: Either Condition a -> Maybe Discard }

instance Semigroup (Strengthen a) where
  (<>) = divide (\efa -> (efa, efa))

instance Monoid (Strengthen a) where
  mempty = conquer
  mappend = (<>)

instance Contravariant Strengthen where
  contramap f (Strengthen d) = Strengthen (d . fmap f)

instance Divisible Strengthen where
  divide f (Strengthen d1) (Strengthen d2) = Strengthen $ \case
    Right a ->
      let (b, c) = f a
      in max (d1 (Right b)) (d2 (Right c))
    Left e -> max (d1 (Left e)) (d2 (Left e))

  conquer = Strengthen (const Nothing)

-- | Combine two discard functions, keeping the stronger.
--
-- @since 1.0.0.0
strengthenDiscard ::
     (Either Condition a -> Maybe Discard)
  -> (Either Condition a -> Maybe Discard)
  -> Either Condition a -> Maybe Discard
strengthenDiscard d1 d2 =
  getStrongDiscarder (Strengthen d1 <> Strengthen d2)

-------------------------------------------------------------------------------
-- * Memory Models

-- | The memory model to use for non-synchronised 'IORef' operations.
--
-- @since 0.4.0.0
data MemType =
    SequentialConsistency
  -- ^ The most intuitive model: a program behaves as a simple
  -- interleaving of the actions in different threads. When a 'IORef'
  -- is written to, that write is immediately visible to all threads.
  | TotalStoreOrder
  -- ^ Each thread has a write buffer. A thread sees its writes
  -- immediately, but other threads will only see writes when they are
  -- committed, which may happen later. Writes are committed in the
  -- same order that they are created.
  | PartialStoreOrder
  -- ^ Each 'IORef' has a write buffer. A thread sees its writes
  -- immediately, but other threads will only see writes when they are
  -- committed, which may happen later. Writes to different 'IORef's
  -- are not necessarily committed in the same order that they are
  -- created.
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- | @since 1.3.1.0
deriving instance Generic MemType

-- | @since 0.5.1.0
instance NFData MemType

-------------------------------------------------------------------------------
-- * @MonadFail@

-- | An exception for errors in testing caused by use of 'fail'.
newtype MonadFailException = MonadFailException String
  deriving Show

instance Exception MonadFailException

-- | @since 1.3.1.0
deriving instance Generic MonadFailException

-- | @since 1.3.1.0
instance NFData MonadFailException

-------------------------------------------------------------------------------
-- ** Concurrency state

-- | A summary of the concurrency state of the program.
--
-- @since 2.0.0.0
data ConcurrencyState = ConcurrencyState
  { concIOState :: Map IORefId Int
  -- ^ Keep track of which @IORef@s have buffered writes.
  , concMVState :: Set MVarId
  -- ^ Keep track of which @MVar@s are full.
  , concMaskState :: Map ThreadId MaskingState
  -- ^ Keep track of thread masking states. If a thread isn't present,
  -- the masking state is assumed to be @Unmasked@. This nicely
  -- provides compatibility with dpor-0.1, where the thread IDs are
  -- not available.
  } deriving (Eq, Show)

instance NFData ConcurrencyState where
  rnf cstate = rnf
    ( concIOState cstate
    , concMVState cstate
    , [(t, show m) | (t, m) <- M.toList (concMaskState cstate)]
    )

-- | Check if a @IORef@ has a buffered write pending.
--
-- @since 2.0.0.0
isBuffered :: ConcurrencyState -> IORefId -> Bool
isBuffered cstate r = numBuffered cstate r /= 0

-- | Check how many buffered writes an @IORef@ has.
--
-- @since 2.0.0.0
numBuffered :: ConcurrencyState -> IORefId -> Int
numBuffered cstate r = M.findWithDefault 0 r (concIOState cstate)

-- | Check if an @MVar@ is full.
--
-- @since 2.0.0.0
isFull :: ConcurrencyState -> MVarId -> Bool
isFull cstate v = S.member v (concMVState cstate)

-- | Check if an exception can interrupt a thread (action).
--
-- @since 2.0.0.0
canInterrupt :: ConcurrencyState -> ThreadId -> ThreadAction -> Bool
canInterrupt cstate tid act
  -- If masked interruptible, blocked actions can be interrupted.
  | isMaskedInterruptible cstate tid = case act of
    BlockedPutMVar  _ -> True
    BlockedReadMVar _ -> True
    BlockedTakeMVar _ -> True
    BlockedSTM      _ -> True
    BlockedThrowTo  _ -> True
    _ -> False
  -- If masked uninterruptible, nothing can be.
  | isMaskedUninterruptible cstate tid = False
  -- If no mask, anything can be.
  | otherwise = True

-- | Check if an exception can interrupt a thread (lookahead).
--
-- @since 2.0.0.0
canInterruptL :: ConcurrencyState -> ThreadId -> Lookahead -> Bool
canInterruptL cstate tid lh
  -- If masked interruptible, actions which can block may be
  -- interrupted.
  | isMaskedInterruptible cstate tid = case lh of
    WillPutMVar  _ -> True
    WillReadMVar _ -> True
    WillTakeMVar _ -> True
    WillSTM        -> True
    WillThrowTo  _ -> True
    _ -> False
  -- If masked uninterruptible, nothing can be.
  | isMaskedUninterruptible cstate tid = False
  -- If no mask, anything can be.
  | otherwise = True

-- | Check if a thread is masked interruptible.
--
-- @since 2.0.0.0
isMaskedInterruptible :: ConcurrencyState -> ThreadId -> Bool
isMaskedInterruptible cstate tid =
  M.lookup tid (concMaskState cstate) == Just MaskedInterruptible

-- | Check if a thread is masked uninterruptible.
--
-- @since 2.0.0.0
isMaskedUninterruptible :: ConcurrencyState -> ThreadId -> Bool
isMaskedUninterruptible cstate tid =
  M.lookup tid (concMaskState cstate) == Just MaskedUninterruptible

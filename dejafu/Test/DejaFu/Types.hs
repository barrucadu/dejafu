{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Test.DejaFu.Types
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : GeneralizedNewtypeDeriving
--
-- Common types and functions used throughout DejaFu.
module Test.DejaFu.Types where

import           Control.DeepSeq   (NFData(..))
import           Control.Exception (Exception(..), MaskingState(..),
                                    SomeException)
import           Data.Function     (on)

-------------------------------------------------------------------------------
-- * Identifiers

-- | Every thread has a unique identitifer.
--
-- @since 1.0.0.0
newtype ThreadId = ThreadId Id
  deriving (Eq, Ord, NFData)

instance Show ThreadId where
  show (ThreadId id_) = show id_

-- | Every @CRef@ has a unique identifier.
--
-- @since 1.0.0.0
newtype CRefId = CRefId Id
  deriving (Eq, Ord, NFData)

instance Show CRefId where
  show (CRefId id_) = show id_

-- | Every @MVar@ has a unique identifier.
--
-- @since 1.0.0.0
newtype MVarId = MVarId Id
  deriving (Eq, Ord, NFData)

instance Show MVarId where
  show (MVarId id_) = show id_

-- | Every @TVar@ has a unique identifier.
--
-- @since 1.0.0.0
newtype TVarId = TVarId Id
  deriving (Eq, Ord, NFData)

instance Show TVarId where
  show (TVarId id_) = show id_

-- | An identifier for a thread, @MVar@, @CRef@, or @TVar@.
--
-- The number is the important bit.  The string is to make execution
-- traces easier to read, but is meaningless.
data Id = Id (Maybe String) {-# UNPACK #-} !Int

instance Eq Id where
  (Id _ i) == (Id _ j) = i == j

instance Ord Id where
  compare (Id _ i) (Id _ j) = compare i j

instance Show Id where
  show (Id (Just n) _) = n
  show (Id _ i) = show i

instance NFData Id where
  rnf (Id n i) = rnf (n, i)

-- | The ID of the initial thread.
--
-- @since 0.4.0.0
initialThread :: ThreadId
initialThread = ThreadId (Id (Just "main") 0)

-------------------------------------------------------------------------------
-- * Actions

-- | All the actions that a thread can perform.
--
-- @since 1.0.0.0
data ThreadAction =
    Fork ThreadId
  -- ^ Start a new thread.
  | ForkOS ThreadId
  -- ^ Start a new bound thread.
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
  | NewCRef CRefId
  -- ^ Create a new 'CRef'.
  | ReadCRef CRefId
  -- ^ Read from a 'CRef'.
  | ReadCRefCas CRefId
  -- ^ Read from a 'CRef' for a future compare-and-swap.
  | ModCRef CRefId
  -- ^ Modify a 'CRef'.
  | ModCRefCas CRefId
  -- ^ Modify a 'CRef' using a compare-and-swap.
  | WriteCRef CRefId
  -- ^ Write to a 'CRef' without synchronising.
  | CasCRef CRefId Bool
  -- ^ Attempt to to a 'CRef' using a compare-and-swap, synchronising
  -- it.
  | CommitCRef ThreadId CRefId
  -- ^ Commit the last write to the given 'CRef' by the given thread,
  -- so that all threads can see the updated value.
  | STM [TAction] [ThreadId]
  -- ^ An STM transaction was executed, possibly waking up some
  -- threads.
  | BlockedSTM [TAction]
  -- ^ Got blocked in an STM transaction.
  | Catching
  -- ^ Register a new exception handler
  | PopCatching
  -- ^ Pop the innermost exception handler from the stack.
  | Throw
  -- ^ Throw an exception.
  | ThrowTo ThreadId
  -- ^ Throw an exception to a thread.
  | BlockedThrowTo ThreadId
  -- ^ Get blocked on a 'throwTo'.
  | Killed
  -- ^ Killed by an uncaught exception.
  | SetMasking Bool MaskingState
  -- ^ Set the masking state. If 'True', this is being used to set the
  -- masking state to the original state in the argument passed to a
  -- 'mask'ed function.
  | ResetMasking Bool MaskingState
  -- ^ Return to an earlier masking state.  If 'True', this is being
  -- used to return to the state of the masked block in the argument
  -- passed to a 'mask'ed function.
  | LiftIO
  -- ^ Lift an IO action. Note that this can only happen with
  -- 'ConcIO'.
  | Return
  -- ^ A 'return' or 'pure' action was executed.
  | Stop
  -- ^ Cease execution and terminate.
  | Subconcurrency
  -- ^ Start executing an action with @subconcurrency@.
  | StopSubconcurrency
  -- ^ Stop executing an action with @subconcurrency@.
  deriving (Eq, Show)

instance NFData ThreadAction where
  rnf (Fork t) = rnf t
  rnf (ForkOS t) = rnf t
  rnf (ThreadDelay n) = rnf n
  rnf (GetNumCapabilities c) = rnf c
  rnf (SetNumCapabilities c) = rnf c
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
  rnf (NewCRef c) = rnf c
  rnf (ReadCRef c) = rnf c
  rnf (ReadCRefCas c) = rnf c
  rnf (ModCRef c) = rnf c
  rnf (ModCRefCas c) = rnf c
  rnf (WriteCRef c) = rnf c
  rnf (CasCRef c b) = rnf (c, b)
  rnf (CommitCRef t c) = rnf (t, c)
  rnf (STM tr ts) = rnf (tr, ts)
  rnf (BlockedSTM tr) = rnf tr
  rnf (ThrowTo t) = rnf t
  rnf (BlockedThrowTo t) = rnf t
  rnf (SetMasking b m) = b `seq` m `seq` ()
  rnf (ResetMasking b m) = b `seq` m `seq` ()
  rnf a = a `seq` ()

-- | A one-step look-ahead at what a thread will do next.
--
-- @since 1.0.0.0
data Lookahead =
    WillFork
  -- ^ Will start a new thread.
  | WillForkOS
  -- ^ Will start a new bound thread.
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
  | WillNewCRef
  -- ^ Will create a new 'CRef'.
  | WillReadCRef CRefId
  -- ^ Will read from a 'CRef'.
  | WillReadCRefCas CRefId
  -- ^ Will read from a 'CRef' for a future compare-and-swap.
  | WillModCRef CRefId
  -- ^ Will modify a 'CRef'.
  | WillModCRefCas CRefId
  -- ^ Will modify a 'CRef' using a compare-and-swap.
  | WillWriteCRef CRefId
  -- ^ Will write to a 'CRef' without synchronising.
  | WillCasCRef CRefId
  -- ^ Will attempt to to a 'CRef' using a compare-and-swap,
  -- synchronising it.
  | WillCommitCRef ThreadId CRefId
  -- ^ Will commit the last write by the given thread to the 'CRef'.
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
  | WillLiftIO
  -- ^ Will lift an IO action. Note that this can only happen with
  -- 'ConcIO'.
  | WillReturn
  -- ^ Will execute a 'return' or 'pure' action.
  | WillStop
  -- ^ Will cease execution and terminate.
  | WillSubconcurrency
  -- ^ Will execute an action with @subconcurrency@.
  | WillStopSubconcurrency
  -- ^ Will stop executing an extion with @subconcurrency@.
  deriving (Eq, Show)

instance NFData Lookahead where
  rnf (WillThreadDelay n) = rnf n
  rnf (WillSetNumCapabilities c) = rnf c
  rnf (WillPutMVar m) = rnf m
  rnf (WillTryPutMVar m) = rnf m
  rnf (WillReadMVar m) = rnf m
  rnf (WillTryReadMVar m) = rnf m
  rnf (WillTakeMVar m) = rnf m
  rnf (WillTryTakeMVar m) = rnf m
  rnf (WillReadCRef c) = rnf c
  rnf (WillReadCRefCas c) = rnf c
  rnf (WillModCRef c) = rnf c
  rnf (WillModCRefCas c) = rnf c
  rnf (WillWriteCRef c) = rnf c
  rnf (WillCasCRef c) = rnf c
  rnf (WillCommitCRef t c) = rnf (t, c)
  rnf (WillThrowTo t) = rnf t
  rnf (WillSetMasking b m) = b `seq` m `seq` ()
  rnf (WillResetMasking b m) = b `seq` m `seq` ()
  rnf l = l `seq` ()

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

-- | @since 0.5.1.0
instance NFData TAction where
  rnf (TRead t) = rnf t
  rnf (TWrite t) = rnf t
  rnf (TOrElse tr mtr) = rnf (tr, mtr)
  rnf (TCatch tr mtr) = rnf (tr, mtr)
  rnf ta = ta `seq` ()

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

-- | @since 0.5.1.0
instance NFData Decision where
  rnf (Start t) = rnf t
  rnf (SwitchTo t) = rnf t
  rnf d = d `seq` ()

-------------------------------------------------------------------------------
-- * Failures

-- | An indication of how a concurrent computation failed.
--
-- The @Eq@, @Ord@, and @NFData@ instances compare/evaluate the
-- exception with @show@ in the @UncaughtException@ case.
--
-- @since 0.9.0.0
data Failure
  = InternalError
  -- ^ Will be raised if the scheduler does something bad. This should
  -- never arise unless you write your own, faulty, scheduler! If it
  -- does, please file a bug report.
  | Abort
  -- ^ The scheduler chose to abort execution. This will be produced
  -- if, for example, all possible decisions exceed the specified
  -- bounds (there have been too many pre-emptions, the computation
  -- has executed for too long, or there have been too many yields).
  | Deadlock
  -- ^ Every thread is blocked, and the main thread is /not/ blocked
  -- in an STM transaction.
  | STMDeadlock
  -- ^ Every thread is blocked, and the main thread is blocked in an
  -- STM transaction.
  | UncaughtException SomeException
  -- ^ An uncaught exception bubbled to the top of the computation.
  | IllegalSubconcurrency
  -- ^ Calls to @subconcurrency@ were nested, or attempted when
  -- multiple threads existed.
  deriving Show

instance Eq Failure where
  InternalError          == InternalError          = True
  Abort                  == Abort                  = True
  Deadlock               == Deadlock               = True
  STMDeadlock            == STMDeadlock            = True
  (UncaughtException e1) == (UncaughtException e2) = show e1 == show e2
  IllegalSubconcurrency  == IllegalSubconcurrency  = True
  _ == _ = False

instance Ord Failure where
  compare = compare `on` transform where
    transform :: Failure -> (Int, Maybe String)
    transform InternalError = (0, Nothing)
    transform Abort = (1, Nothing)
    transform Deadlock = (2, Nothing)
    transform STMDeadlock = (3, Nothing)
    transform (UncaughtException e) = (4, Just (show e))
    transform IllegalSubconcurrency = (5, Nothing)

instance NFData Failure where
  rnf (UncaughtException e) = rnf (show e)
  rnf f = f `seq` ()

-- | Check if a failure is an @InternalError@.
--
-- @since 0.9.0.0
isInternalError :: Failure -> Bool
isInternalError InternalError = True
isInternalError _ = False

-- | Check if a failure is an @Abort@.
--
-- @since 0.9.0.0
isAbort :: Failure -> Bool
isAbort Abort = True
isAbort _ = False

-- | Check if a failure is a @Deadlock@ or an @STMDeadlock@.
--
-- @since 0.9.0.0
isDeadlock :: Failure -> Bool
isDeadlock Deadlock = True
isDeadlock STMDeadlock = True
isDeadlock _ = False

-- | Check if a failure is an @UncaughtException@
--
-- @since 0.9.0.0
isUncaughtException :: Failure -> Bool
isUncaughtException (UncaughtException _) = True
isUncaughtException _ = False

-- | Check if a failure is an @IllegalSubconcurrency@
--
-- @since 0.9.0.0
isIllegalSubconcurrency :: Failure -> Bool
isIllegalSubconcurrency IllegalSubconcurrency = True
isIllegalSubconcurrency _ = False

-------------------------------------------------------------------------------
-- * Discarding results and traces

-- | An @Either Failure a -> Maybe Discard@ value can be used to
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

instance NFData Discard where
  rnf d = d `seq` ()

-- | Combine two discard values, keeping the weaker.
--
-- @Nothing@ is weaker than @Just DiscardTrace@, which is weaker than
-- @Just DiscardResultAndTrace@.  This forms a commutative monoid
-- where the unit is @const (Just DiscardResultAndTrace)@.
--
-- @since 1.0.0.0
weakenDiscard ::
     (Either Failure a -> Maybe Discard)
  -> (Either Failure a -> Maybe Discard)
  -> Either Failure a -> Maybe Discard
weakenDiscard d1 d2 efa = case (d1 efa, d2 efa) of
  (Nothing, _) -> Nothing
  (_, Nothing) -> Nothing
  (Just DiscardTrace, _) -> Just DiscardTrace
  (_, Just DiscardTrace) -> Just DiscardTrace
  _ -> Just DiscardResultAndTrace

-- | Combine two discard functions, keeping the stronger.
--
-- @Just DiscardResultAndTrace@ is stronger than @Just DiscardTrace@,
-- which is stronger than @Nothing@.  This forms a commutative monoid
-- where the unit is @const Nothing@.
--
-- @since 1.0.0.0
strengthenDiscard ::
     (Either Failure a -> Maybe Discard)
  -> (Either Failure a -> Maybe Discard)
  -> Either Failure a -> Maybe Discard
strengthenDiscard d1 d2 efa = case (d1 efa, d2 efa) of
  (Just DiscardResultAndTrace, _) -> Just DiscardResultAndTrace
  (_, Just DiscardResultAndTrace) -> Just DiscardResultAndTrace
  (Just DiscardTrace, _) -> Just DiscardTrace
  (_, Just DiscardTrace) -> Just DiscardTrace
  _ -> Nothing

-------------------------------------------------------------------------------
-- * Memory Models

-- | The memory model to use for non-synchronised 'CRef' operations.
--
-- @since 0.4.0.0
data MemType =
    SequentialConsistency
  -- ^ The most intuitive model: a program behaves as a simple
  -- interleaving of the actions in different threads. When a 'CRef'
  -- is written to, that write is immediately visible to all threads.
  | TotalStoreOrder
  -- ^ Each thread has a write buffer. A thread sees its writes
  -- immediately, but other threads will only see writes when they are
  -- committed, which may happen later. Writes are committed in the
  -- same order that they are created.
  | PartialStoreOrder
  -- ^ Each 'CRef' has a write buffer. A thread sees its writes
  -- immediately, but other threads will only see writes when they are
  -- committed, which may happen later. Writes to different 'CRef's
  -- are not necessarily committed in the same order that they are
  -- created.
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- | @since 0.5.1.0
instance NFData MemType where
  rnf m = m `seq` ()

-------------------------------------------------------------------------------
-- * @MonadFail@

-- | An exception for errors in testing caused by use of 'fail'.
newtype MonadFailException = MonadFailException String
  deriving Show

instance Exception MonadFailException

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : Test.DejaFu.Types
-- Copyright   : (c) 2017--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : DeriveGeneric, GeneralizedNewtypeDeriving, LambdaCase, StandaloneDeriving
--
-- Common types and functions used throughout DejaFu.
module Test.DejaFu.Types where

import           Control.DeepSeq                      (NFData(..))
import           Control.Exception                    (Exception(..),
                                                       MaskingState(..),
                                                       SomeException)
import           Data.Function                        (on)
import           Data.Functor.Contravariant           (Contravariant(..))
import           Data.Functor.Contravariant.Divisible (Divisible(..))
import           Data.Semigroup                       (Semigroup(..))
import           GHC.Generics                         (Generic)

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
-- @since 1.11.0.0
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
  | BlockedSTM [TAction]
  -- ^ Got blocked in an STM transaction.
  | Catching
  -- ^ Register a new exception handler
  | PopCatching
  -- ^ Pop the innermost exception handler from the stack.
  | Throw Bool
  -- ^ Throw an exception.  If the 'Bool' is @True@, then this killed
  -- the thread.
  | ThrowTo ThreadId Bool
  -- ^ Throw an exception to a thread.  If the 'Bool' is @True@, then
  -- this killed the thread.
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
  | DontCheck Trace
  -- ^ Execute an action with @dontCheck@.
  deriving (Eq, Generic, Show)

-- this makes me sad
instance NFData ThreadAction where
  rnf (Fork t) = rnf t
  rnf (ForkOS t) = rnf t
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
  rnf (BlockedSTM as) = rnf as
  rnf Catching = ()
  rnf PopCatching = ()
  rnf (Throw b) = rnf b
  rnf (ThrowTo t b) = rnf (t, b)
  rnf (BlockedThrowTo t) = rnf t
  rnf (SetMasking b m) = rnf (b, show m)
  rnf (ResetMasking b m) = rnf (b, show m)
  rnf LiftIO = ()
  rnf Return = ()
  rnf Stop = ()
  rnf Subconcurrency = ()
  rnf StopSubconcurrency = ()
  rnf (DontCheck as) = rnf as

-- | A one-step look-ahead at what a thread will do next.
--
-- @since 1.11.0.0
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
  | WillDontCheck
  -- ^ Will execute an action with @dontCheck@.
  deriving (Eq, Generic, Show)

-- this also makes me sad
instance NFData Lookahead where
  rnf WillFork = ()
  rnf WillForkOS = ()
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
  rnf WillLiftIO = ()
  rnf WillReturn = ()
  rnf WillStop = ()
  rnf WillSubconcurrency = ()
  rnf WillStopSubconcurrency = ()
  rnf WillDontCheck = ()

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
-- * Failures

-- | An indication of how a concurrent computation failed.
--
-- The @Eq@, @Ord@, and @NFData@ instances compare/evaluate the
-- exception with @show@ in the @UncaughtException@ case.
--
-- @since 1.1.0.0
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
  | IllegalDontCheck
  -- ^ A call to @dontCheck@ was attempted after the first action of
  -- the initial thread.
  deriving Show

instance Eq Failure where
  InternalError          == InternalError          = True
  Abort                  == Abort                  = True
  Deadlock               == Deadlock               = True
  STMDeadlock            == STMDeadlock            = True
  (UncaughtException e1) == (UncaughtException e2) = show e1 == show e2
  IllegalSubconcurrency  == IllegalSubconcurrency  = True
  IllegalDontCheck       == IllegalDontCheck       = True
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
    transform IllegalDontCheck = (6, Nothing)

instance NFData Failure where
  rnf (UncaughtException e) = rnf (show e)
  rnf f = f `seq` ()

-- | @since 1.3.1.0
deriving instance Generic Failure

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

-- | Check if a failure is an @IllegalDontCheck@
--
-- @since 1.1.0.0
isIllegalDontCheck :: Failure -> Bool
isIllegalDontCheck IllegalDontCheck = True
isIllegalDontCheck _ = False

-------------------------------------------------------------------------------
-- * Schedule bounding

-- | @since 0.2.0.0
data Bounds = Bounds
  { boundPreemp :: Maybe PreemptionBound
  , boundFair   :: Maybe FairBound
  , boundLength :: Maybe LengthBound
  } deriving (Eq, Ord, Read, Show)

-- | @since 1.3.1.0
deriving instance Generic Bounds

-- | @since 0.5.1.0
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
  { getWeakDiscarder :: Either Failure a -> Maybe Discard }

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
     (Either Failure a -> Maybe Discard)
  -> (Either Failure a -> Maybe Discard)
  -> Either Failure a -> Maybe Discard
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
  { getStrongDiscarder :: Either Failure a -> Maybe Discard }

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
     (Either Failure a -> Maybe Discard)
  -> (Either Failure a -> Maybe Discard)
  -> Either Failure a -> Maybe Discard
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

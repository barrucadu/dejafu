{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

-- | Common types and utility functions for deterministic execution of
-- 'MonadConc' implementations.
module Test.DejaFu.Deterministic.Internal.Common where

import Control.DeepSeq (NFData(..))
import Control.Exception (Exception, MaskingState(..), SomeException(..))
import Control.Monad.Cont (Cont)
import Control.State
import Data.List.Extra
import Test.DejaFu.STM (CTVarId)

--------------------------------------------------------------------------------
-- * The @Conc@ Monad

-- | The underlying monad is based on continuations over Actions.
type M n r s a = Cont (Action n r s) a

-- | CVars are represented as a unique numeric identifier, and a
-- reference containing a Maybe value.
type V r a = (CVarId, r (Maybe a))

-- | CRefs are represented as a unique numeric identifier, and a
-- reference containing a value.
type R r a = (CRefId, r a)

-- | Dict of methods for implementations to override.
type Fixed n r s = Wrapper n r (Cont (Action n r s))

--------------------------------------------------------------------------------
-- * Primitive Actions

-- | Scheduling is done in terms of a trace of 'Action's. Blocking can
-- only occur as a result of an action, and they cover (most of) the
-- primitives of the concurrency. 'spawn' is absent as it is
-- implemented in terms of 'newEmptyCVar', 'fork', and 'putCVar'.
data Action n r s =
    AFork ((forall b. M n r s b -> M n r s b) -> Action n r s) (ThreadId -> Action n r s)
  | AMyTId (ThreadId -> Action n r s)
  | forall a. APut     (V r a) a (Action n r s)
  | forall a. ATryPut  (V r a) a (Bool -> Action n r s)
  | forall a. AGet     (V r a) (a -> Action n r s)
  | forall a. ATake    (V r a) (a -> Action n r s)
  | forall a. ATryTake (V r a) (Maybe a -> Action n r s)
  | forall a. AReadRef (R r a) (a -> Action n r s)
  | forall a b. AModRef  (R r a) (a -> (a, b)) (b -> Action n r s)
  | forall a. ANoTest  (M n r s a) (a -> Action n r s)
  | forall a. AAtom    (s n r a) (a -> Action n r s)
  | ANew  (CVarId -> n (Action n r s))
  | ANewRef (CRefId -> n (Action n r s))
  | ALift (n (Action n r s))
  | AThrow SomeException
  | AThrowTo ThreadId SomeException (Action n r s)
  | forall a e. Exception e => ACatching (e -> M n r s a) (M n r s a) (a -> Action n r s)
  | APopCatching (Action n r s)
  | forall a. AMasking MaskingState ((forall b. M n r s b -> M n r s b) -> M n r s a) (a -> Action n r s)
  | AResetMask Bool Bool MaskingState (Action n r s)
  | AKnowsAbout (Either CVarId CTVarId) (Action n r s)
  | AForgets (Either CVarId CTVarId) (Action n r s)
  | AAllKnown (Action n r s)
  | AStop

--------------------------------------------------------------------------------
-- * Identifiers

-- | Every live thread has a unique identitifer.
type ThreadId = Int

-- | Every 'CVar' also has a unique identifier.
type CVarId = Int

-- | Every 'CRef' also has a unique identifier.
type CRefId = Int

-- | The number of ID parameters was getting a bit unwieldy, so this
-- hides them all away.
data IdSource = Id { _nextCRId :: CRefId, _nextCVId :: CVarId, _nextCTVId :: CTVarId, _nextTId :: ThreadId }

-- | Get the next free 'CRefId'.
nextCRId :: IdSource -> (IdSource, CRefId)
nextCRId idsource = let newid = _nextCRId idsource + 1 in (idsource { _nextCRId = newid }, newid)

-- | Get the next free 'CVarId'.
nextCVId :: IdSource -> (IdSource, CVarId)
nextCVId idsource = let newid = _nextCVId idsource + 1 in (idsource { _nextCVId = newid }, newid)

-- | Get the next free 'CTVarId'.
nextCTVId :: IdSource -> (IdSource, CTVarId)
nextCTVId idsource = let newid = _nextCTVId idsource + 1 in (idsource { _nextCTVId = newid }, newid)

-- | Get the next free 'ThreadId'.
nextTId :: IdSource -> (IdSource, ThreadId)
nextTId idsource = let newid = _nextTId idsource + 1 in (idsource { _nextTId = newid }, newid)

-- | The initial ID source.
initialIdSource :: IdSource
initialIdSource = Id 0 0 0 0

--------------------------------------------------------------------------------
-- * Scheduling & Traces

-- | A @Scheduler@ maintains some internal state, @s@, takes the
-- 'ThreadId' of the last thread scheduled, and the list of runnable
-- threads. It produces a 'ThreadId' to schedule, and a new state.
--
-- Note: In order to prevent computation from hanging, the runtime
-- will assume that a deadlock situation has arisen if the scheduler
-- attempts to (a) schedule a blocked thread, or (b) schedule a
-- nonexistent thread. In either of those cases, the computation will
-- be halted.
type Scheduler s = s -> ThreadId -> NonEmpty ThreadId -> (ThreadId, s)

-- | One of the outputs of the runner is a @Trace@, which is a log of
-- decisions made, alternative decisions, and the action a thread took
-- in its step.
type Trace = [(Decision, [Decision], ThreadAction)]

-- | Pretty-print a trace.
showTrace :: Trace -> String
showTrace = trace "" 0 where
  trace prefix num ((Start tid,_,_):ds)    = thread prefix num ++ trace ("S" ++ show tid) 1 ds
  trace prefix num ((SwitchTo tid,_,_):ds) = thread prefix num ++ trace ("P" ++ show tid) 1 ds
  trace prefix num ((Continue,_,_):ds)     = trace prefix (num + 1) ds
  trace prefix num []                      = thread prefix num

  thread prefix num = prefix ++ replicate num '-'

-- | Scheduling decisions are based on the state of the running
-- program, and so we can capture some of that state in recording what
-- specific decision we made.
data Decision =
    Start ThreadId
  -- ^ Start a new thread, because the last was blocked (or it's the
  -- start of computation).
  | Continue
  -- ^ Continue running the last thread for another step.
  | SwitchTo ThreadId
  -- ^ Pre-empt the running thread, and switch to another.
  deriving (Eq, Show)

instance NFData Decision where
  rnf (Start    tid) = rnf tid
  rnf (SwitchTo tid) = rnf tid
  rnf Continue = ()

-- | All the actions that a thread can perform.
data ThreadAction =
    Fork ThreadId
  -- ^ Start a new thread.
  | MyThreadId
  -- ^ Get the 'ThreadId' of the current thread.
  | New CVarId
  -- ^ Create a new 'CVar'.
  | Put CVarId [ThreadId]
  -- ^ Put into a 'CVar', possibly waking up some threads.
  | BlockedPut CVarId
  -- ^ Get blocked on a put.
  | TryPut CVarId Bool [ThreadId]
  -- ^ Try to put into a 'CVar', possibly waking up some threads.
  | Read CVarId
  -- ^ Read from a 'CVar'.
  | BlockedRead CVarId
  -- ^ Get blocked on a read.
  | Take CVarId [ThreadId]
  -- ^ Take from a 'CVar', possibly waking up some threads.
  | BlockedTake CVarId
  -- ^ Get blocked on a take.
  | TryTake CVarId Bool [ThreadId]
  -- ^ Try to take from a 'CVar', possibly waking up some threads.
  | NewRef CRefId
  -- ^ Create a new 'CRef'.
  | ReadRef CRefId
  -- ^ Read from a 'CRef'.
  | ModRef CRefId
  -- ^ Modify a 'CRef'.
  | STM [ThreadId]
  -- ^ An STM transaction was executed, possibly waking up some
  -- threads.
  | BlockedSTM
  -- ^ Got blocked in an STM transaction.
  | NoTest
  -- ^ A computation annotated with '_concNoTest' was executed in a
  -- single step.
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
  | Lift
  -- ^ Lift an action from the underlying monad. Note that the
  -- penultimate action in a trace will always be a @Lift@, this is an
  -- artefact of how the runner works.
  | Stop
  -- ^ Cease execution and terminate.
  deriving (Eq, Show)

instance NFData ThreadAction where
  rnf (TryTake c b tids) = rnf (c, b, tids)
  rnf (TryPut  c b tids) = rnf (c, b, tids)
  rnf (SetMasking   b m) = m `seq` b `seq` ()
  rnf (ResetMasking b m) = m `seq` b `seq` ()
  rnf (BlockedRead c) = rnf c
  rnf (BlockedTake c) = rnf c
  rnf (BlockedPut  c) = rnf c
  rnf (ThrowTo tid) = rnf tid
  rnf (Take c tids) = rnf (c, tids)
  rnf (Put  c tids) = rnf (c, tids)
  rnf (STM  tids) = rnf tids
  rnf (Fork tid)  = rnf tid
  rnf (New  c) = rnf c
  rnf (Read c) = rnf c
  rnf ta = ta `seq` ()

--------------------------------------------------------------------------------
-- * Failures

-- | An indication of how a concurrent computation failed.
data Failure =
    InternalError
  -- ^ Will be raised if the scheduler does something bad. This should
  -- never arise unless you write your own, faulty, scheduler! If it
  -- does, please file a bug report.
  | Deadlock
  -- ^ The computation became blocked indefinitely on @CVar@s.
  | STMDeadlock
  -- ^ The computation became blocked indefinitely on @CTVar@s.
  | UncaughtException
  -- ^ An uncaught exception bubbled to the top of the computation.
  | FailureInNoTest
  -- ^ A computation annotated with '_concNoTest' produced a failure,
  -- rather than a result.
  deriving (Eq, Show)

instance NFData Failure where
  rnf f = f `seq` () -- WHNF == NF

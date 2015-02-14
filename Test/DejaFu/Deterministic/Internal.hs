{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

-- | Concurrent monads with a fixed scheduler: internal types and
-- functions.
module Test.DejaFu.Deterministic.Internal where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.Exception (Exception, MaskingState(..), SomeException(..), fromException)
import Control.Monad (when)
import Control.Monad.Cont (Cont, runCont)
import Control.State
import Data.List (intersect)
import Data.List.Extra
import Data.Map (Map)
import Data.Maybe (fromJust, isJust, isNothing)
import Test.DejaFu.STM (CTVarId, Result(..))

import qualified Data.Map as M

-- * The @Conc@ Monad

-- | The underlying monad is based on continuations over Actions.
type M n r s a = Cont (Action n r s) a

-- | CVars are represented as a unique numeric identifier, and a
-- reference containing a Maybe value.
type R r a = (CVarId, r (Maybe a))

-- | Dict of methods for implementations to override.
type Fixed n r s = Wrapper n r (Cont (Action n r s))

-- * Running @Conc@ Computations

-- | Scheduling is done in terms of a trace of 'Action's. Blocking can
-- only occur as a result of an action, and they cover (most of) the
-- primitives of the concurrency. 'spawn' is absent as it is
-- implemented in terms of 'newEmptyCVar', 'fork', and 'putCVar'.
data Action n r s =
    AFork (Action n r s) (ThreadId -> Action n r s)
  | AMyTId (ThreadId -> Action n r s)
  | forall a. APut     (R r a) a (Action n r s)
  | forall a. ATryPut  (R r a) a (Bool -> Action n r s)
  | forall a. AGet     (R r a) (a -> Action n r s)
  | forall a. ATake    (R r a) (a -> Action n r s)
  | forall a. ATryTake (R r a) (Maybe a -> Action n r s)
  | forall a. ANoTest  (M n r s a) (a -> Action n r s)
  | forall a. AAtom    (s n r a) (a -> Action n r s)
  | ANew  (CVarId -> n (Action n r s))
  | ALift (n (Action n r s))
  | AThrow SomeException
  | AThrowTo ThreadId SomeException (Action n r s)
  | forall a e. Exception e => ACatching (e -> M n r s a) (M n r s a) (a -> Action n r s)
  | APopCatching (Action n r s)
  | forall a. AMasking MaskingState ((forall b. M n r s b -> M n r s b) -> M n r s a) (a -> Action n r s)
  | AResetMask MaskingState (Action n r s)
  | AStop

-- | Every live thread has a unique identitifer.
type ThreadId = Int

-- | Every 'CVar' also has a unique identifier.
type CVarId = Int

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
  | Killed
  -- ^ Killed by an uncaught exception.
  | SetMasking MaskingState
  -- ^ Set the masking state.
  | ResetMasking MaskingState
  -- ^ Return to an earlier masking state.
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
  rnf (SetMasking   m) = m `seq` ()
  rnf (ResetMasking m) = m `seq` ()
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

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning a 'Just' if it terminates, and 'Nothing' if a
-- deadlock is detected. Also returned is the final state of the
-- scheduler, and an execution trace.
runFixed :: (Functor n, Monad n) => Fixed n r s -> (forall x. s n r x -> CTVarId -> n (Result x, CTVarId))
         -> Scheduler g -> g -> M n r s a -> n (Either Failure a, g, Trace)
runFixed fixed runstm sched s ma = (\(e,g,_,t) -> (e,g,t)) <$> runFixed' fixed runstm sched s initialIdSource ma

-- | Same as 'runFixed', be parametrised by an 'IdSource'.
runFixed' :: (Functor n, Monad n) => Fixed n r s -> (forall x. s n r x -> CTVarId -> n (Result x, CTVarId))
          -> Scheduler g -> g -> IdSource -> M n r s a -> n (Either Failure a, g, IdSource, Trace)
runFixed' fixed runstm sched s idSource ma = do
  ref <- newRef (wref fixed) Nothing

  let c       = ma >>= liftN fixed . writeRef (wref fixed) ref . Just . Right
  let threads = launch 0 (runCont c $ const AStop) M.empty

  (s', idSource', trace) <- runThreads fixed runstm sched s threads idSource ref
  out <- readRef (wref fixed) ref

  return (fromJust out, s', idSource', reverse trace)

-- * Running threads

-- | A @BlockedOn@ is used to determine what sort of variable a thread
-- is blocked on.
data BlockedOn = OnCVarFull CVarId | OnCVarEmpty CVarId | OnCTVar [CTVarId] deriving Eq

-- | Determine if a thread is blocked in a certainw ay.
(~=) :: Thread n r s -> BlockedOn -> Bool
thread ~= theblock = case (_blocking thread, theblock) of
  (Just (OnCVarFull  _), OnCVarFull  _) -> True
  (Just (OnCVarEmpty _), OnCVarEmpty _) -> True
  (Just (OnCTVar     _), OnCTVar     _) -> True
  _ -> False

-- | Threads are stored in a map index by 'ThreadId'.
type Threads n r s = Map ThreadId (Thread n r s)

-- | All the state of a thread.
data Thread n r s = Thread
  { _continuation :: Action n r s
  -- ^ The next action to execute.
  , _blocking     :: Maybe BlockedOn
  -- ^ The state of any blocks.
  , _handlers     :: [Handler n r s]
  -- ^ Stack of exception handlers
  , _masking      :: MaskingState
  -- ^ The exception masking state.
  }

-- | An exception handler.
data Handler n r s = forall e. Exception e => Handler (e -> Action n r s)

-- | Propagate an exception upwards, finding the closest handler
-- which can deal with it.
propagate :: SomeException -> [Handler n r s] -> Maybe (Action n r s, [Handler n r s])
propagate _ [] = Nothing
propagate e (Handler h:hs) = maybe (propagate e hs) (\act -> Just (act, hs)) $ h <$> e' where
  e' = fromException e

-- | The number of ID parameters was getting a bit unwieldy, so this
-- hides them all away.
data IdSource = Id { _nextCVId :: CVarId, _nextCTVId :: CTVarId, _nextTId :: ThreadId }

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
initialIdSource = Id 0 0 0

-- | Run a collection of threads, until there are no threads left.
--
-- A thread is represented as a tuple of (next action, is blocked).
--
-- Note: this returns the trace in reverse order, because it's more
-- efficient to prepend to a list than append. As this function isn't
-- exposed to users of the library, this is just an internal gotcha to
-- watch out for.
runThreads :: (Functor n, Monad n) => Fixed n r s -> (forall x. s n r x -> CTVarId -> n (Result x, CTVarId))
           -> Scheduler g -> g -> Threads n r s -> IdSource -> r (Maybe (Either Failure a)) -> n (g, IdSource, Trace)
runThreads fixed runstm sched origg origthreads idsrc ref = go idsrc [] (-1) origg origthreads where
  go idSource sofar prior g threads
    | isTerminated  = return (g, idSource, sofar)
    | isDeadlocked  = writeRef (wref fixed) ref (Just $ Left Deadlock) >> return (g, idSource, sofar)
    | isSTMLocked   = writeRef (wref fixed) ref (Just $ Left STMDeadlock) >> return (g, idSource, sofar)
    | isNonexistant = writeRef (wref fixed) ref (Just $ Left InternalError) >> return (g, idSource, sofar)
    | isBlocked     = writeRef (wref fixed) ref (Just $ Left InternalError) >> return (g, idSource, sofar)
    | otherwise = do
      stepped <- stepThread fixed runconc runstm (_continuation $ fromJust thread) idSource chosen threads
      case stepped of
        Right (threads', idSource', act) ->
          let sofar' = (decision, alternatives, act) : sofar
          in  go idSource' sofar' chosen g' threads'

        Left UncaughtException
          | chosen == 0 -> writeRef (wref fixed) ref (Just $ Left UncaughtException) >> return (g, idSource, sofar)
          | otherwise ->
          let sofar' = (decision, alternatives, Killed) : sofar
              threads' = kill chosen threads
          in go idSource sofar' chosen g' threads'

        Left failure -> writeRef (wref fixed) ref (Just $ Left failure) >> return (g, idSource, sofar)

    where
      (chosen, g')  = if prior == -1 then (0, g) else sched g prior $ head runnable' :| tail runnable'
      runnable'     = M.keys runnable
      runnable      = M.filter (isNothing . _blocking) threads
      thread        = M.lookup chosen threads
      isBlocked     = isJust . _blocking $ fromJust thread
      isNonexistant = isNothing thread
      isTerminated  = 0 `notElem` M.keys threads
      isDeadlocked  = M.null runnable && (((~= OnCVarFull  undefined) <$> M.lookup 0 threads) == Just True ||
                                         ((~= OnCVarEmpty undefined) <$> M.lookup 0 threads) == Just True)
      isSTMLocked   = M.null runnable && ((~= OnCTVar []) <$> M.lookup 0 threads) == Just True

      runconc ma i = do { (a,_,i',_) <- runFixed' fixed runstm sched g i ma; return (a,i') }

      decision
        | chosen == prior         = Continue
        | prior `elem` runnable' = SwitchTo chosen
        | otherwise              = Start chosen

      alternatives
        | chosen == prior         = map SwitchTo $ filter (/=prior) runnable'
        | prior `elem` runnable' = Continue : map SwitchTo (filter (\t -> t /= prior && t /= chosen) runnable')
        | otherwise              = map Start $ filter (/=chosen) runnable'

-- | Run a single thread one step, by dispatching on the type of
-- 'Action'.
stepThread :: (Functor n, Monad n) => Fixed n r s
           -> (forall x. M n r s x -> IdSource -> n (Either Failure x, IdSource))
           -- ^ Run a 'MonadConc' computation atomically.
           -> (forall x. s n r x -> CTVarId -> n (Result x, CTVarId))
           -- ^ Run a 'MonadSTM' transaction atomically.
           -> Action n r s
           -- ^ Action to step
           -> IdSource
           -- ^ Source of fresh IDs
           -> ThreadId
           -- ^ ID of the current thread
           -> Threads n r s
           -- ^ Current state of threads
           -> n (Either Failure (Threads n r s, IdSource, ThreadAction))
stepThread fixed runconc runstm action idSource tid threads = case action of
  AFork    a b     -> stepFork        a b
  AMyTId   c       -> stepMyTId       c
  APut     ref a c -> stepPut         ref a c
  ATryPut  ref a c -> stepTryPut      ref a c
  AGet     ref c   -> stepGet         ref c
  ATake    ref c   -> stepTake        ref c
  ATryTake ref c   -> stepTryTake     ref c
  AAtom    stm c   -> stepAtom        stm c
  ANew     na      -> stepNew         na
  ALift    na      -> stepLift        na
  AThrow   e       -> stepThrow       e
  AThrowTo t e c   -> stepThrowTo     t e c
  ACatching h ma c -> stepCatching    h ma c
  APopCatching a   -> stepPopCatching a
  AMasking m ma c  -> stepMasking     m ma c
  AResetMask m c   -> stepResetMask   m c
  ANoTest  ma a    -> stepNoTest      ma a
  AStop            -> stepStop

  where
    -- | Start a new thread, assigning it the next 'ThreadId'
    stepFork a b = return $ Right (goto (b newtid) tid threads', idSource', Fork newtid) where
      threads' = launch newtid a threads
      (idSource', newtid) = nextTId idSource

    -- | Get the 'ThreadId' of the current thread
    stepMyTId c = return $ Right (goto (c tid) tid threads, idSource, MyThreadId)

    -- | Put a value into a @CVar@, blocking the thread until it's
    -- empty.
    stepPut cvar@(cvid, _) a c = do
      (success, threads', woken) <- putIntoCVar True cvar a (const c) fixed tid threads
      return $ Right (threads', idSource, if success then Put cvid woken else BlockedPut cvid)

    -- | Try to put a value into a @CVar@, without blocking.
    stepTryPut cvar@(cvid, _) a c = do
      (success, threads', woken) <- putIntoCVar False cvar a c fixed tid threads
      return $ Right (threads', idSource, TryPut cvid success woken)

    -- | Get the value from a @CVar@, without emptying, blocking the
    -- thread until it's full.
    stepGet cvar@(cvid, _) c = do
      (success, threads', _) <- readFromCVar False True cvar (c . fromJust) fixed tid threads
      return $ Right (threads', idSource, if success then Read cvid else BlockedRead cvid)

    -- | Take the value from a @CVar@, blocking the thread until it's
    -- full.
    stepTake cvar@(cvid, _) c = do
      (success, threads', woken) <- readFromCVar True True cvar (c . fromJust) fixed tid threads
      return $ Right (threads', idSource, if success then Take cvid woken else BlockedTake cvid)

    -- | Try to take the value from a @CVar@, without blocking.
    stepTryTake cvar@(cvid, _) c = do
      (success, threads', woken) <- readFromCVar True False cvar c fixed tid threads
      return $ Right (threads', idSource, TryTake cvid success woken)

    -- | Run a STM transaction atomically.
    stepAtom stm c = do
      (res, newctvid) <- runstm stm (_nextCTVId idSource)
      case res of
        Success touched val ->
          let (threads', woken) = wake (OnCTVar touched) threads
          in return $ Right (goto (c val) tid threads', idSource { _nextCTVId = newctvid }, STM woken)
        Retry touched ->
          let threads' = block (OnCTVar touched) tid threads
          in return $ Right (threads', idSource { _nextCTVId = newctvid }, BlockedSTM)
        Exception e -> stepThrow e

    -- | Run a subcomputation in an exception-catching context.
    stepCatching h ma c = return $ Right (threads', idSource, Catching) where
      a     = runCont ma      (APopCatching . c)
      e exc = runCont (h exc) (APopCatching . c)

      threads' = M.alter (\(Just thread) -> Just $ thread { _continuation = a, _handlers = Handler e : _handlers thread }) tid threads

    -- | Pop the top exception handler from the thread's stack.
    stepPopCatching a = return $ Right (threads', idSource, PopCatching) where
      threads' = M.alter (\(Just thread) -> Just $ thread { _continuation = a, _handlers = tail $_handlers thread }) tid threads

    -- | Throw an exception, and propagate it to the appropriate
    -- handler.
    stepThrow e = return $
      case propagate e . _handlers . fromJust $ M.lookup tid threads of
        Just (act, hs) ->
          let threads' = M.alter (\(Just thread) -> Just $ thread { _continuation = act, _handlers = hs }) tid threads
          in  Right (threads', idSource, Throw)
        Nothing -> Left UncaughtException

    -- | Throw an exception to the target thread, and propagate it to
    -- the appropriate handler.
    stepThrowTo t e c = return $
      let threads' = goto c tid threads
      in case propagate e . _handlers <$> M.lookup t threads of
           Just (Just (act, hs)) -> Right (M.alter (\(Just thread) -> Just $ thread { _continuation = act, _blocking = Nothing, _handlers = hs }) t threads', idSource, ThrowTo t)
           Just Nothing
             | t == 0     -> Left UncaughtException
             | otherwise -> Right (kill t threads', idSource, ThrowTo t)
           Nothing -> Right (threads', idSource, ThrowTo t)

    -- | Execute a subcomputation with a new masking state, and give
    -- it a function to run a computation with the current masking
    -- state.
    stepMasking m ma c = error "'AMasking' not yet implemented in 'stepThread'"

    -- | Reset the masking thread of the state.
    stepResetMask m c = error "'AResetMask' not yet implemented  in 'stepThread'"

    -- | Create a new @CVar@, using the next 'CVarId'.
    stepNew na = do
      let (idSource', newcvid) = nextCVId idSource
      a <- na newcvid
      return $ Right (goto a tid threads, idSource', New newcvid)

    -- | Lift an action from the underlying monad into the @Conc@
    -- computation.
    stepLift na = do
      a <- na
      return $ Right (goto a tid threads, idSource, Lift)

    -- | Run a computation atomically. If this fails, the entire thing fails.
    stepNoTest ma c = do
      (a, idSource') <- runconc ma idSource
      return $
        case a of
          Right a' -> Right (goto (c a') tid threads, idSource', NoTest)
          _ -> Left FailureInNoTest

    -- | Kill the current thread.
    stepStop = return $ Right (kill tid threads, idSource, Stop)

-- * Manipulating @CVar@s

-- | Put a value into a @CVar@, in either a blocking or nonblocking
-- way.
putIntoCVar :: Monad n
            => Bool -> R r a -> a -> (Bool -> Action n r s)
            -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
putIntoCVar blocking (cvid, ref) a c fixed threadid threads = do
  val <- readRef (wref fixed) ref

  case val of
    Just _
      | blocking ->
        let threads' = block (OnCVarEmpty cvid) threadid threads
        in return (False, threads', [])

      | otherwise ->
        return (False, goto (c False) threadid threads, [])

    Nothing -> do
      writeRef (wref fixed) ref $ Just a
      let (threads', woken) = wake (OnCVarFull cvid) threads
      return (True, goto (c True) threadid threads', woken)

-- | Take a value from a @CVar@, in either a blocking or nonblocking
-- way.
readFromCVar :: Monad n
             => Bool -> Bool -> R r a -> (Maybe a -> Action n r s)
             -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
readFromCVar emptying blocking (cvid, ref) c fixed threadid threads = do
  val <- readRef (wref fixed) ref

  case val of
    Just _ -> do
      when emptying $ writeRef (wref fixed) ref Nothing
      let (threads', woken) = wake (OnCVarEmpty cvid) threads
      return (True, goto (c val) threadid threads', woken)

    Nothing
      | blocking ->
        let threads' = block (OnCVarFull cvid) threadid threads
        in return (False, threads', [])

      | otherwise ->
        return (False, goto (c Nothing) threadid threads, [])

-- * Manipulating threads

-- | Replace the @Action@ of a thread.
goto :: Action n r s -> ThreadId -> Threads n r s -> Threads n r s
goto a = M.alter $ \(Just thread) -> Just (thread { _continuation = a })

-- | Start a thread with the given ID. This must not already be in use!
launch :: ThreadId -> Action n r s -> Threads n r s -> Threads n r s
launch tid a = M.insert tid thread where
  thread = Thread { _continuation = a, _blocking = Nothing, _handlers = [], _masking = Unmasked }

-- | Kill a thread.
kill :: ThreadId -> Threads n r s -> Threads n r s
kill = M.delete

-- | Block a thread.
block :: BlockedOn -> ThreadId -> Threads n r s -> Threads n r s
block blockedOn = M.alter doBlock where
  doBlock (Just thread) = Just $ thread { _blocking = Just blockedOn }

-- | Unblock all threads waiting on the appropriate block. For 'CTVar'
-- blocks, this will wake all threads waiting on at least one of the
-- given 'CTVar's.
wake :: BlockedOn -> Threads n r s -> (Threads n r s, [ThreadId])
wake blockedOn threads = (M.map unblock threads, M.keys $ M.filter isBlocked threads) where
  unblock thread
    | isBlocked thread = thread { _blocking = Nothing }
    | otherwise = thread

  isBlocked thread = case (_blocking thread, blockedOn) of
    (Just (OnCTVar ctvids), OnCTVar blockedOn') -> ctvids `intersect` blockedOn' /= []
    (theblock, _) -> theblock == Just blockedOn

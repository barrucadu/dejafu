{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

-- | Concurrent monads with a fixed scheduler: internal types and
-- functions.
module Test.DejaFu.Deterministic.Internal where

import Control.DeepSeq (NFData(..))
import Control.Monad (liftM, mapAndUnzipM)
import Control.Monad.Cont (Cont, runCont)
import Data.List.Extra
import Data.Map (Map)
import Data.Maybe (catMaybes, fromJust, isNothing)

import qualified Data.Map as M

-- * The @Conc@ Monad

-- | The underlying monad is based on continuations over Actions.
type M n r a = Cont (Action n r) a

-- | CVars are represented as a reference containing a Maybe value, a
-- list of things blocked on it, and a unique numeric identifier.
type R r a = r (CVarId, Maybe a, [Block])

-- | Dict of methods for concrete implementations to override.
data Fixed c n r t = F
  { newRef   :: forall a. a -> n (r a)
  -- ^ Create a new reference
  , readRef  :: forall a. r a -> n a
  -- ^ Read a reference.
  , writeRef :: forall a. r a -> a -> n ()
  -- ^ Overwrite the contents of a reference.
  , liftN    :: forall a. n a -> c t a
  -- ^ Lift an action from the underlying monad
  , getCont  :: forall a. c t a -> M n r a
  -- ^ Unpack the continuation-based computation from its wrapping
  -- type.
  }

-- * Running @Conc@ Computations

-- | Scheduling is done in terms of a trace of 'Action's. Blocking can
-- only occur as a result of an action, and they cover (most of) the
-- primitives of the concurrency. 'spawn' is absent as it is
-- implemented in terms of 'newEmptyCVar', 'fork', and 'putCVar'.
data Action n r =
    AFork (Action n r) (Action n r)
  | forall a. APut     (R r a) a (Action n r)
  | forall a. ATryPut  (R r a) a (Bool -> Action n r)
  | forall a. AGet     (R r a) (a -> Action n r)
  | forall a. ATake    (R r a) (a -> Action n r)
  | forall a. ATryTake (R r a) (Maybe a -> Action n r)
  | ANew  (CVarId -> n (Action n r))
  | ALift (n (Action n r))
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
  rnf (BlockedRead c) = rnf c
  rnf (BlockedTake c) = rnf c
  rnf (BlockedPut  c) = rnf c
  rnf (Take c tids) = rnf (c, tids)
  rnf (Put  c tids) = rnf (c, tids)
  rnf (Fork tid) = rnf tid
  rnf (New  c) = rnf c
  rnf (Read c) = rnf c
  rnf Lift = ()
  rnf Stop = ()

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning a 'Just' if it terminates, and 'Nothing' if a
-- deadlock is detected. Also returned is the final state of the
-- scheduler, and an execution trace.
runFixed :: (Monad (c t), Monad n) => Fixed c n r t
          -> Scheduler s -> s -> c t a -> n (Maybe a, s, Trace)
runFixed fixed sched s ma = do
  ref <- newRef fixed Nothing

  let c       = getCont fixed $ ma >>= liftN fixed . writeRef fixed ref . Just
  let threads = M.fromList [(0, (runCont c $ const AStop, False))]

  (s', trace) <- runThreads fixed (-1, 0) [] (negate 1) sched s threads ref
  out         <- readRef fixed ref

  return (out, s', reverse trace)

-- * Running threads

-- | A @Block@ is used to determine what sort of block a thread is
-- experiencing.
data Block = WaitFull ThreadId | WaitEmpty ThreadId deriving Eq

-- | Threads are represented as a tuple of (next action, is blocked).
type Threads n r = Map ThreadId (Action n r, Bool)

-- | Run a collection of threads, until there are no threads left.
--
-- A thread is represented as a tuple of (next action, is blocked).
--
-- Note: this returns the trace in reverse order, because it's more
-- efficient to prepend to a list than append. As this function isn't
-- exposed to users of the library, this is just an internal gotcha to
-- watch out for.
runThreads :: (Monad (c t), Monad n) => Fixed c n r t
           -> (CVarId, ThreadId) -> Trace -> ThreadId -> Scheduler s -> s -> Threads n r -> r (Maybe a) -> n (s, Trace)
runThreads fixed (lastcvid, lasttid) sofar prior sched s threads ref
  | isTerminated  = return (s, sofar)
  | isDeadlocked  = writeRef fixed ref Nothing >> return (s, sofar)
  | isNonexistant = writeRef fixed ref Nothing >> return (s, sofar)
  | isBlocked     = writeRef fixed ref Nothing >> return (s, sofar)
  | otherwise = do
    (threads', act) <- stepThread (fst $ fromJust thread) fixed (lastcvid, lasttid) chosen threads
    let sofar' = (decision, alternatives, act) : sofar

    let lastcvid' = case act of { New  c -> c; _ -> lastcvid }
    let lasttid'  = case act of { Fork t -> t; _ -> lasttid  }

    runThreads fixed (lastcvid', lasttid') sofar' chosen sched s' threads' ref

  where
    (chosen, s')  = if prior == -1 then (0, s) else sched s prior $ head runnable' :| tail runnable'
    runnable'     = M.keys runnable
    runnable      = M.filter (not . snd) threads
    thread        = M.lookup chosen threads
    isBlocked     = snd $ fromJust thread
    isNonexistant = isNothing thread
    isTerminated  = 0 `notElem` M.keys threads
    isDeadlocked  = M.null runnable

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
stepThread :: (Monad (c t), Monad n)
           => Action n r
           -> Fixed c n r t -> (CVarId, ThreadId) -> ThreadId -> Threads n r -> n (Threads n r, ThreadAction)
stepThread action fixed (lastcvid, lasttid) tid threads = case action of
  AFork    a b     -> stepFork    a b
  APut     ref a c -> stepPut     ref a c
  ATryPut  ref a c -> stepTryPut  ref a c
  AGet     ref c   -> stepGet     ref c
  ATake    ref c   -> stepTake    ref c
  ATryTake ref c   -> stepTryTake ref c
  ANew     na      -> stepNew     na
  ALift    na      -> stepLift    na
  AStop            -> stepStop

  where
    -- | Start a new thread, assigning it the next 'ThreadId'
    stepFork a b = return (goto b tid threads', Fork newtid) where
      threads' = launch newtid a threads
      newtid   = lasttid + 1

    -- | Put a value into a @CVar@, blocking the thread until it's
    -- empty.
    stepPut ref a c = do
      (success, threads', woken) <- putIntoCVar True ref a (const c) fixed tid threads
      cvid <- getCVarId fixed ref
      return (threads', if success then Put cvid woken else BlockedPut cvid)

    -- | Try to put a value into a @CVar@, without blocking.
    stepTryPut ref a c= do
      (success, threads', woken) <- putIntoCVar False ref a c fixed tid threads
      cvid <- getCVarId fixed ref
      return (threads', TryPut cvid success woken)

    -- | Get the value from a @CVar@, without emptying, blocking the
    -- thread until it's full.
    stepGet ref c = do
      (cvid, val, _) <- readRef fixed ref
      case val of
        Just val' -> return (goto (c val') tid threads, Read cvid)
        Nothing   -> do
          threads' <- block fixed ref WaitFull tid threads
          return (threads', BlockedRead cvid)

    -- | Take the value from a @CVar@, blocking the thread until it's
    -- full.
    stepTake ref c = do
      (success, threads', woken) <- takeFromCVar True ref (c . fromJust) fixed tid threads
      cvid <- getCVarId fixed ref
      return (threads', if success then Take cvid woken else BlockedTake cvid)

    -- | Try to take the value from a @CVar@, without blocking.
    stepTryTake ref c = do
      (success, threads', woken) <- takeFromCVar True ref c fixed tid threads
      cvid <- getCVarId fixed ref
      return (threads', TryTake cvid success woken)

    -- | Create a new @CVar@, using the next 'CVarId'.
    stepNew na = do
      let newcvid = lastcvid + 1
      a <- na newcvid
      return (goto a tid threads, New newcvid)

    -- | Lift an action from the underlying monad into the @Conc@
    -- computation.
    stepLift na = do
      a <- na
      return (goto a tid threads, Lift)

    -- | Kill the current thread.
    stepStop = return (kill tid threads, Stop)

-- * Manipulating @CVar@s

-- | Get the ID of a CVar
getCVarId :: (Monad (c t), Monad n) => Fixed c n r t -> R r a -> n CVarId
getCVarId fixed ref = (\(cvid,_,_) -> cvid) `liftM` readRef fixed ref

-- | Put a value into a @CVar@, in either a blocking or nonblocking
-- way.
putIntoCVar :: (Monad (c t), Monad n)
            => Bool -> R r a -> a -> (Bool -> Action n r)
            -> Fixed c n r t -> ThreadId -> Threads n r -> n (Bool, Threads n r, [ThreadId])
putIntoCVar blocking ref a c fixed threadid threads = do
  (cvid, val, blocks) <- readRef fixed ref

  case val of
    Just _
      | blocking -> do
        threads' <- block fixed ref WaitEmpty threadid threads
        return (False, threads', [])

      | otherwise ->
        return (False, goto (c False) threadid threads, [])

    Nothing -> do
      writeRef fixed ref (cvid, Just a, blocks)
      (threads', woken) <- wake fixed ref WaitFull threads
      return (True, goto (c True) threadid threads', woken)

-- | Take a value from a @CVar@, in either a blocking or nonblocking
-- way.
takeFromCVar :: (Monad (c t), Monad n)
             => Bool -> R r a -> (Maybe a -> Action n r)
             -> Fixed c n r t -> ThreadId -> Threads n r -> n (Bool, Threads n r, [ThreadId])
takeFromCVar blocking ref c fixed threadid threads = do
  (cvid, val, blocks) <- readRef fixed ref

  case val of
    Just _ -> do
      writeRef fixed ref (cvid, Nothing, blocks)
      (threads', woken) <- wake fixed ref WaitEmpty threads
      return (True, goto (c val) threadid threads', woken)

    Nothing
      | blocking -> do
        threads' <- block fixed ref WaitFull threadid threads
        return (False, threads', [])

      | otherwise ->
        return (False, goto (c Nothing) threadid threads, [])

-- * Manipulating threads

-- | Replace the @Action@ of a thread.
goto :: Action n r -> ThreadId -> Threads n r -> Threads n r
goto a = M.alter $ \(Just (_, b)) -> Just (a, b)

-- | Block a thread on a @CVar@.
block :: (Monad (c t), Monad n) => Fixed c n r t
      -> R r a -> (ThreadId -> Block) -> ThreadId -> Threads n r -> n (Threads n r)
block fixed ref typ tid threads = do
  (cvid, val, blocks) <- readRef fixed ref
  writeRef fixed ref (cvid, val, typ tid : blocks)
  return $ M.alter (\(Just (a, _)) -> Just (a, True)) tid threads

-- | Start a thread with the given ID. This must not already be in use!
launch :: ThreadId -> Action n r -> Threads n r -> Threads n r
launch tid a = M.insert tid (a, False)

-- | Kill a thread.
kill :: ThreadId -> Threads n r -> Threads n r
kill = M.delete

-- | Wake every thread blocked on a @CVar@ read/write.
wake :: (Monad (c t), Monad n) => Fixed c n r t
     -> R r a -> (ThreadId -> Block) -> Threads n r -> n (Threads n r, [ThreadId])
wake fixed ref typ m = do
  (m', woken) <- mapAndUnzipM wake' (M.toList m)

  return (M.fromList m', catMaybes woken)

  where
    wake' a@(tid, (act, True)) = do
      let blck = typ tid
      (cvid, val, blocks) <- readRef fixed ref

      if blck `elem` blocks
      then writeRef fixed ref (cvid, val, filter (/= blck) blocks) >> return ((tid, (act, False)), Just tid)
      else return (a, Nothing)

    wake' a = return (a, Nothing)

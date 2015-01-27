{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

-- | Concurrent monads with a fixed scheduler: internal types and
-- functions.
module Control.Monad.Conc.Fixed.Internal where

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

-- | CVars are represented as a reference containing a maybe value,
-- and a list of things blocked on it.
type R r a = r (Maybe a, [Block])

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
  | ANew  (n (Action n r))
  | ALift (n (Action n r))
  | AStop

-- | Every live thread has a unique identitifer. These are implemented
-- as integers, but you shouldn't assume they are necessarily
-- contiguous.
type ThreadId = Int

-- | A @Scheduler@ maintains some internal state, @s@, takes the
-- 'ThreadId' of the last thread scheduled, and the list of runnable
-- threads. It produces a 'ThreadId' to schedule, and a new state.
--
-- Note: In order to prevent computation from hanging, the 'Conc'
-- runtime will assume that a deadlock situation has arisen if the
-- scheduler attempts to (a) schedule a blocked thread, or (b)
-- schedule a nonexistent thread. In either of those cases, the
-- computation will be halted.
type Scheduler s = s -> ThreadId -> NonEmpty ThreadId -> (ThreadId, s)

-- | One of the outputs of the runner is a @Trace@, which is just a
-- log of threads and actions they have taken.
type Trace = [(ThreadId, ThreadAction)]

-- | All the actions that a thread can perform.
data ThreadAction =
    Fork ThreadId
  -- ^ Start a new thread.
  | New
  -- ^ Create a new 'CVar'.
  | Put [ThreadId]
  -- ^ Put into a 'CVar', possibly waking up some threads.
  | BlockedPut
  -- ^ Get blocked on a put.
  | TryPut Bool [ThreadId]
  -- ^ Try to put into a 'CVar', possibly waking up some threads.
  | Read
  -- ^ Read from a 'CVar'.
  | BlockedRead
  -- ^ Get blocked on a read.
  | Take [ThreadId]
  -- ^ Take from a 'CVar', possibly waking up some threads.
  | BlockedTake
  -- ^ Get blocked on a take.
  | TryTake Bool [ThreadId]
  -- ^ Try to take from a 'CVar', possibly waking up some threads.
  | Lift
  -- ^ Lift an action from the underlying monad. Note that the
  -- penultimate action in a trace will always be a @Lift@, this is an
  -- artefact of how the runner works.
  | Stop
  -- ^ Cease execution and terminate.
  deriving (Eq, Show)

instance NFData ThreadAction where
  rnf (TryTake b tids) = rnf (b, tids)
  rnf (TryPut  b tids) = rnf (b, tids)
  rnf (Fork tid)  = rnf tid
  rnf (Take tids) = rnf tids
  rnf (Put  tids) = rnf tids
  rnf BlockedRead = ()
  rnf BlockedTake = ()
  rnf BlockedPut  = ()
  rnf New  = ()
  rnf Read = ()
  rnf Lift = ()
  rnf Stop = ()

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning a 'Just' if it terminates, and 'Nothing' if a
-- deadlock is detected.
runFixed :: (Monad (c t), Monad n) => Fixed c n r t
         -> Scheduler s -> s -> c t a -> n (Maybe a)
runFixed fixed sched s ma = liftM (\(a,_,_) -> a) $ runFixed' fixed sched s ma

-- | Variant of 'runFixed' which returns the final state of the
-- scheduler and an execution trace.
runFixed' :: (Monad (c t), Monad n) => Fixed c n r t
          -> Scheduler s -> s -> c t a -> n (Maybe a, s, Trace)
runFixed' fixed sched s ma = do
  ref <- newRef fixed Nothing

  let c       = getCont fixed $ ma >>= liftN fixed . writeRef fixed ref . Just
  let threads = M.fromList [(0, (runCont c $ const AStop, False))]

  (s', trace) <- runThreads fixed [] (negate 1) sched s threads ref
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
           -> Trace -> ThreadId -> Scheduler s -> s -> Threads n r -> r (Maybe a) -> n (s, Trace)
runThreads fixed sofar prior sched s threads ref
  | isTerminated  = return (s, sofar)
  | isDeadlocked  = writeRef fixed ref Nothing >> return (s, sofar)
  | isNonexistant = writeRef fixed ref Nothing >> return (s, sofar)
  | isBlocked     = writeRef fixed ref Nothing >> return (s, sofar)
  | otherwise = do
    (threads', act) <- stepThread (fst $ fromJust thread) fixed chosen threads
    let sofar' = (chosen, act) : sofar
    runThreads fixed sofar' chosen sched s' threads' ref

  where
    (chosen, s')  = if prior == -1 then (0, s) else sched s prior $ head runnable' :| tail runnable'
    runnable'     = M.keys runnable
    runnable      = M.filter (not . snd) threads
    thread        = M.lookup chosen threads
    isBlocked     = snd $ fromJust thread
    isNonexistant = isNothing thread
    isTerminated  = 0 `notElem` M.keys threads
    isDeadlocked  = M.null runnable

-- | Run a single thread one step, by dispatching on the type of
-- 'Action'.
stepThread :: (Monad (c t), Monad n)
           => Action n r
           -> Fixed c n r t -> ThreadId -> Threads n r -> n (Threads n r, ThreadAction)
stepThread (AFork    a b)     = stepFork    a b
stepThread (APut     ref a c) = stepPut     ref a c
stepThread (ATryPut  ref a c) = stepTryPut  ref a c
stepThread (AGet     ref c)   = stepGet     ref c
stepThread (ATake    ref c)   = stepTake    ref c
stepThread (ATryTake ref c)   = stepTryTake ref c
stepThread (ANew     na)      = stepNew     na
stepThread (ALift    na)      = stepLift    na
stepThread  AStop             = stepStop

-- | Start a new thread, assigning it a unique 'ThreadId'
stepFork :: (Monad (c t), Monad n)
         => Action n r -> Action n r
         -> Fixed c n r t -> ThreadId -> Threads n r -> n (Threads n r, ThreadAction)
stepFork a b _ i threads =
  let (threads', newid) = launch a threads
  in return (goto b i threads', Fork newid)

-- | Put a value into a @CVar@, blocking the thread until it's empty.
stepPut :: (Monad (c t), Monad n)
        => R r a -> a -> Action n r
        -> Fixed c n r t -> ThreadId -> Threads n r -> n (Threads n r, ThreadAction)
stepPut ref a c fixed i threads = do
  (val, blocks) <- readRef fixed ref
  case val of
    Just _  -> do
      threads' <- block fixed ref WaitEmpty i threads
      return (threads', BlockedPut)
    Nothing -> do
      writeRef fixed ref (Just a, blocks)
      (threads', woken) <- wake fixed ref WaitFull threads
      return (goto c i threads', Put woken)

-- | Try to put a value into a @CVar@, without blocking.
stepTryPut :: (Monad (c t), Monad n)
           => R r a -> a -> (Bool -> Action n r)
           -> Fixed c n r t -> ThreadId -> Threads n r -> n (Threads n r, ThreadAction)
stepTryPut ref a c fixed i threads = do
  (val, blocks) <- readRef fixed ref
  case val of
    Just _  -> return (goto (c False) i threads, TryPut False [])
    Nothing -> do
      writeRef fixed ref (Just a, blocks)
      (threads', woken) <- wake fixed ref WaitFull threads
      return (goto (c True) i threads', TryPut True woken)

-- | Get the value from a @CVar@, without emptying, blocking the
-- thread until it's full.
stepGet :: (Monad (c t), Monad n)
        => R r a -> (a -> Action n r)
        -> Fixed c n r t -> ThreadId -> Threads n r -> n (Threads n r, ThreadAction)
stepGet ref c fixed i threads = do
  (val, _) <- readRef fixed ref
  case val of
    Just val' -> return (goto (c val') i threads, Read)
    Nothing   -> do
      threads' <- block fixed ref WaitFull i threads
      return (threads', BlockedRead)

-- | Take the value from a @CVar@, blocking the thread until it's
-- full.
stepTake :: (Monad (c t), Monad n)
         => R r a -> (a -> Action n r)
         -> Fixed c n r t -> ThreadId -> Threads n r -> n (Threads n r, ThreadAction)
stepTake ref c fixed i threads = do
  (val, blocks) <- readRef fixed ref
  case val of
    Just val' -> do
      writeRef fixed ref (Nothing, blocks)
      (threads', woken) <- wake fixed ref WaitEmpty threads
      return (goto (c val') i threads', Take woken)
    Nothing   -> do
      threads' <- block fixed ref WaitFull i threads
      return (threads', BlockedTake)

-- | Try to take the value from a @CVar@, without blocking.
stepTryTake :: (Monad (c t), Monad n)
            => R r a -> (Maybe a -> Action n r)
            -> Fixed c n r t -> ThreadId -> Threads n r -> n (Threads n r, ThreadAction)
stepTryTake ref c fixed i threads = do
  (val, blocks) <- readRef fixed ref
  case val of
    Just _ -> do
      writeRef fixed ref (Nothing, blocks)
      (threads', woken) <- wake fixed ref WaitEmpty threads
      return (goto (c val) i threads', TryTake True woken)
    Nothing -> return (goto (c Nothing) i threads, TryTake False [])

-- | Create a new @CVar@. This is exactly the same as lifting a value,
-- except by separating the two we can (a) produce a more useful
-- trace, and (b) make smarter pre-emption decisions.
stepNew :: (Monad (c t), Monad n)
        => n (Action n r)
        -> Fixed c n r t -> ThreadId -> Threads n r -> n (Threads n r, ThreadAction)
stepNew na _ i threads = do
  a <- na
  return (goto a i threads, New)

-- | Lift an action from the underlying monad into the @Conc@
-- computation.
stepLift :: (Monad (c t), Monad n)
         => n (Action n r)
         -> Fixed c n r t -> ThreadId -> Threads n r -> n (Threads n r, ThreadAction)
stepLift na _ i threads = do
  a <- na
  return (goto a i threads, Lift)

-- | Kill the current thread.
stepStop :: (Monad (c t), Monad n)
         => Fixed c n r t -> ThreadId -> Threads n r -> n (Threads n r, ThreadAction)
stepStop _ i threads = return (kill i threads, Stop)

-- * Manipulating threads

-- | Replace the @Action@ of a thread.
goto :: Action n r -> ThreadId -> Threads n r -> Threads n r
goto a = M.alter $ \(Just (_, b)) -> Just (a, b)

-- | Block a thread on a @CVar@.
block :: (Monad (c t), Monad n) => Fixed c n r t
      -> R r a -> (ThreadId -> Block) -> ThreadId -> Threads n r -> n (Threads n r)
block fixed ref typ tid threads = do
  (val, blocks) <- readRef fixed ref
  writeRef fixed ref (val, typ tid : blocks)
  return $ M.alter (\(Just (a, _)) -> Just (a, True)) tid threads

-- | Start a thread with the next free ID.
launch :: Action n r -> Threads n r -> (Threads n r, ThreadId)
launch a m = (M.insert k (a, False) m, k) where
  k = succ . maximum $ M.keys m

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
      (val, blocks) <- readRef fixed ref

      if blck `elem` blocks
      then writeRef fixed ref (val, filter (/= blck) blocks) >> return ((tid, (act, False)), Just tid)
      else return (a, Nothing)

    wake' a = return (a, Nothing)

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

-- | Concurrent monads with a fixed scheduler.
module Control.Monad.Conc.Fixed
  ( -- * The Conc Monad
    Conc
  , Trace
  , ThreadAction(..)
  , runConc
  , runConc'
  , liftIO
  , spawn
  , fork

  -- * Communication: CVars
  , CVar
  , newEmptyCVar
  , putCVar
  , tryPutCVar
  , readCVar
  , takeCVar
  , tryTakeCVar

  -- * Scheduling
  , Scheduler
  , ThreadId
  , randomSched
  , randomSchedNP
  , roundRobinSched
  , roundRobinSchedNP
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Cont (Cont, cont, runCont)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Random (RandomGen, randomR)

import qualified Control.Monad.Conc.Class as C
import qualified Control.Monad.IO.Class as IO
import qualified Data.Map as M

-- | Scheduling is done in terms of a trace of 'Action's. Blocking can
-- only occur as a result of an action, and they cover (most of) the
-- primitives of the concurrency. `spawn` is absent as it can be
-- derived from `new`, `fork` and `put`.
data Action =
    AFork Action Action
  | forall t a. APut     (CVar t a) a Action
  | forall t a. ATryPut  (CVar t a) a (Bool -> Action)
  | forall t a. AGet     (CVar t a) (a -> Action)
  | forall t a. ATake    (CVar t a) (a -> Action)
  | forall t a. ATryTake (CVar t a) (Maybe a -> Action)
  | ALift (IO Action)
  | AStop

-- | The @Conc@ monad itself. Under the hood, this uses continuations
-- so it's able to interrupt and resume a monadic computation at any
-- point where a primitive is used.
--
-- This uses the same universally-quantified indexing state trick as
-- used by 'ST' and 'STRef's to prevent mutable references from
-- leaking out of the monad. See 'runConc' for an example of what this
-- means.
newtype Conc t a = C (Cont Action a) deriving (Functor, Applicative, Monad)

instance IO.MonadIO (Conc t) where
  liftIO = liftIO

instance C.ConcFuture (CVar t) (Conc t) where
  spawn    = spawn
  readCVar = readCVar

instance C.ConcCVar (CVar t) (Conc t) where
  fork         = fork
  newEmptyCVar = newEmptyCVar
  putCVar      = putCVar
  tryPutCVar   = tryPutCVar
  takeCVar     = takeCVar
  tryTakeCVar  = tryTakeCVar

-- | The concurrent variable type used with the 'Conc'
-- monad. Internally, these are implemented as 'IORef's, but they are
-- structured to behave fairly similarly to 'MVar's. One notable
-- difference is that 'MVar's are single-wakeup, and wake up in a FIFO
-- order. Writing to a @CVar@ wakes up all threads blocked on reading
-- it, and it is up to the scheduler which one runs next. Taking from
-- a @CVar@ behaves analogously.
newtype CVar t a = V (IORef (Maybe a, [Block])) deriving Eq

-- | Lift an 'IO' action into the 'Conc' monad.
--
-- Caution! Blocking on the action of another thread in @liftIO@
-- cannot be detected! So if you perform some potentially blocking
-- action in a 'liftIO' the entire collection of threads may deadlock!
-- You should therefore keep 'IO' blocks small, and only perform
-- blocking operations with the supplied primitives, insofar as
-- possible.
liftIO :: IO a -> Conc t a
liftIO ma = C $ cont lifted where
  lifted c = ALift $ c <$> ma

-- | Run the provided computation concurrently, returning the result.
spawn :: Conc t a -> Conc t (CVar t a)
spawn ma = do
  cvar <- newEmptyCVar
  fork $ ma >>= putCVar cvar
  return cvar

-- | Block on a 'CVar' until it is full, then read from it (without
-- emptying).
readCVar :: CVar t a -> Conc t a
readCVar cvar = C $ cont $ AGet cvar

-- | Run the provided computation concurrently.
fork :: Conc t () -> Conc t ()
fork (C ma) = C $ cont $ \c -> AFork (runCont ma $ const AStop) $ c ()

-- | Create a new empty 'CVar'.
newEmptyCVar :: Conc t (CVar t a)
newEmptyCVar = liftIO $ do
  ioref <- newIORef (Nothing, [])
  return $ V ioref

-- | Block on a 'CVar' until it is empty, then write to it.
putCVar :: CVar t a -> a -> Conc t ()
putCVar cvar a = C $ cont $ \c -> APut cvar a $ c ()

-- | Put a value into a 'CVar' if there isn't one, without blocking.
tryPutCVar :: CVar t a -> a -> Conc t Bool
tryPutCVar cvar a = C $ cont $ ATryPut cvar a

-- | Block on a 'CVar' until it is full, then read from it (with
-- emptying).
takeCVar :: CVar t a -> Conc t a
takeCVar cvar = C $ cont $ ATake cvar

-- | Read a value from a 'CVar' if there is one, without blocking.
tryTakeCVar :: CVar t a -> Conc t (Maybe a)
tryTakeCVar cvar = C $ cont $ ATryTake cvar

-- | Every thread has a unique identitifer. These are implemented as
-- integers, but you shouldn't assume they are necessarily contiguous.
type ThreadId = Int

-- | A @Scheduler@ maintains some internal state, `s`, takes the
-- 'ThreadId' of the last thread scheduled, and the list of runnable
-- threads (which will never be empty). It produces a 'ThreadId' to
-- schedule, and a new state.
--
-- Note: In order to prevent deadlock, the 'Conc' runtime will assume
-- that a deadlock situation has arisen if the scheduler attempts to
-- (a) schedule a blocked thread, or (b) schedule a nonexistant
-- thread. In either of those cases, the computation will be halted.
type Scheduler s = s -> ThreadId -> [ThreadId] -> (ThreadId, s)

-- | One of the outputs of the runner is a @Trace@, which is just a
-- log of threads and actions they have taken.
type Trace = [(ThreadId, ThreadAction)]

-- | All the actions that a thread can perform.
data ThreadAction =
    Fork ThreadId
  -- ^ Start a new thread.
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
  -- ^ try to take from a 'CVar', possibly waking up some threads.
  | IO
  -- ^ Perform some IO.
  deriving (Eq, Show)

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning `Just result` if it terminates, and `Nothing` if a
-- deadlock is detected.
--
-- Note how the `t` in 'Conc' is universally quantified, what this
-- means in practice is that you can't do something like this:
--
-- > runConc (\s _ (x:_) -> (x, s)) () $ new >>= return
--
-- So 'CVar's cannot leak out of the 'Conc' computation. If this is
-- making your head hurt, check out the \"How `runST` works\" section
-- of <https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html>
runConc :: Scheduler s -> s -> (forall t. Conc t a) -> IO (Maybe a)
runConc sched s ma = (\(a,_,_) -> a) <$> runConc' sched s ma

-- | variant of 'runConc' which returns the final state of the
-- scheduler and an execution trace.
runConc' :: Scheduler s -> s -> (forall t. Conc t a) -> IO (Maybe a, s, Trace)
runConc' sched s ma = do
  mvar <- newEmptyMVar
  let (C c) = ma >>= liftIO . putMVar mvar . Just
  (s', trace) <- runThreads [] (negate 1) sched s (M.fromList [(0, (runCont c $ const AStop, False))]) mvar
  out <- takeMVar mvar
  return (out, s', reverse trace)

-- | A simple random scheduler which, at every step, picks a random
-- thread to run.
randomSched :: RandomGen g => Scheduler g
randomSched g _ threads = (threads !! choice, g') where
  (choice, g') = randomR (0, length threads - 1) g

-- | A random scheduler which doesn't pre-empt the running
-- thread. That is, if the last thread scheduled is still runnable,
-- run that, otherwise schedule randomly.
randomSchedNP :: RandomGen g => Scheduler g
randomSchedNP = makeNP randomSched

-- | A round-robin scheduler which, at every step, schedules the
-- thread with the next 'ThreadId'.
roundRobinSched :: Scheduler ()
roundRobinSched _ prior threads
  | prior >= maximum threads = (minimum threads, ())
  | otherwise = (minimum $ filter (>prior) threads, ())

-- | A round-robin scheduler which doesn't pre-empt the running
-- thread.
roundRobinSchedNP :: Scheduler ()
roundRobinSchedNP = makeNP roundRobinSched

-- | Turn a potentially pre-emptive scheduler into a non-preemptive
-- one.
makeNP :: Scheduler s -> Scheduler s
makeNP sched = newsched where
  newsched s prior threads
    | prior `elem` threads = (prior, s)
    | otherwise = sched s prior threads

-------------------- Internal stuff --------------------

-- | A @Block@ is used to determine what sort of block a thread is
-- experiencing.
data Block = WaitFull ThreadId | WaitEmpty ThreadId deriving Eq

-- | Run a collection of threads, until there are no threads left.
--
-- A thread is represented as a tuple of (next action, is blocked).
--
-- Note: this returns the trace in reverse order, because it's more
-- efficient to prepend to a list than append. As this function isn't
-- exposed to users of the library, this is just an internal gotcha to
-- watch out for.
runThreads :: Trace -> ThreadId -> Scheduler s -> s -> Map ThreadId (Action, Bool) -> MVar (Maybe a) -> IO (s, Trace)
runThreads sofar prior sched s threads mvar
  | isTerminated  = return (s, sofar)
  | isDeadlocked  = putMVar mvar Nothing >> return (s, sofar)
  | isBlocked     = putStrLn "Attempted to run a blocked thread, assuming deadlock."     >> putMVar mvar Nothing >> return (s, sofar)
  | isNonexistant = putStrLn "Attempted to run a nonexistant thread, assuming deadlock." >> putMVar mvar Nothing >> return (s, sofar)
  | otherwise = do
    (threads', act) <- runThread (fst $ fromJust thread, chosen) threads
    let sofar' = maybe sofar (\a -> (chosen, a) : sofar) act
    runThreads sofar' chosen sched s' threads' mvar

  where
    (chosen, s')  = if prior == -1 then (0, s) else sched s prior $ M.keys runnable
    runnable      = M.filter (not . snd) threads
    thread        = M.lookup chosen threads
    isBlocked     = snd . fromJust $ M.lookup chosen threads
    isNonexistant = isNothing thread
    isTerminated  = 0 `notElem` M.keys threads
    isDeadlocked  = M.null runnable

-- | Run a single thread one step, by dispatching on the type of
-- 'Action'.
runThread :: (Action, ThreadId) -> Map ThreadId (Action, Bool) -> IO (Map ThreadId (Action, Bool), Maybe ThreadAction)
runThread (AFork a b, i) threads =
  let (threads', newid) = launch a threads
  in return (goto b i threads', Just $ Fork newid)

runThread (APut v a c, i) threads = do
  let (V ref) = v
  (val, blocks) <- readIORef ref
  case val of
    Just _  -> do
      threads' <- block v WaitEmpty i threads
      return (threads', Just BlockedPut)
    Nothing -> do
      writeIORef ref (Just a, blocks)
      (threads', woken) <- wake v WaitFull threads
      return (goto c i threads', Just $ Put woken)

runThread (ATryPut v a c, i) threads = do
  let (V ref) = v
  (val, blocks) <- readIORef ref
  case val of
    Just _  -> return (goto (c False) i threads, Just $ TryPut False [])
    Nothing -> do
      writeIORef ref (Just a, blocks)
      (threads', woken) <- wake v WaitFull threads
      return (goto (c True) i threads', Just $ TryPut True woken)

runThread (AGet v c, i) threads = do
  let (V ref) = v
  (val, _) <- readIORef ref
  case val of
    Just val' -> return (goto (c val') i threads, Just Read)
    Nothing   -> do
      threads' <- block v WaitFull i threads
      return (threads', Just BlockedRead)

runThread (ATake v c, i) threads = do
  let (V ref) = v
  (val, blocks) <- readIORef ref
  case val of
    Just val' -> do
      writeIORef ref (Nothing, blocks)
      (threads', woken) <- wake v WaitEmpty threads
      return (goto (c val') i threads', Just $ Take woken)
    Nothing   -> do
      threads' <- block v WaitFull i threads
      return (threads', Just BlockedTake)

runThread (ATryTake v c, i) threads = do
  let (V ref) = v
  (val, blocks) <- readIORef ref
  case val of
    Just _ -> do
      writeIORef ref (Nothing, blocks)
      (threads', woken) <- wake v WaitEmpty threads
      return (goto (c val) i threads', Just $ TryTake True woken)
    Nothing   -> return (goto (c Nothing) i threads, Just $ TryTake False [])

runThread (ALift io, i) threads = do
  a <- io
  return (goto a i threads, Just IO)

runThread (AStop, i) threads = return (kill i threads, Nothing)

-- | Replace the 'Action' of a thread.
goto :: Ord k => a -> k -> Map k (a, b) -> Map k (a, b)
goto a = M.alter $ \(Just (_, b)) -> Just (a, b)

-- | Block a thread on a 'CVar'.
block :: Ord k => CVar t v -> (k -> Block) -> k -> Map k (a, Bool) -> IO (Map k (a, Bool))
block (V ref) typ tid threads = do
  (val, blocks) <- readIORef ref
  writeIORef ref (val, typ tid : blocks)
  return $ M.alter (\(Just (a, _)) -> Just (a, True)) tid threads

-- | Start a thread with the next free ID.
launch :: (Ord k, Enum k) => a -> Map k (a, Bool) -> (Map k (a, Bool), k)
launch a m = (M.insert k (a, False) m, k) where
  k = succ . maximum $ M.keys m

-- | Kill a thread.
kill :: Ord k => k -> Map k (a, b) -> Map k (a, b)
kill = M.delete

-- | Wake every thread blocked on a 'CVar' read/write.
wake :: Ord k => CVar t v -> (k -> Block) -> Map k (a, Bool) -> IO (Map k (a, Bool), [k])
wake (V ref) typ m = do
  (m', woken) <- unzip <$> mapM wake' (M.toList m)

  return (M.fromList m', catMaybes woken)

  where
    wake' a@(tid, (act, True)) = do
      let blck = typ tid
      (val, blocks) <- readIORef ref

      if blck `elem` blocks
      then writeIORef ref (val, filter (/= blck) blocks) >> return ((tid, (act, False)), Just tid)
      else return (a, Nothing)

    wake' a = return (a, Nothing)

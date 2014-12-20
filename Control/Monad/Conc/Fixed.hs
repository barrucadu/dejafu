{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

-- | Concurrent monads with a fixed scheduler.
module Control.Monad.Conc.Fixed
  ( -- * The Conc Monad
    Conc
  , runConc
  , runConc'
  , liftIO
  , spawn
  , fork

  -- * Communication: CVars
  , CVar
  , new
  , put
  , get
  , take
  , tryTake

  -- * Scheduling
  , Scheduler
  , ThreadId
  , randomSched
  , randomSchedNP
  , roundRobinSched
  , roundRobinSchedNP
  ) where

import Prelude hiding (take)

import Control.Applicative (Applicative(..), (<$>))
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Monad.Cont (Cont, cont, runCont)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe, isNothing, isJust)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import System.Random (RandomGen, randomR)

import qualified Control.Monad.Conc.Class as C
import qualified Control.Monad.IO.Class as IO
import qualified Data.Map as M

-- | Scheduling is done in terms of a trace of 'Action's. Blocking can
-- only occur as a result of an action, and they cover (most of) the
-- primitives of the concurrency. `spawn` is absent as it can be
-- derived from `new`, `fork` and `put`.
data Action =
    Fork Action Action
  | forall t a. Put     (CVar t a) a Action
  | forall t a. Get     (CVar t a) (a -> Action)
  | forall t a. Take    (CVar t a) (a -> Action)
  | forall t a. TryTake (CVar t a) (Maybe a -> Action)
  | Lift (IO Action)
  | Stop

-- | The @Conc@ monad itself. Under the hood, this uses continuations
-- so it's able to interrupt and resume a monadic computation at any
-- point where a primitive is used.
--
-- This uses the same universally-quantified indexing state trick as
-- used by 'ST' and 'STRef's to prevent mutable references from
-- leaking out of the monad. See 'runConc' for an example of what this
-- means.
newtype Conc t a = C (Cont (Action) a) deriving (Functor, Applicative, Monad)

instance IO.MonadIO (Conc t) where
  liftIO = liftIO

instance C.ConcFuture (CVar t) (Conc t) where
  spawn = spawn
  get   = get

instance C.ConcCVar (CVar t) (Conc t) where
  fork    = fork
  new     = new
  put     = put
  take    = take
  tryTake = tryTake

-- | The concurrent variable type used with the 'Conc'
-- monad. Internally, these are implemented as 'IORef's, but they are
-- structured to behave fairly similarly to 'MVar's. One notable
-- difference is that 'MVar's are single-wakeup, and wake up in a FIFO
-- order. @CVar@s wake up all blocked threads, and it is up to the
-- scheduler which one runs next.
--
-- In fact, due to implementation failings (which will be fixed!) at
-- the moment, mutating a @CVar@ wakes up /all/ threads blocking in an
-- appropriate way (i.e., on read or write, depending on the
-- mutation), not just the ones blocked on this particular @CVar@.
newtype CVar t a = V (IORef (Maybe a)) deriving Eq

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
  lifted c = Lift $ c <$> ma

-- | Run the provided computation concurrently, returning the result.
spawn :: Conc t a -> Conc t (CVar t a)
spawn ma = do
  cvar <- new
  fork $ ma >>= put cvar
  return cvar

-- | Block on a 'CVar' until it is full, then read from it (without
-- emptying).
get :: CVar t a -> Conc t a
get cvar = C $ cont $ Get cvar

-- | Run the provided computation concurrently.
fork :: Conc t () -> Conc t ()
fork (C ma) = C $ cont $ \c -> Fork (runCont ma $ const Stop) $ c ()

-- | Create a new empty 'CVar'.
new :: Conc t (CVar t a)
new = liftIO $ do
  ioref <- newIORef Nothing
  return $ V ioref

-- | Block on a 'CVar' until it is empty, then write to it.
put :: CVar t a -> a -> Conc t ()
put cvar a = C $ cont $ \c -> Put cvar a $ c ()

-- | Block on a 'CVar' until it is full, then read from it (with
-- emptying).
take :: CVar t a -> Conc t a
take cvar = C $ cont $ Take cvar

-- | Read a value from a 'CVar' if there is one, without blocking.
tryTake :: CVar t a -> Conc t (Maybe a)
tryTake cvar = C $ cont $ TryTake cvar

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
runConc sched s ma = fst <$> runConc' sched s ma

-- | variant of 'runConc' which returns the final state of the
-- scheduler.
runConc' :: Scheduler s -> s -> (forall t. Conc t a) -> IO (Maybe a, s)
runConc' sched s ma = do
  mvar <- newEmptyMVar
  let (C c) = ma >>= liftIO . putMVar mvar . Just
  s' <- runThreads (negate 1) sched s (M.fromList [(0, (runCont c $ const Stop, Nothing))]) mvar
  out <- takeMVar mvar
  return (out, s')

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
roundRobinSched _ last threads
  | last >= maximum threads = (minimum threads, ())
  | otherwise = (minimum $ filter (<=last) threads, ())

-- | A round-robin scheduler which doesn't pre-empt the running
-- thread.
roundRobinSchedNP :: Scheduler ()
roundRobinSchedNP = makeNP roundRobinSched

-- | Turn a potentially pre-emptive scheduler into a non-preemptive
-- one.
makeNP :: Scheduler s -> Scheduler s
makeNP sched = newsched where
  newsched s last threads
    | last `elem` threads = (last, s)
    | otherwise = sched s last threads

-------------------- Internal stuff --------------------

-- | A @Block@ is used to determine what sort of block a thread is
-- experiencing.
data Block = WaitFull | WaitEmpty

-- | Run a collection of threads, until there are no threads left.
runThreads :: ThreadId -> Scheduler s -> s -> Map ThreadId (Action, Maybe Block) -> MVar (Maybe a) -> IO s
runThreads last sched s threads mvar
  | M.null threads   = return s
  | M.null runnable  = do
    -- If we're here, it's possible the main thread has terminated
    -- successfully, in which case we DON'T have a deadlock.
    val <- fromMaybe Nothing <$> tryTakeMVar mvar
    putMVar mvar val
    return s

  | isBlocked        = putStrLn "Attempted to run a blocked thread, assuming deadlock."     >> putMVar mvar Nothing >> return s
  | isNothing thread = putStrLn "Attempted to run a nonexistant thread, assuming deadlock." >> putMVar mvar Nothing >> return s
  | otherwise = do
    threads <- runThread (fst $ fromJust thread, chosen) threads
    runThreads chosen sched s' threads mvar

  where
    (chosen, s') = if last == -1 then (0, s) else sched s last $ M.keys runnable
    runnable     = M.filter (isNothing . snd) threads
    thread       = M.lookup chosen threads
    isBlocked    = isJust . snd . fromJust $ M.lookup chosen threads

-- | Run a single thread one step, by dispatching on the type of
-- 'Action'.
runThread :: (Action, ThreadId) -> Map ThreadId (Action, Maybe Block) -> IO (Map ThreadId (Action, Maybe Block))
runThread (Fork a b, i) threads = return . goto b i $ launch a threads

runThread (Put v a c, i) threads = do
  let (V ref) = v
  val <- readIORef ref
  case val of
    Just _  -> return $ block WaitEmpty i threads
    Nothing -> do
      writeIORef ref $ Just a
      return . goto c i $ wakeGetters threads

runThread (Get v c, i) threads = do
  let (V ref) = v
  val <- readIORef ref
  case val of
    Just val' -> return $ goto (c val') i threads
    Nothing   -> return $ block WaitFull i threads

runThread (Take v c, i) threads = do
  let (V ref) = v
  val <- readIORef ref
  case val of
    Just val' -> do
      writeIORef ref Nothing
      return . goto (c val') i $ wakePutters threads
    Nothing   -> return $ block WaitFull i threads

runThread (TryTake v c, i) threads = do
  let (V ref) = v
  val <- readIORef ref
  return $ goto (c val) i threads

runThread (Lift io, i) threads = do
  a <- io
  return $ goto a i threads

runThread (Stop, i) threads = return $ kill i threads

-- | Replace the 'Action' of a thread.
goto :: Ord k => a -> k -> Map k (a, b) -> Map k (a, b)
goto a = M.alter $ \(Just (_, b)) -> Just (a, b)

-- | Set the 'Block' of a thread.
block :: Ord k => b -> k -> Map k (a, Maybe b) -> Map k (a, Maybe b)
block b = M.alter $ \(Just (a, _)) -> Just (a, Just b)

-- | Start a thread with the next free ID.
launch :: (Ord k, Enum k) => a -> Map k (a, Maybe b) -> Map k (a, Maybe b)
launch a m = M.insert (succ . maximum $ M.keys m) (a, Nothing) m

-- | Kill a thread.
kill :: Ord k => k -> Map k (a, b) -> Map k (a, b)
kill = M.delete

-- | Wake every thread blocked on a 'CVar' read.
wakeGetters :: Map k (a, Maybe Block) -> Map k (a, Maybe Block)
wakeGetters = M.map wake where
  wake (a, Just WaitFull) = (a, Nothing)
  wake a = a

-- | Wake every thread blocked on a 'CVar' write.
wakePutters :: Map k (a, Maybe Block) -> Map k (a, Maybe Block)
wakePutters = M.map wake where
  wake (a, Just WaitEmpty) = (a, Nothing)
  wake a = a

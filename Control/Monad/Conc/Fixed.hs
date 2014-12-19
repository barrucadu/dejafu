{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- | Concurrent monads with a fixed scheduler.
module Control.Monad.Conc.Fixed
  ( -- * The Conc Monad
    Conc
  , ThreadId
  , Scheduler
  , runConc

  -- * Communication: CVars
  , CVar
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Cont (Cont, cont, runCont)
import Control.Monad.Conc.Class
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')

import qualified Data.Map as M

-- | Scheduling is done in terms of a trace of 'Action's. Blocking can
-- only occur as a result of an action, and they cover (most of) the
-- primitives of the concurrency. `spawn` is absent as it can be
-- derived from `new`, `fork` and `put`.
data Action =
    Fork Action Action
  | forall a. Put     (CVar a) a Action
  | forall a. Get     (CVar a) (a -> Action)
  | forall a. Take    (CVar a) (a -> Action)
  | forall a. TryTake (CVar a) (Maybe a -> Action)
  | Lift (IO Action)
  | Stop

-- | The @Conc@ monad itself. Under the hood, this uses continuations
-- so it's able to interrupt and resume a monadic computation at any
-- point where a primitive is used.
--
-- Caution! Blocking on the action of another thread in the 'liftIO'
-- method of the 'MonadIO' instance cannot be detected! So if you
-- perform some potentially blocking action in a 'liftIO' the entire
-- collection of threads may deadlock! You should therefore keep 'IO'
-- blocks small, and only perform blocking operations with the
-- supplied primitives, insofar as possible.
newtype Conc a = C (Cont Action a) deriving (Functor, Applicative, Monad)

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
newtype CVar a = V (IORef (Maybe a)) deriving Eq

instance MonadIO Conc where
  liftIO ma = C $ cont lifted where
    lifted c = Lift $ c <$> ma

instance ConcFuture CVar Conc where
  spawn ma = do
    cvar <- new
    fork $ ma >>= put cvar
    return cvar

  get cvar = C $ cont $ Get cvar

instance ConcCVar CVar Conc where
  fork (C ma) = C $ cont $ \c -> Fork (runCont ma $ const Stop) $ c ()

  new = liftIO $ do
    ioref <- newIORef Nothing
    return $ V ioref

  put cvar a = C $ cont $ \c -> Put cvar a $ c ()

  take cvar = C $ cont $ Take cvar

  tryTake cvar = C $ cont $ TryTake cvar

-- | Every thread has a unique identitifer. These are implemented as
-- integers, but you shouldn't assume they are necessarily contiguous.
type ThreadId = Int

-- | A @Scheduler@ maintains some internal state, `s`, takes the ID of
-- the last thread scheduled, and the list of unblocked thread IDs. It
-- produces a thread ID to schedule, and a new state.
--
-- Note: The last thread to run may no longer be runnable, so it's not
-- safe to just run the same thread all the time.
--
-- Note also: In order to prevent deadlock, the 'Conc' runtime will
-- assume that a deadlock situation has arisen if the scheduler
-- attempts to (a) schedule a blocked thread, or (b) schedule a
-- nonexistant thread. In either of those cases, the computation will
-- be halted.
type Scheduler s = s -> ThreadId -> [ThreadId] -> (ThreadId, s)

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning `Just result` if it terminates, and `Nothing` if a
-- deadlock is detected.
runConc :: Scheduler s -> s -> Conc a -> IO (Maybe a)
runConc sched s ma = do
  mvar <- newEmptyMVar
  let (C c) = ma >>= liftIO . putMVar mvar . Just
  runThreads (negate 1) sched s (M.fromList [(0, (runCont c $ const Stop, Nothing))]) mvar
  takeMVar mvar

-------------------- Internal stuff --------------------

-- | A @Block@ is used to determine what sort of block a thread is
-- experiencing.
data Block = WaitFull | WaitEmpty

-- | Run a collection of threads, until there are no threads left.
runThreads :: ThreadId -> Scheduler s -> s -> Map ThreadId (Action, Maybe Block) -> MVar (Maybe a) -> IO ()
runThreads last sched s threads mvar
  | M.null threads   = return ()
  | blocked          = putStrLn "Attempted to run a blocked thread, assuming deadlock."     >> putMVar mvar Nothing
  | isNothing thread = putStrLn "Attempted to run a nonexistant thread, assuming deadlock." >> putMVar mvar Nothing
  | otherwise = do
    threads <- runThread (fst $ fromJust thread, chosen) threads
    runThreads chosen sched s' threads mvar

  where
    (chosen, s') = if last == -1 then (0, s) else sched s last $ M.keys $ M.filter (isNothing . snd) threads
    thread       = M.lookup chosen threads
    blocked      = isJust . snd . fromJust $ M.lookup chosen threads

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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ParallelListComp #-}

{-
The monad-par package:
https://hackage.haskell.org/package/monad-par

This is the code from Control.Monad.Par.Scheds.Direct, with CPP
expanded in its default configuration, modified to use MonadConc.

- - - - -

Copyright Simon Marlow, Ryan Newton 2011

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the authors nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE FlexibleInstances #-}

-- | A scheduler for the Par monad based on directly performing IO
-- actions when Par methods are called (i.e. without using a lazy
-- trace data structure).

module Examples.ParMonad.Direct (
   Sched(..),
   Par, -- abstract: Constructor not exported.
   IVar(..), IVarContents(..),
--    sched,
    runPar, runParIO,
    new, get, put_, fork,
    newFull, newFull_, put,
    spawn, spawn_, spawnP,
    spawn1_
--   runParAsync, runParAsyncHelper,
--   yield,
 ) where

import qualified Data.Vector                                   as V

import           Control.Applicative
import           Control.Concurrent.Classy                     hiding (fork,
                                                                spawn)
import           "mtl" Control.Monad.Cont                      as C
import           Control.Monad.IO.Class                        (MonadIO(..))
import qualified "mtl" Control.Monad.Reader                    as RD
import           Control.Monad.Trans                           (MonadTrans(lift))
import           Examples.ParMonad.DirectInternal              (HotVar, Par(..),
                                                                Sched(..),
                                                                Session(Session),
                                                                SessionID,
                                                                modifyHotVar,
                                                                modifyHotVar_,
                                                                newHotVar,
                                                                readHotVar,
                                                                writeHotVarRaw)
import           System.IO.Unsafe                              (unsafePerformIO)
import qualified System.Random.MWC                             as Random

import           Control.DeepSeq
import           Control.Monad                                 (join,
                                                                replicateM,
                                                                unless)
import           Data.Foldable                                 (forM_)
import qualified Data.Set                                      as S
import           Data.Word                                     (Word64)

import           Data.Concurrent.Deque.Reference               as R
import           Data.Concurrent.Deque.Reference.DequeInstance ()


import qualified Control.Exception                             as E (AsyncException(..))
import qualified Control.Monad.Catch                           as E

import           Prelude                                       hiding (null)
import qualified Prelude

_PARPUTS :: Bool
_PARPUTS = True

_FORKPARENT :: Bool
_FORKPARENT = True

_IDLING_ON :: Bool
_IDLING_ON = True

--------------------------------------------------------------------------------
-- Core type definitions
--------------------------------------------------------------------------------

type ROnly m = RD.ReaderT (Sched m) m

newtype IVar m a = IVar (IORef m (IVarContents m a))

data IVarContents m a = Full a | Empty | Blocked [a -> m ()]

unsafeParIO :: MonadConc m => m a -> Par m a
unsafeParIO iom = Par (lift$ lift iom)

io :: MonadConc m => m a -> Par m a
io = unsafeParIO -- shorthand used below

--------------------------------------------------------------------------------
-- Global State
--------------------------------------------------------------------------------

-- This keeps track of ALL worker threads across all unreated
-- `runPar` instantiations.  This is used to detect nested invocations
-- of `runPar` and avoid reinitialization.
-- globalWorkerPool :: IORef (Data.IntMap ())
-- TODO! Make this semi-local! (not shared between "top-level" runPars)

{-# INLINE amINested #-}
{-# INLINE registerWorker #-}
{-# INLINE unregisterWorker #-}
amINested :: Monad m => ThreadId m -> m (Maybe (Sched m))
registerWorker :: Monad m => ThreadId m -> Sched m -> m ()
unregisterWorker :: Monad m => ThreadId m -> m ()

amINested      _      = pure Nothing
registerWorker _ _    = pure ()
unregisterWorker _tid = pure ()


-----------------------------------------------------------------------------
-- Helpers #2:  Pushing and popping work.
-----------------------------------------------------------------------------

{-# INLINE popWork  #-}
popWork :: (MonadConc m, MonadIO m) => Sched m -> m (Maybe (Par m ()))
popWork Sched{ workpool } = liftIO $ R.tryPopL workpool

{-# INLINE pushWork #-}
pushWork :: (MonadConc m, MonadIO m) => Sched m -> Par m () -> m ()
pushWork Sched { workpool, idle } task = do
  liftIO $ R.pushL workpool task
  --when isMain$    -- Experimenting with reducing contention by doing this only from a single thread.
                    -- TODO: We need to have a proper binary wakeup-tree.
  tryWakeIdle idle

tryWakeIdle :: MonadConc m => HotVar m [MVar m Bool] -> m ()
tryWakeIdle idle = do
-- NOTE: I worry about having the idle var hammmered by all threads on their spawn-path:
  -- If any worker is idle, wake one up and give it work to do.
  idles <- readHotVar idle -- Optimistically do a normal read first.
  unless (Prelude.null idles) . join $
    modifyHotVar idle (\case
                          []      -> ([], pure ())
                          (i:ils) -> (ils, putMVar i False))

rand :: (MonadConc m, MonadIO m) => HotVar m Random.GenIO -> m Int
rand ref = do
  numCapabilities <- liftIO getNumCapabilities
  liftIO . Random.uniformR (0, numCapabilities-1) =<< readHotVar ref

--------------------------------------------------------------------------------
-- Running computations in the Par monad
--------------------------------------------------------------------------------

instance NFData (IVar m a) where
  rnf _ = ()

{-# NOINLINE runPar #-}
runPar = unsafePerformIO . runParIO


-- | This procedure creates a new worker on the current thread (with a
--   new session ID) and plugs it into the work-stealing environment.
--   This new worker extracts itself from the work stealing pool when
--   `userComp` has completed, thus freeing the current thread (this
--   procedure) to return normally.
runNewSessionAndWait :: (MonadConc m, MonadIO m) => String -> Sched m -> Par m b -> m b
runNewSessionAndWait name sched userComp = do
    tid <- myThreadId -- TODO: remove when done debugging
    sid <- modifyHotVar (sessionCounter sched) (\ x -> (x+1,x))
    _ <- modifyHotVar (activeSessions sched) (\ set -> (S.insert sid set, ()))

    -- Here we have an extra IORef... ugly.
    ref <- newIORef (error$ "Empty session-result ref ("++name++") should never be touched (sid "++ show sid++", "++show tid ++")")
    newFlag <- newHotVar False
    -- Push the new session:
    _ <- modifyHotVar (sessions sched) (\ ls -> (Session sid newFlag : ls, ()))

    let userComp' = do ans <- userComp
                       -- This add-on to userComp will run only after userComp has completed successfully,
                       -- but that does NOT guarantee that userComp-forked computations have terminated:
                       io$ do writeIORef ref ans
                              writeHotVarRaw newFlag True
                              modifyHotVar (activeSessions sched) (\ set -> (S.delete sid set, ()))
        kont n = trivialCont$ "("++name++", sid "++show sid++", round "++show n++")"
        loop n = do flg <- readIORef newFlag
                    unless flg $ do
                      rescheduleR 0 $ trivialCont$ "("++name++", sid "++show sid++")"
                      loop (n+1)

    -- THIS IS RETURNING TOO EARLY!!:
    runReaderWith sched (C.runContT (unPar userComp') (kont (0::Int)))  -- Does this ASSUME child stealing?
    runReaderWith sched (loop (1::Int))

    -- TODO: Ideally we would wait for ALL outstanding (stolen) work on this "team" to complete.

    -- Here we pop off the frame we added to the session stack:
    modifyHotVar_ (sessions sched) $ \ (Session sid2 _ : tl) ->
        if sid == sid2
        then tl
        else error$ "Tried to pop the session stack and found we ("++show sid
                   ++") were not on the top! (instead "++show sid2++")"

    -- By returning here we ARE implicitly reengaging the scheduler, since we
    -- are already inside the rescheduleR loop on this thread
    -- (before runParIO was called in a nested fashion).
    readIORef ref


{-# NOINLINE runParIO #-}
runParIO userComp = do
   tid <- myThreadId

    --
    -- Lacking threadCapability, we always pick CPU #0 to run the main
    -- thread.  If the current thread is not running on CPU #0, this
    -- will require some data to be shipped over the memory bus, and
    -- hence will be slightly slower than the version above.
    --
   let main_cpu = 0

   maybSched <- amINested tid
   tidorig <- myThreadId -- TODO: remove when done debugging
   case maybSched of
     Just sched -> do
       -- Here the current thread is ALREADY a worker.  All we need to
       -- do is plug the users new computation in.

       _ <- readHotVar (sessionCounter sched)
       runNewSessionAndWait "nested runPar" sched userComp

     ------------------------------------------------------------
     -- Non-nested case, make a new set of worker threads:
     ------------------------------------------------------------
     Nothing -> do
       allscheds <- makeScheds main_cpu
       sessions <- readHotVar$ sessions$ (\(a:_) -> a) allscheds
       case sessions of
         [Session _ topSessFlag] -> do
            mfin <- newEmptyMVar
            forM_ (zip [0..] allscheds) $ \(cpu,sched) -> do
                  workerDone <- newEmptyMVar
                  ----------------------------------------
                  let wname = "(worker "++show cpu++" of originator "++show tidorig++")"
      --            forkOn cpu $ do
                  _ <- forkWithExceptions (forkOn cpu) wname $ do
                  ------------------------------------------------------------STRT WORKER THREAD
                    tid2 <- myThreadId
                    registerWorker tid2 sched
                    if cpu /= main_cpu
                      then do runReaderWith sched $ rescheduleR 0 (trivialCont (wname++show tid2))
                              putMVar workerDone cpu
                      else do x <- runNewSessionAndWait "top-lvl main worker" sched userComp
                              -- When the main worker finishes we can tell the anonymous "system" workers:
                              writeIORef topSessFlag True
                              putMVar mfin x

                    unregisterWorker tid
                  ------------------------------------------------------------END WORKER THREAD
                  pure (if cpu == main_cpu then Nothing else Just workerDone)

            takeMVar mfin -- Final value.

         _ -> error "sessions"

       ----------------------------------------

-- Create the default scheduler(s) state:
makeScheds :: (MonadConc m, MonadIO m) => Int -> m [Sched m]
makeScheds main = do
   numCapabilities <- getNumCapabilities
   workpools <- replicateM numCapabilities $ liftIO R.newQ
   rngs      <- mapM (\i -> liftIO (Random.initialize (V.singleton $ fromIntegral i)) >>= newHotVar) [0..numCapabilities]
   idle      <- newHotVar []
   -- The STACKs are per-worker.. but the root finished flag is shared between all anonymous system workers:
   sessionFinished <- newHotVar False
   sessionStacks   <- replicateM numCapabilities (newHotVar [Session baseSessionID sessionFinished])
   activeSessions  <- newHotVar S.empty
   sessionCounter  <- newHotVar (baseSessionID + 1)
   let allscheds = [ Sched { no=x, idle, isMain= x==main,
                             workpool=wp, scheds=allscheds, rng=rng,
                             sessions = stck,
                             activeSessions=activeSessions,
                             sessionCounter=sessionCounter
                           }
                   | x   <- [0 .. numCapabilities-1]
                   | wp  <- workpools
                   | rng <- rngs
                   | stck <- sessionStacks
                   ]
   pure allscheds

-- The ID of top-level runPar sessions.
baseSessionID :: SessionID
baseSessionID = 1000


--------------------------------------------------------------------------------
-- IVar operations
--------------------------------------------------------------------------------

{-# INLINE new  #-}
-- | Creates a new @IVar@
new :: MonadConc m => Par m (IVar m a)
new  = io$ IVar <$> newIORef Empty

{-# INLINE get  #-}
-- | Read the value in an @IVar@.  The 'get' operation can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @IVar@.
get (IVar vr) =
  callCC $ \kont ->
    do
       e  <- io$ readIORef vr
       case e of
          Full a -> pure a
          _ -> do
            sch <- RD.ask

            let resched = longjmpSched -- Invariant: kont must not be lost.
            -- Because we continue on the same processor the Sched stays the same:
            -- TODO: Try NOT using monadic values as first class.  Check for performance effect:
            join . io$ atomicModifyIORef vr $ \case
                      Empty      -> (Blocked [pushWork sch . kont], resched)
                      Full a     -> (Full a, pure a) -- kont is implicit here.
                      Blocked ks -> (Blocked (pushWork sch . kont:ks), resched)

------------------------------------------------------------
{-# INLINE put_ #-}
-- | @put_@ is a version of @put@ that is head-strict rather than fully-strict.
--   In this scheduler, puts immediately execute woken work in the current thread.
put_ (IVar vr) !content = do
   sched <- RD.ask
   ks <- io$ atomicModifyIORef vr $ \case
               Empty      -> (Full content, [])
               Full _     -> error "multiple put"
               Blocked ks -> (Full content, ks)
   wakeUp sched ks content

-- | When an IVar is filled in, continuations wake up.
{-# INLINE wakeUp #-}
wakeUp :: (MonadConc m, MonadIO m) => Sched m -> [a -> m ()]-> a -> Par m ()
wakeUp _sched ks arg = loop ks
 where
   loop [] = pure ()
   loop (kont:rest) = do
     -- FIXME -- without strict firewalls keeping ivars from moving
     -- between runPar sessions, if we allow nested scheduler use
     -- we could potentially wake up work belonging to a different
     -- runPar and thus bring it into our worker and delay our own
     -- continuation until its completion.
     if _PARPUTS then
       -- We do NOT force the putting thread to postpone its continuation.
       do _ <- spawn_$ pMap kont rest
          pure ()
       -- case rest of
       --   [] -> spawn_$ io$ kont arg
       --   _  -> spawn_$ do spawn_$ io$ kont arg
       --                    io$ parchain rest
       -- error$"FINISHME - wake "++show (length ks)++" conts"
      else
       -- This version sacrifices a parallelism opportunity and
       -- imposes additional serialization.
       --
       -- [2012.08.31] WARNING -- this serialzation CAN cause deadlock.
       -- This "optimization" should not be on the table.
       -- mapM_ ($arg) ks
       do io$ kont arg
          loop rest
     pure ()

   pMap kont [] = io$ kont arg
   pMap kont (more:rest) =
     do _ <- spawn_$ io$ kont arg
        pMap more rest

   -- parchain [kont] = kont arg
   -- parchain (kont:rest) = do spawn$ io$ kont arg
   --                           parchain rest


------------------------------------------------------------
{-# INLINE fork #-}
fork :: (MonadConc m, MonadIO m) => Par m () -> Par m ()
fork task =
  -- Forking the "parent" means offering up the continuation of the
  -- fork rather than the task argument for stealing:
  if _FORKPARENT
    then do
      sched <- RD.ask
      callCC$ \parent -> do
         let wrapped = parent ()
         io$ pushWork sched wrapped
         -- Then execute the child task and return to the scheduler when it is complete:
         task
         -- If we get to this point we have finished the child task:
         longjmpSched -- We reschedule to pop the cont we pushed.
         -- TODO... OPTIMIZATION: we could also try the pop directly, and if it succeeds return normally....

    else do
      sch <- RD.ask
      io$ pushWork sch task

-- This routine "longjmp"s to the scheduler, throwing out its own continuation.
longjmpSched :: (MonadConc m, MonadIO m) => Par m a
-- longjmpSched = Par $ C.ContT rescheduleR
longjmpSched = Par $ C.ContT (\ _k -> rescheduleR 0 (trivialCont "longjmpSched"))

-- Reschedule the scheduler loop until it observes sessionFinished==True, and
-- then it finally invokes its continuation.
rescheduleR :: (MonadConc m, MonadIO m) => Word64 -> (a -> ROnly m ()) -> ROnly m ()
rescheduleR cnt kont = do
  mysched <- RD.ask
  mtask  <- lift $ popWork mysched
  case mtask of
    Nothing -> do
                  sessions <- lift $ readIORef $ sessions mysched
                  case sessions of
                    Session _ finRef:_ -> do
                      fin <- lift $ readIORef finRef
                      if fin
                      then kont (error "Direct.hs: The result value from rescheduleR should not be used.")
                      else do
                        lift $ steal mysched
                        lift   yield
                        rescheduleR (cnt+1) kont
                    _ -> error "sessions"
    Just task -> do
       let C.ContT fn = unPar task
       -- Run the stolen task with a continuation that returns to the scheduler if the task exits normally:
       fn (\ _ -> rescheduleR 0 kont)


-- | Attempt to steal work or, failing that, give up and go idle.
--
--   The current policy is to do a burst of of N tries without
--   yielding or pausing inbetween.
steal :: (MonadConc m, MonadIO m) => Sched m -> m ()
steal mysched@Sched{ idle, scheds, rng, no=my_no } = do
  i <- getnext (-1 :: Int)
  numCapabilities <- getNumCapabilities
  go (maxtries numCapabilities) i
 where
--    maxtries = numCapabilities -- How many times should we attempt theft before going idle?
    maxtries numCapabilities = 20 * numCapabilities -- How many times should we attempt theft before going idle?

    getnext _ = rand rng

    ----------------------------------------
    -- IDLING behavior:
    go 0 _ | _IDLING_ON =
            do m <- newEmptyMVar
               r <- modifyHotVar idle $ \is -> (m:is, is)
               numCapabilities <- getNumCapabilities
               if length r == numCapabilities - 1
                  then
                     mapM_ (`putMVar` True) r
                  else do
                    done <- takeMVar m
                    if done
                       then
                         pure ()
                       else do
                         i <- getnext (-1::Int)
                         go (maxtries numCapabilities) i

    -- We need to return from this loop to check sessionFinished and exit the scheduler if necessary.
    go 0 _i | not _IDLING_ON = yield

    ----------------------------------------
    go tries i
      | i == my_no = do i' <- getnext i
                        go (tries-1) i'

      | otherwise     = do
         -- We ONLY go through the global sched array to access victims:
         let schd = scheds!!i

--         let dq = workpool schd :: WSDeque (Par ())
         let dq = workpool schd
         r <- liftIO $ R.tryPopR dq

         case r of
           Just task  ->
              runReaderWith mysched $
                C.runContT (unPar task)
                 (\_ -> pure ())

           Nothing -> do i' <- getnext i
                         go (tries-1) i'

trivialCont :: Monad m => String -> a -> ROnly m ()
trivialCont _ _ = pure ()


--------------------------------------------------------------------------------
-- <boilerplate>

-- TEMP: TODO: Factor out this boilerplate somehow.

{-# INLINE spawn1_ #-}
-- Spawn a one argument function instead of a thunk.  This is good for debugging if the value supports "Show".
spawn1_ f x =
    spawn_ (f x)

-- The following is usually inefficient!
newFull_ a = do v <- new
                put_ v a
                pure v

newFull a = deepseq a (newFull_ a)

{-# INLINE put  #-}
put v a = deepseq a (put_ v a)

spawn p  = do r <- new;  fork (p >>= put r);   pure r
spawn_ p = do r <- new;  fork (p >>= put_ r);  pure r
spawnP a = spawn (pure a)

spawn  :: (MonadConc m, MonadIO m, NFData a) => Par m a -> Par m (IVar m a)
spawn_ :: (MonadConc m, MonadIO m) => Par m a -> Par m (IVar m a)
spawn1_ :: (MonadConc m, MonadIO m) => (a -> Par m b) -> a -> Par m (IVar m b)
spawnP :: (MonadConc m, MonadIO m, NFData a) => a -> Par m (IVar m a)
put_   :: (MonadConc m, MonadIO m) => IVar m a -> a -> Par m ()
put    :: (MonadConc m, MonadIO m, NFData a) => IVar m a -> a -> Par m ()
get    :: (MonadConc m, MonadIO m) => IVar m a -> Par m a
runPar :: Par IO a -> a
runParIO :: (MonadConc m, MonadIO m) => Par m a -> m a
newFull :: (MonadConc m, MonadIO m, NFData a) => a -> Par m (IVar m a)
newFull_ ::  (MonadConc m, MonadIO m) => a -> Par m (IVar m a)

-- </boilerplate>
--------------------------------------------------------------------------------


{-# INLINE runReaderWith #-}
-- | Arguments flipped for convenience.
runReaderWith :: r -> RD.ReaderT r m a -> m a
runReaderWith state m = RD.runReaderT m state


--------------------------------------------------------------------------------
-- DEBUGGING TOOLs
--------------------------------------------------------------------------------


-- | Exceptions that walk up the fork tree of threads:
forkWithExceptions :: MonadConc m => (m () -> m (ThreadId m)) -> String -> m () -> m (ThreadId m)
forkWithExceptions forkit _ action = do
   parent <- myThreadId
   forkit $
      E.catch action
         (\ e ->
           case E.fromException e of
             Just E.ThreadKilled -> pure ()
             _  -> throwTo parent (e :: E.SomeException)
         )

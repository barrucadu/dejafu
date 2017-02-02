{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : RankNTypes, ScopedTypeVariables
--
-- Concurrent monads with a fixed scheduler: internal types and
-- functions. This module is NOT considered to form part of the public
-- interface of this library.
module Test.DejaFu.Conc.Internal where

import Control.Exception (MaskingState(..), toException)
import Control.Monad.Ref (MonadRef, newRef, readRef, writeRef)
import Data.Functor (void)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)

import Test.DejaFu.Common
import Test.DejaFu.Conc.Internal.Common
import Test.DejaFu.Conc.Internal.Memory
import Test.DejaFu.Conc.Internal.Threading
import Test.DejaFu.Schedule
import Test.DejaFu.STM (Result(..), runTransaction)

{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}
{-# ANN module ("HLint: ignore Use const"           :: String) #-}

--------------------------------------------------------------------------------
-- * Execution

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning a failure reason on error. Also returned is the
-- final state of the scheduler, and an execution trace (in reverse
-- order).
runConcurrency :: MonadRef r n
               => Scheduler g
               -> MemType
               -> g
               -> M n r a
               -> n (Either Failure a, g, Trace)
runConcurrency sched memtype g ma = do
  ref <- newRef Nothing

  let c = runCont ma (AStop . writeRef ref . Just . Right)
  let threads = launch' Unmasked initialThread (const c) M.empty
  let ctx = Context { cSchedState = g, cIdSource = initialIdSource, cThreads = threads, cWriteBuf = emptyBuffer, cCaps = 2 }

  (finalCtx, trace) <- runThreads sched memtype ref ctx
  out <- readRef ref
  pure (fromJust out, cSchedState finalCtx, trace)

-- | The context a collection of threads are running in.
data Context n r g = Context
  { cSchedState :: g
  , cIdSource   :: IdSource
  , cThreads    :: Threads n r
  , cWriteBuf   :: WriteBuffer r
  , cCaps       :: Int
  }

-- | Run a collection of threads, until there are no threads left.
--
-- Note: this returns the trace in reverse order, because it's more
-- efficient to prepend to a list than append. As this function isn't
-- exposed to users of the library, this is just an internal gotcha to
-- watch out for.
runThreads :: MonadRef r n
           => Scheduler g -> MemType -> r (Maybe (Either Failure a)) -> Context n r g -> n (Context n r g, Trace)
runThreads sched memtype ref = go [] Nothing where
  go sofar prior ctx
    | isTerminated  = stop ctx
    | isDeadlocked  = die Deadlock ctx
    | isSTMLocked   = die STMDeadlock ctx
    | isAborted     = die Abort $ ctx { cSchedState = g' }
    | isNonexistant = die InternalError $ ctx { cSchedState = g' }
    | isBlocked     = die InternalError $ ctx { cSchedState = g' }
    | otherwise = do
      stepped <- stepThread sched memtype chosen (_continuation $ fromJust thread) $ ctx { cSchedState = g' }
      case stepped of
        Right (ctx', actOrTrc) -> loop actOrTrc ctx'
        Left UncaughtException
          | chosen == initialThread -> die UncaughtException $ ctx { cSchedState = g' }
          | otherwise -> loop (Right Killed) $ ctx { cThreads = kill chosen threadsc, cSchedState = g' }
        Left failure -> die failure $ ctx { cSchedState = g' }

    where
      (choice, g')  = sched (map (\(d,_,a) -> (d,a)) $ reverse sofar) ((\p (_,_,a) -> (p,a)) <$> prior <*> listToMaybe sofar) (fromList $ map (\(t,l:|_) -> (t,l)) runnable') (cSchedState ctx)
      chosen        = fromJust choice
      runnable'     = [(t, nextActions t) | t <- sort $ M.keys runnable]
      runnable      = M.filter (isNothing . _blocking) threadsc
      thread        = M.lookup chosen threadsc
      threadsc      = addCommitThreads (cWriteBuf ctx) (cThreads ctx)
      isAborted     = isNothing choice
      isBlocked     = isJust . _blocking $ fromJust thread
      isNonexistant = isNothing thread
      isTerminated  = initialThread `notElem` M.keys (cThreads ctx)
      isDeadlocked  = M.null (M.filter (isNothing . _blocking) (cThreads ctx)) &&
        (((~=  OnMVarFull  undefined) <$> M.lookup initialThread (cThreads ctx)) == Just True ||
         ((~=  OnMVarEmpty undefined) <$> M.lookup initialThread (cThreads ctx)) == Just True ||
         ((~=  OnMask      undefined) <$> M.lookup initialThread (cThreads ctx)) == Just True)
      isSTMLocked = M.null (M.filter (isNothing . _blocking) (cThreads ctx)) &&
        ((~=  OnTVar []) <$> M.lookup initialThread (cThreads ctx)) == Just True

      unblockWaitingOn tid = fmap unblock where
        unblock thrd = case _blocking thrd of
          Just (OnMask t) | t == tid -> thrd { _blocking = Nothing }
          _ -> thrd

      decision
        | Just chosen == prior = Continue
        | prior `notElem` map (Just . fst) runnable' = Start chosen
        | otherwise = SwitchTo chosen

      nextActions t = lookahead . _continuation . fromJust $ M.lookup t threadsc

      stop finalCtx = pure (finalCtx, sofar)
      die reason finalCtx = writeRef ref (Just $ Left reason) >> stop finalCtx

      loop trcOrAct ctx' =
        let trc = case trcOrAct of
              Left (act, acts) -> acts ++ [(decision, runnable', act)]
              Right act -> [(decision, runnable', act)]
            sofar' =  trc++sofar
            threads' = if (interruptible <$> M.lookup chosen (cThreads ctx')) /= Just False
                       then unblockWaitingOn chosen (cThreads ctx')
                       else cThreads ctx'
        in go sofar' (Just chosen) $ ctx' { cThreads = delCommitThreads threads' }

--------------------------------------------------------------------------------
-- * Single-step execution

-- | Run a single thread one step, by dispatching on the type of
-- 'Action'.
stepThread :: forall n r g. MonadRef r n
  => Scheduler g
  -- ^ The scheduler.
  -> MemType
  -- ^ The memory model to use.
  -> ThreadId
  -- ^ ID of the current thread
  -> Action n r
  -- ^ Action to step
  -> Context n r g
  -- ^ The execution context.
  -> n (Either Failure (Context n r g, Either (ThreadAction, Trace) ThreadAction))
stepThread sched memtype tid action ctx = case action of
  AFork n a b -> stepFork n a b
  AMyTId c -> stepMyTId c
  AGetNumCapabilities c -> stepGetNumCapabilities c
  ASetNumCapabilities i c -> stepSetNumCapabilities i c
  AYield c -> stepYield c
  ANewVar n c -> stepNewVar n c
  APutVar var a c -> stepPutVar var a c
  ATryPutVar var a c -> stepTryPutVar var a c
  AReadVar var c -> stepReadVar var c
  ATakeVar var c -> stepTakeVar var c
  ATryTakeVar var c -> stepTryTakeVar var c
  ANewRef n a c -> stepNewRef n a c
  AReadRef ref c -> stepReadRef ref c
  AReadRefCas ref c -> stepReadRefCas ref c
  AModRef ref f c -> stepModRef ref f c
  AModRefCas ref f c -> stepModRefCas ref f c
  AWriteRef ref a c -> stepWriteRef ref a c
  ACasRef ref tick a c -> stepCasRef ref tick a c
  ACommit t c -> stepCommit t c
  AAtom stm c -> stepAtom stm c
  ALift na -> stepLift na
  AThrow e -> stepThrow e
  AThrowTo t e c -> stepThrowTo t e c
  ACatching h ma c -> stepCatching h ma c
  APopCatching a -> stepPopCatching a
  AMasking m ma c -> stepMasking m ma c
  AResetMask b1 b2 m c -> stepResetMask b1 b2 m c
  AReturn c -> stepReturn c
  AMessage m c  -> stepMessage m c
  AStop na -> stepStop na
  ASub ma k -> stepSubconcurrency ma k

  where
    -- | Start a new thread, assigning it the next 'ThreadId'
    --
    -- Explicit type signature needed for GHC 8. Looks like the
    -- impredicative polymorphism checks got stronger.
    stepFork :: String
             -> ((forall b. M n r b -> M n r b) -> Action n r)
             -> (ThreadId -> Action n r)
             -> n (Either x (Context n r g, Either z ThreadAction))
    stepFork n a b = return $ Right (ctx { cThreads = goto (b newtid) tid threads', cIdSource = idSource' }, Right (Fork newtid)) where
      threads' = launch tid newtid a (cThreads ctx)
      (idSource', newtid) = nextTId n (cIdSource ctx)

    -- | Get the 'ThreadId' of the current thread
    stepMyTId c = simple (goto (c tid) tid (cThreads ctx)) MyThreadId

    -- | Get the number of capabilities
    stepGetNumCapabilities c = simple (goto (c (cCaps ctx)) tid (cThreads ctx)) $ GetNumCapabilities (cCaps ctx)

    -- | Set the number of capabilities
    stepSetNumCapabilities i c = return $ Right (ctx { cThreads = goto c tid (cThreads ctx), cCaps = i }, Right (SetNumCapabilities i))

    -- | Yield the current thread
    stepYield c = simple (goto c tid (cThreads ctx)) Yield

    -- | Put a value into a @MVar@, blocking the thread until it's
    -- empty.
    stepPutVar cvar@(MVar cvid _) a c = synchronised $ do
      (success, threads', woken) <- putIntoMVar cvar a c tid (cThreads ctx)
      simple threads' $ if success then PutVar cvid woken else BlockedPutVar cvid

    -- | Try to put a value into a @MVar@, without blocking.
    stepTryPutVar cvar@(MVar cvid _) a c = synchronised $ do
      (success, threads', woken) <- tryPutIntoMVar cvar a c tid (cThreads ctx)
      simple threads' $ TryPutVar cvid success woken

    -- | Get the value from a @MVar@, without emptying, blocking the
    -- thread until it's full.
    stepReadVar cvar@(MVar cvid _) c = synchronised $ do
      (success, threads', _) <- readFromMVar cvar c tid (cThreads ctx)
      simple threads' $ if success then ReadVar cvid else BlockedReadVar cvid

    -- | Take the value from a @MVar@, blocking the thread until it's
    -- full.
    stepTakeVar cvar@(MVar cvid _) c = synchronised $ do
      (success, threads', woken) <- takeFromMVar cvar c tid (cThreads ctx)
      simple threads' $ if success then TakeVar cvid woken else BlockedTakeVar cvid

    -- | Try to take the value from a @MVar@, without blocking.
    stepTryTakeVar cvar@(MVar cvid _) c = synchronised $ do
      (success, threads', woken) <- tryTakeFromMVar cvar c tid (cThreads ctx)
      simple threads' $ TryTakeVar cvid success woken

    -- | Read from a @CRef@.
    stepReadRef cref@(CRef crid _) c = do
      val <- readCRef cref tid
      simple (goto (c val) tid (cThreads ctx)) $ ReadRef crid

    -- | Read from a @CRef@ for future compare-and-swap operations.
    stepReadRefCas cref@(CRef crid _) c = do
      tick <- readForTicket cref tid
      simple (goto (c tick) tid (cThreads ctx)) $ ReadRefCas crid

    -- | Modify a @CRef@.
    stepModRef cref@(CRef crid _) f c = synchronised $ do
      (new, val) <- f <$> readCRef cref tid
      writeImmediate cref new
      simple (goto (c val) tid (cThreads ctx)) $ ModRef crid

    -- | Modify a @CRef@ using a compare-and-swap.
    stepModRefCas cref@(CRef crid _) f c = synchronised $ do
      tick@(Ticket _ _ old) <- readForTicket cref tid
      let (new, val) = f old
      void $ casCRef cref tid tick new
      simple (goto (c val) tid (cThreads ctx)) $ ModRefCas crid

    -- | Write to a @CRef@ without synchronising
    stepWriteRef cref@(CRef crid _) a c = case memtype of
      -- Write immediately.
      SequentialConsistency -> do
        writeImmediate cref a
        simple (goto c tid (cThreads ctx)) $ WriteRef crid

      -- Add to buffer using thread id.
      TotalStoreOrder -> do
        wb' <- bufferWrite (cWriteBuf ctx) (tid, Nothing) cref a
        return $ Right (ctx { cThreads = goto c tid (cThreads ctx), cWriteBuf = wb' }, Right (WriteRef crid))

      -- Add to buffer using both thread id and cref id
      PartialStoreOrder -> do
        wb' <- bufferWrite (cWriteBuf ctx) (tid, Just crid) cref a
        return $ Right (ctx { cThreads = goto c tid (cThreads ctx), cWriteBuf = wb' }, Right (WriteRef crid))

    -- | Perform a compare-and-swap on a @CRef@.
    stepCasRef cref@(CRef crid _) tick a c = synchronised $ do
      (suc, tick') <- casCRef cref tid tick a
      simple (goto (c (suc, tick')) tid (cThreads ctx)) $ CasRef crid suc

    -- | Commit a @CRef@ write
    stepCommit t c = do
      wb' <- case memtype of
        -- Shouldn't ever get here
        SequentialConsistency ->
          error "Attempting to commit under SequentialConsistency"

        -- Commit using the thread id.
        TotalStoreOrder -> commitWrite (cWriteBuf ctx) (t, Nothing)

        -- Commit using the cref id.
        PartialStoreOrder -> commitWrite (cWriteBuf ctx) (t, Just c)

      return $ Right (ctx { cWriteBuf = wb' }, Right (CommitRef t c))

    -- | Run a STM transaction atomically.
    stepAtom stm c = synchronised $ do
      (res, idSource', trace) <- runTransaction stm (cIdSource ctx)
      case res of
        Success _ written val ->
          let (threads', woken) = wake (OnTVar written) (cThreads ctx)
          in return $ Right (ctx { cThreads = goto (c val) tid threads', cIdSource = idSource' }, Right (STM trace woken))
        Retry touched ->
          let threads' = block (OnTVar touched) tid (cThreads ctx)
          in return $ Right (ctx { cThreads = threads', cIdSource = idSource'}, Right (BlockedSTM trace))
        Exception e -> do
          res' <- stepThrow e
          return $ case res' of
            Right (ctx', _) -> Right (ctx' { cIdSource = idSource' }, Right Throw)
            Left err -> Left err

    -- | Run a subcomputation in an exception-catching context.
    stepCatching h ma c = simple threads' Catching where
      a     = runCont ma      (APopCatching . c)
      e exc = runCont (h exc) (APopCatching . c)

      threads' = goto a tid (catching e tid (cThreads ctx))

    -- | Pop the top exception handler from the thread's stack.
    stepPopCatching a = simple threads' PopCatching where
      threads' = goto a tid (uncatching tid (cThreads ctx))

    -- | Throw an exception, and propagate it to the appropriate
    -- handler.
    stepThrow e =
      case propagate (toException e) tid (cThreads ctx) of
        Just threads' -> simple threads' Throw
        Nothing -> return $ Left UncaughtException

    -- | Throw an exception to the target thread, and propagate it to
    -- the appropriate handler.
    stepThrowTo t e c = synchronised $
      let threads' = goto c tid (cThreads ctx)
          blocked  = block (OnMask t) tid (cThreads ctx)
      in case M.lookup t (cThreads ctx) of
           Just thread
             | interruptible thread -> case propagate (toException e) t threads' of
               Just threads'' -> simple threads'' $ ThrowTo t
               Nothing
                 | t == initialThread -> return $ Left UncaughtException
                 | otherwise -> simple (kill t threads') $ ThrowTo t
             | otherwise -> simple blocked $ BlockedThrowTo t
           Nothing -> simple threads' $ ThrowTo t

    -- | Execute a subcomputation with a new masking state, and give
    -- it a function to run a computation with the current masking
    -- state.
    --
    -- Explicit type sig necessary for checking in the prescence of
    -- 'umask', sadly.
    stepMasking :: MaskingState
                -> ((forall b. M n r b -> M n r b) -> M n r a)
                -> (a -> Action n r)
                -> n (Either x (Context n r g, Either z ThreadAction))
    stepMasking m ma c = simple threads' $ SetMasking False m where
      a = runCont (ma umask) (AResetMask False False m' . c)

      m' = _masking . fromJust $ M.lookup tid (cThreads ctx)
      umask mb = resetMask True m' >> mb >>= \b -> resetMask False m >> return b
      resetMask typ ms = cont $ \k -> AResetMask typ True ms $ k ()

      threads' = goto a tid (mask m tid (cThreads ctx))

    -- | Reset the masking thread of the state.
    stepResetMask b1 b2 m c = simple threads' act where
      act      = (if b1 then SetMasking else ResetMasking) b2 m
      threads' = goto c tid (mask m tid (cThreads ctx))

    -- | Create a new @MVar@, using the next 'MVarId'.
    stepNewVar n c = do
      let (idSource', newmvid) = nextMVId n (cIdSource ctx)
      ref <- newRef Nothing
      let mvar = MVar newmvid ref
      return $ Right (ctx { cThreads = goto (c mvar) tid (cThreads ctx), cIdSource = idSource' }, Right (NewVar newmvid))

    -- | Create a new @CRef@, using the next 'CRefId'.
    stepNewRef n a c = do
      let (idSource', newcrid) = nextCRId n (cIdSource ctx)
      ref <- newRef (M.empty, 0, a)
      let cref = CRef newcrid ref
      return $ Right (ctx { cThreads = goto (c cref) tid (cThreads ctx), cIdSource = idSource' }, Right (NewRef newcrid))

    -- | Lift an action from the underlying monad into the @Conc@
    -- computation.
    stepLift na = do
      a <- na
      simple (goto a tid (cThreads ctx)) LiftIO

    -- | Execute a 'return' or 'pure'.
    stepReturn c = simple (goto c tid (cThreads ctx)) Return

    -- | Add a message to the trace.
    stepMessage m c = simple (goto c tid (cThreads ctx)) (Message m)

    -- | Kill the current thread.
    stepStop na = na >> simple (kill tid (cThreads ctx)) Stop

    -- | Run a subconcurrent computation.
    stepSubconcurrency ma c
      | M.size (cThreads ctx) > 1 = return (Left IllegalSubconcurrency)
      | otherwise = do
          (res, g', trace) <- runConcurrency sched memtype (cSchedState ctx) ma
          return $ Right (ctx { cThreads = goto (c res) tid (cThreads ctx), cSchedState = g' }, Left (Subconcurrency, trace))

    -- | Helper for actions which don't touch the 'IdSource' or
    -- 'WriteBuffer'
    simple threads' act = return $ Right (ctx { cThreads = threads' }, Right act)

    -- | Helper for actions impose a write barrier.
    synchronised ma = do
      writeBarrier (cWriteBuf ctx)
      res <- ma

      return $ case res of
        Right (ctx', act) -> Right (ctx' { cWriteBuf = emptyBuffer }, act)
        _ -> res

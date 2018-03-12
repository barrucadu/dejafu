{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal
-- Copyright   : (c) 2016--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : MultiParamTypeClasses, RankNTypes, ScopedTypeVariables
--
-- Concurrent monads with a fixed scheduler: internal types and
-- functions. This module is NOT considered to form part of the public
-- interface of this library.
module Test.DejaFu.Conc.Internal where

import           Control.Exception                   (MaskingState(..),
                                                      toException)
import           Control.Monad.Conc.Class            (MonadConc,
                                                      rtsSupportsBoundThreads)
import           Control.Monad.Ref                   (MonadRef, newRef, readRef,
                                                      writeRef)
import           Data.Foldable                       (foldrM, toList)
import           Data.Functor                        (void)
import           Data.List                           (sortOn)
import qualified Data.Map.Strict                     as M
import           Data.Maybe                          (fromMaybe, isJust,
                                                      isNothing)
import           Data.Monoid                         ((<>))
import           Data.Sequence                       (Seq, (<|))
import qualified Data.Sequence                       as Seq

import           Test.DejaFu.Conc.Internal.Common
import           Test.DejaFu.Conc.Internal.Memory
import           Test.DejaFu.Conc.Internal.STM
import           Test.DejaFu.Conc.Internal.Threading
import           Test.DejaFu.Internal
import           Test.DejaFu.Schedule
import           Test.DejaFu.Types

--------------------------------------------------------------------------------
-- * Execution

-- | 'Trace' but as a sequence.
type SeqTrace
  = Seq (Decision, [(ThreadId, Lookahead)], ThreadAction)

-- | The result of running a concurrent program.
data CResult n r g a = CResult
  { finalContext :: Context n r g
  , finalRef :: r (Maybe (Either Failure a))
  , finalRestore :: Maybe (Threads n r -> n ())
  -- ^ Meaningless if this result doesn't come from a snapshotting
  -- execution.
  , finalTrace :: SeqTrace
  , finalDecision :: Maybe (ThreadId, ThreadAction)
  }

-- | A snapshot of the concurrency state immediately after 'dontCheck'
-- finishes.
--
-- @since 1.1.0.0
data DCSnapshot r n a = DCSnapshot
  { dcsContext :: Context n r ()
  -- ^ The execution context.  The scheduler state is ignored when
  -- restoring.
  , dcsRestore :: Threads n r -> n ()
  -- ^ Action to restore CRef, MVar, and TVar values.
  , dcsRef :: r (Maybe (Either Failure a))
  -- ^ Reference where the result will be written.
  }

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning a failure reason on error. Also returned is the
-- final state of the scheduler, and an execution trace.
runConcurrency :: (MonadConc n, MonadRef r n)
  => Bool
  -> Scheduler g
  -> MemType
  -> g
  -> IdSource
  -> Int
  -> M n r a
  -> n (CResult n r g a)
runConcurrency forSnapshot sched memtype g idsrc caps ma = do
  let ctx = Context { cSchedState = g
                    , cIdSource   = idsrc
                    , cThreads    = M.empty
                    , cWriteBuf   = emptyBuffer
                    , cCaps       = caps
                    }
  res <- runConcurrency' forSnapshot sched memtype ctx ma
  killAllThreads (finalContext res)
  pure res

-- | Like 'runConcurrency' but starts from a snapshot.
runConcurrencyWithSnapshot :: (MonadConc n, MonadRef r n)
  => Scheduler g
  -> MemType
  -> Context n r g
  -> (Threads n r -> n ())
  -> r (Maybe (Either Failure a))
  -> n (CResult n r g a)
runConcurrencyWithSnapshot sched memtype ctx restore ref = do
  let boundThreads = M.filter (isJust . _bound) (cThreads ctx)
  threads <- foldrM makeBound (cThreads ctx) (M.keys boundThreads)
  let ctx' = ctx { cThreads = threads }
  restore (cThreads ctx')
  res <- runConcurrency'' False sched memtype ref ctx { cThreads = threads}
  killAllThreads (finalContext res)
  pure res

-- | Kill the remaining threads
killAllThreads :: MonadConc n => Context n r g -> n ()
killAllThreads ctx =
  let finalThreads = cThreads ctx
  in mapM_ (`kill` finalThreads) (M.keys finalThreads)

-- | Run a concurrent program using the given context, and without
-- killing threads which remain at the end.  The context must have no
-- main thread.
runConcurrency' :: (MonadConc n, MonadRef r n)
  => Bool
  -> Scheduler g
  -> MemType
  -> Context n r g
  -> M n r a
  -> n (CResult n r g a)
runConcurrency' forSnapshot sched memtype ctx ma = do
  (c, ref) <- runRefCont AStop (Just . Right) (runM ma)
  let threads0 = launch' Unmasked initialThread (const c) (cThreads ctx)
  threads <- (if rtsSupportsBoundThreads then makeBound initialThread else pure) threads0
  runConcurrency'' forSnapshot sched memtype ref ctx { cThreads = threads}

-- | Like 'runConcurrency'' but doesn't do *ANY* set up at all.
runConcurrency'' :: (MonadConc n, MonadRef r n)
  => Bool
  -> Scheduler g
  -> MemType
  -> r (Maybe (Either Failure a))
  -> Context n r g
  -> n (CResult n r g a)
runConcurrency'' forSnapshot sched memtype ref ctx = do
  (finalCtx, trace, finalD, restore) <- runThreads forSnapshot sched memtype ref ctx
  pure CResult
    { finalContext = finalCtx
    , finalRef = ref
    , finalRestore = restore
    , finalTrace = trace
    , finalDecision = finalD
    }

-- | The context a collection of threads are running in.
data Context n r g = Context
  { cSchedState :: g
  , cIdSource   :: IdSource
  , cThreads    :: Threads n r
  , cWriteBuf   :: WriteBuffer r
  , cCaps       :: Int
  }

-- | Run a collection of threads, until there are no threads left.
runThreads :: (MonadConc n, MonadRef r n)
  => Bool
  -> Scheduler g
  -> MemType
  -> r (Maybe (Either Failure a))
  -> Context n r g
  -> n (Context n r g, SeqTrace, Maybe (ThreadId, ThreadAction), Maybe (Threads n r -> n ()))
runThreads forSnapshot sched memtype ref = go (const $ pure ()) Seq.empty Nothing where
  go restore sofar prior ctx
    | isTerminated  = stop restore sofar prior ctx
    | isDeadlocked  = die restore sofar prior Deadlock ctx
    | isSTMLocked   = die restore sofar prior STMDeadlock ctx
    | otherwise =
      let ctx' = ctx { cSchedState = g' }
      in case choice of
           Just chosen -> case M.lookup chosen threadsc of
             Just thread
               | isBlocked thread -> die restore sofar prior InternalError ctx'
               | otherwise -> step chosen thread ctx'
             Nothing -> die restore sofar prior InternalError ctx'
           Nothing -> die restore sofar prior Abort ctx'
    where
      (choice, g')  = scheduleThread sched prior (efromList "runThreads" runnable') (cSchedState ctx)
      runnable'     = [(t, lookahead (_continuation a)) | (t, a) <- sortOn fst $ M.assocs runnable]
      runnable      = M.filter (not . isBlocked) threadsc
      threadsc      = addCommitThreads (cWriteBuf ctx) threads
      threads       = cThreads ctx
      isBlocked     = isJust . _blocking
      isTerminated  = initialThread `notElem` M.keys threads
      isDeadlocked  = M.null (M.filter (not . isBlocked) threads) &&
        (((~=  OnMVarFull  undefined) <$> M.lookup initialThread threads) == Just True ||
         ((~=  OnMVarEmpty undefined) <$> M.lookup initialThread threads) == Just True ||
         ((~=  OnMask      undefined) <$> M.lookup initialThread threads) == Just True)
      isSTMLocked = M.null (M.filter (not . isBlocked) threads) &&
        ((~=  OnTVar []) <$> M.lookup initialThread threads) == Just True

      unblockWaitingOn tid = fmap unblock where
        unblock thrd = case _blocking thrd of
          Just (OnMask t) | t == tid -> thrd { _blocking = Nothing }
          _ -> thrd

      die restore' sofar' finalD reason finalCtx = do
        writeRef ref (Just $ Left reason)
        stop restore' sofar' finalD finalCtx

      stop restore' sofar' finalD finalCtx =
        pure (finalCtx, sofar', finalD, if forSnapshot then Just restore' else Nothing)

      step chosen thread ctx' = do
          (res, actOrTrc, actionSnap) <- stepThread
              forSnapshot
              (isNothing prior)
              sched
              memtype
              chosen
              (_continuation thread)
              ctx { cSchedState = g' }
          let trc    = getTrc actOrTrc
          let sofar' = sofar <> trc
          let prior' = getPrior actOrTrc
          let restore' threads' =
                if forSnapshot
                then restore threads' >> actionSnap threads'
                else restore threads'
          case res of
            Succeeded ctx'' ->
              let threads' = if (interruptible <$> M.lookup chosen (cThreads ctx'')) /= Just False
                             then unblockWaitingOn chosen (cThreads ctx'')
                             else cThreads ctx''
                  ctx''' = ctx'' { cThreads = delCommitThreads threads' }
              in go restore' sofar' prior' ctx'''
            Failed failure ->
              let ctx'' = ctx' { cThreads = delCommitThreads threads }
              in die restore' sofar' prior' failure ctx''
            Snap ctx'' ->
              stop actionSnap sofar' prior' ctx''
        where
          decision
            | Just chosen == (fst <$> prior) = Continue
            | (fst <$> prior) `notElem` map (Just . fst) runnable' = Start chosen
            | otherwise = SwitchTo chosen

          getTrc (Single a) = Seq.singleton (decision, alternatives, a)
          getTrc (SubC as _) = (decision, alternatives, Subconcurrency) <| as

          alternatives = filter (\(t, _) -> t /= chosen) runnable'

          getPrior (Single a) = Just (chosen, a)
          getPrior (SubC _ finalD) = finalD

--------------------------------------------------------------------------------
-- * Single-step execution

-- | What a thread did, for trace purposes.
data Act
  = Single ThreadAction
  -- ^ Just one action.
  | SubC SeqTrace (Maybe (ThreadId, ThreadAction))
  -- ^ @subconcurrency@, with the given trace and final action.
  deriving (Eq, Show)

-- | What a thread did, for execution purposes.
data What n r g
  = Succeeded (Context n r g)
  -- ^ Action succeeded: continue execution.
  | Failed Failure
  -- ^ Action caused computation to fail: stop.
  | Snap (Context n r g)
  -- ^ Action was a snapshot point and we're in snapshot mode: stop.

-- | Run a single thread one step, by dispatching on the type of
-- 'Action'.
--
-- Note: the returned snapshot action will definitely not do the right
-- thing with relaxed memory.
stepThread :: forall n r g. (MonadConc n, MonadRef r n)
  => Bool
  -- ^ Should we record a snapshot?
  -> Bool
  -- ^ Is this the first action?
  -> Scheduler g
  -- ^ The scheduler.
  -> MemType
  -- ^ The memory model to use.
  -> ThreadId
  -- ^ ID of the current thread
  -> Action n r
  -- ^ Action to step
  -> Context n r g
  -- ^ The execution context.
  -> n (What n r g, Act, Threads n r -> n ())
stepThread forSnapshot isFirst sched memtype tid action ctx = case action of
    -- start a new thread, assigning it the next 'ThreadId'
    AFork n a b -> pure $
      let threads' = launch tid newtid a (cThreads ctx)
          (idSource', newtid) = nextTId n (cIdSource ctx)
      in (Succeeded ctx { cThreads = goto (b newtid) tid threads', cIdSource = idSource' }, Single (Fork newtid), noSnap)

    -- start a new bound thread, assigning it the next 'ThreadId'
    AForkOS n a b -> do
      let (idSource', newtid) = nextTId n (cIdSource ctx)
      let threads' = launch tid newtid a (cThreads ctx)
      threads'' <- makeBound newtid threads'
      pure (Succeeded ctx { cThreads = goto (b newtid) tid threads'', cIdSource = idSource' }, Single (ForkOS newtid), noSnap)

    -- check if the current thread is bound
    AIsBound c ->
      let isBound = isJust . _bound $ elookup "stepThread.AIsBound" tid (cThreads ctx)
      in simple (goto (c isBound) tid (cThreads ctx)) (IsCurrentThreadBound isBound) noSnap

    -- get the 'ThreadId' of the current thread
    AMyTId c -> simple (goto (c tid) tid (cThreads ctx)) MyThreadId noSnap

    -- get the number of capabilities
    AGetNumCapabilities c -> simple (goto (c (cCaps ctx)) tid (cThreads ctx)) (GetNumCapabilities $ cCaps ctx) noSnap

    -- set the number of capabilities
    ASetNumCapabilities i c -> pure
      (Succeeded ctx { cThreads = goto c tid (cThreads ctx), cCaps = i }, Single (SetNumCapabilities i), noSnap)

    -- yield the current thread
    AYield c -> simple (goto c tid (cThreads ctx)) Yield noSnap

    -- yield the current thread (delay is ignored)
    ADelay n c -> simple (goto c tid (cThreads ctx)) (ThreadDelay n) noSnap

    -- create a new @MVar@, using the next 'MVarId'.
    ANewMVar n c -> do
      let (idSource', newmvid) = nextMVId n (cIdSource ctx)
      ref <- newRef Nothing
      let mvar = MVar newmvid ref
      pure ( Succeeded ctx { cThreads = goto (c mvar) tid (cThreads ctx), cIdSource = idSource' }
           , Single (NewMVar newmvid)
           , const (writeRef ref Nothing)
           )

    -- put a value into a @MVar@, blocking the thread until it's empty.
    APutMVar cvar@(MVar cvid _) a c -> synchronised $ do
      (success, threads', woken, effect) <- putIntoMVar cvar a c tid (cThreads ctx)
      simple threads' (if success then PutMVar cvid woken else BlockedPutMVar cvid) (const effect)

    -- try to put a value into a @MVar@, without blocking.
    ATryPutMVar cvar@(MVar cvid _) a c -> synchronised $ do
      (success, threads', woken, effect) <- tryPutIntoMVar cvar a c tid (cThreads ctx)
      simple threads' (TryPutMVar cvid success woken) (const effect)

    -- get the value from a @MVar@, without emptying, blocking the
    -- thread until it's full.
    AReadMVar cvar@(MVar cvid _) c -> synchronised $ do
      (success, threads', _, _) <- readFromMVar cvar c tid (cThreads ctx)
      simple threads' (if success then ReadMVar cvid else BlockedReadMVar cvid) noSnap

    -- try to get the value from a @MVar@, without emptying, without
    -- blocking.
    ATryReadMVar cvar@(MVar cvid _) c -> synchronised $ do
      (success, threads', _, _) <- tryReadFromMVar cvar c tid (cThreads ctx)
      simple threads' (TryReadMVar cvid success) noSnap

    -- take the value from a @MVar@, blocking the thread until it's
    -- full.
    ATakeMVar cvar@(MVar cvid _) c -> synchronised $ do
      (success, threads', woken, effect) <- takeFromMVar cvar c tid (cThreads ctx)
      simple threads' (if success then TakeMVar cvid woken else BlockedTakeMVar cvid) (const effect)

    -- try to take the value from a @MVar@, without blocking.
    ATryTakeMVar cvar@(MVar cvid _) c -> synchronised $ do
      (success, threads', woken, effect) <- tryTakeFromMVar cvar c tid (cThreads ctx)
      simple threads' (TryTakeMVar cvid success woken) (const effect)

    -- create a new @CRef@, using the next 'CRefId'.
    ANewCRef n a c -> do
      let (idSource', newcrid) = nextCRId n (cIdSource ctx)
      let val = (M.empty, 0, a)
      ref <- newRef val
      let cref = CRef newcrid ref
      pure ( Succeeded ctx { cThreads = goto (c cref) tid (cThreads ctx), cIdSource = idSource' }
           , Single (NewCRef newcrid)
           , const (writeRef ref val)
           )

    -- read from a @CRef@.
    AReadCRef cref@(CRef crid _) c -> do
      val <- readCRef cref tid
      simple (goto (c val) tid (cThreads ctx)) (ReadCRef crid) noSnap

    -- read from a @CRef@ for future compare-and-swap operations.
    AReadCRefCas cref@(CRef crid _) c -> do
      tick <- readForTicket cref tid
      simple (goto (c tick) tid (cThreads ctx)) (ReadCRefCas crid) noSnap

    -- modify a @CRef@.
    AModCRef cref@(CRef crid _) f c -> synchronised $ do
      (new, val) <- f <$> readCRef cref tid
      effect <- writeImmediate cref new
      simple (goto (c val) tid (cThreads ctx)) (ModCRef crid) (const effect)

    -- modify a @CRef@ using a compare-and-swap.
    AModCRefCas cref@(CRef crid _) f c -> synchronised $ do
      tick@(Ticket _ _ old) <- readForTicket cref tid
      let (new, val) = f old
      (_, _, effect) <- casCRef cref tid tick new
      simple (goto (c val) tid (cThreads ctx)) (ModCRefCas crid) (const effect)

    -- write to a @CRef@ without synchronising.
    AWriteCRef cref@(CRef crid _) a c -> case memtype of
      -- write immediately.
      SequentialConsistency -> do
        effect <- writeImmediate cref a
        simple (goto c tid (cThreads ctx)) (WriteCRef crid) (const effect)
      -- add to buffer using thread id.
      TotalStoreOrder -> do
        wb' <- bufferWrite (cWriteBuf ctx) (tid, Nothing) cref a
        pure (Succeeded ctx { cThreads = goto c tid (cThreads ctx), cWriteBuf = wb' }, Single (WriteCRef crid), noSnap)
      -- add to buffer using both thread id and cref id
      PartialStoreOrder -> do
        wb' <- bufferWrite (cWriteBuf ctx) (tid, Just crid) cref a
        pure (Succeeded ctx { cThreads = goto c tid (cThreads ctx), cWriteBuf = wb' }, Single (WriteCRef crid), noSnap)

    -- perform a compare-and-swap on a @CRef@.
    ACasCRef cref@(CRef crid _) tick a c -> synchronised $ do
      (suc, tick', effect) <- casCRef cref tid tick a
      simple (goto (c (suc, tick')) tid (cThreads ctx)) (CasCRef crid suc) (const effect)

    -- commit a @CRef@ write
    ACommit t c -> do
      wb' <- case memtype of
        -- shouldn't ever get here
        SequentialConsistency ->
          fatal "stepThread.ACommit" "Attempting to commit under SequentialConsistency"
        -- commit using the thread id.
        TotalStoreOrder -> commitWrite (cWriteBuf ctx) (t, Nothing)
        -- commit using the cref id.
        PartialStoreOrder -> commitWrite (cWriteBuf ctx) (t, Just c)
      pure (Succeeded ctx { cWriteBuf = wb' }, Single (CommitCRef t c), noSnap)

    -- run a STM transaction atomically.
    AAtom stm c -> synchronised $ do
      let transaction = runTransaction stm (cIdSource ctx)
      let effect = const (void transaction)
      (res, idSource', trace) <- transaction
      case res of
        Success _ written val ->
          let (threads', woken) = wake (OnTVar written) (cThreads ctx)
          in pure (Succeeded ctx { cThreads = goto (c val) tid threads', cIdSource = idSource' }, Single (STM trace woken), effect)
        Retry touched ->
          let threads' = block (OnTVar touched) tid (cThreads ctx)
          in pure (Succeeded ctx { cThreads = threads', cIdSource = idSource'}, Single (BlockedSTM trace), effect)
        Exception e -> do
          let act = STM trace []
          res' <- stepThrow tid (cThreads ctx) act e
          pure $ case res' of
            (Succeeded ctx', _, effect') -> (Succeeded ctx' { cIdSource = idSource' }, Single act, effect')
            (Failed err, _, effect') -> (Failed err, Single act, effect')
            (Snap _, _, _) -> fatal "stepThread.AAtom" "Unexpected snapshot while propagating STM exception"

    -- lift an action from the underlying monad into the @Conc@
    -- computation.
    ALift na -> do
      let effect threads = runLiftedAct tid threads na
      a <- effect (cThreads ctx)
      simple (goto a tid (cThreads ctx)) LiftIO (void <$> effect)

    -- throw an exception, and propagate it to the appropriate
    -- handler.
    AThrow e -> stepThrow tid (cThreads ctx) Throw e

    -- throw an exception to the target thread, and propagate it to
    -- the appropriate handler.
    AThrowTo t e c -> synchronised $
      let threads' = goto c tid (cThreads ctx)
          blocked  = block (OnMask t) tid (cThreads ctx)
      in case M.lookup t (cThreads ctx) of
           Just thread
             | interruptible thread -> stepThrow t threads' (ThrowTo t) e
             | otherwise -> simple blocked (BlockedThrowTo t) noSnap
           Nothing -> simple threads' (ThrowTo t) noSnap

    -- run a subcomputation in an exception-catching context.
    ACatching h ma c ->
      let a        = runCont ma (APopCatching . c)
          e exc    = runCont (h exc) c
          threads' = goto a tid (catching e tid (cThreads ctx))
      in simple threads' Catching noSnap

    -- pop the top exception handler from the thread's stack.
    APopCatching a ->
      let threads' = goto a tid (uncatching tid (cThreads ctx))
      in simple threads' PopCatching noSnap

    -- execute a subcomputation with a new masking state, and give it
    -- a function to run a computation with the current masking state.
    AMasking m ma c ->
      let a = runCont (ma umask) (AResetMask False False m' . c)
          m' = _masking $ elookup "stepThread.AMasking" tid (cThreads ctx)
          umask mb = resetMask True m' >> mb >>= \b -> resetMask False m >> pure b
          resetMask typ ms = cont $ \k -> AResetMask typ True ms $ k ()
          threads' = goto a tid (mask m tid (cThreads ctx))
      in simple threads' (SetMasking False m) noSnap


    -- reset the masking thread of the state.
    AResetMask b1 b2 m c ->
      let act      = (if b1 then SetMasking else ResetMasking) b2 m
          threads' = goto c tid (mask m tid (cThreads ctx))
      in simple threads' act noSnap

    -- execute a 'return' or 'pure'.
    AReturn c -> simple (goto c tid (cThreads ctx)) Return noSnap

    -- kill the current thread.
    AStop na -> do
      na
      threads' <- kill tid (cThreads ctx)
      simple threads' Stop noSnap

    -- run a subconcurrent computation.
    ASub ma c
      | forSnapshot -> pure (Failed IllegalSubconcurrency, Single Subconcurrency, noSnap)
      | M.size (cThreads ctx) > 1 -> pure (Failed IllegalSubconcurrency, Single Subconcurrency, noSnap)
      | otherwise -> do
          res <- runConcurrency False sched memtype (cSchedState ctx) (cIdSource ctx) (cCaps ctx) ma
          out <- efromJust "stepThread.ASub" <$> readRef (finalRef res)
          pure (Succeeded ctx
                { cThreads    = goto (AStopSub (c out)) tid (cThreads ctx)
                , cIdSource   = cIdSource (finalContext res)
                , cSchedState = cSchedState (finalContext res)
                }
               , SubC (finalTrace res) (finalDecision res)
               , noSnap
               )

    -- after the end of a subconcurrent computation. does nothing,
    -- only exists so that: there is an entry in the trace for
    -- returning to normal computation; and every item in the trace
    -- corresponds to a scheduling point.
    AStopSub c -> simple (goto c tid (cThreads ctx)) StopSubconcurrency noSnap

    -- run an action atomically, with a non-preemptive length bounded
    -- round robin scheduler, under sequential consistency.
    ADontCheck lb ma c
      | isFirst -> do
          -- create a restricted context
          threads' <- kill tid (cThreads ctx)
          let dcCtx = ctx { cThreads = threads', cSchedState = lb }
          res <- runConcurrency' forSnapshot dcSched SequentialConsistency dcCtx ma
          out <- efromJust "stepThread.ADontCheck" <$> readRef (finalRef res)
          case out of
            Right a -> do
              let threads'' = launch' Unmasked tid (const (c a)) (cThreads (finalContext res))
              threads''' <- (if rtsSupportsBoundThreads then makeBound tid else pure) threads''
              pure ( (if forSnapshot then Snap else Succeeded) (finalContext res) { cThreads = threads''', cSchedState = cSchedState ctx }
                   , Single (DontCheck (toList (finalTrace res)))
                   , fromMaybe noSnap (finalRestore res)
                   )
            Left f ->
              pure (Failed f, Single (DontCheck (toList (finalTrace res))), noSnap)
      | otherwise -> pure (Failed IllegalDontCheck, Single (DontCheck []), noSnap)
  where

    -- this is not inline in the long @case@ above as it's needed by
    -- @AAtom@, @AThrow@, and @AThrowTo@.
    stepThrow t ts act e =
      let some = toException e
      in case propagate some t ts of
           Just ts' -> simple ts' act noSnap
           Nothing
             | t == initialThread -> pure (Failed (UncaughtException some), Single act, noSnap)
             | otherwise -> do
                 ts' <- kill t ts
                 simple ts' act noSnap

    -- helper for actions which only change the threads.
    simple threads' act effect = pure (Succeeded ctx { cThreads = threads' }, Single act, effect)

    -- helper for actions impose a write barrier.
    synchronised ma = do
      writeBarrier (cWriteBuf ctx)
      res <- ma

      pure $ case res of
        (Succeeded ctx', act, effect) -> (Succeeded ctx' { cWriteBuf = emptyBuffer }, act, effect)
        _ -> res

    -- scheduler for @ADontCheck@
    dcSched = Scheduler go where
      go _ _ (Just 0) = (Nothing, Just 0)
      go prior threads s =
        let (t, _) = scheduleThread roundRobinSchedNP prior threads ()
        in (t, fmap (\lb -> lb - 1) s)

    -- no snapshot
    noSnap _ = pure ()

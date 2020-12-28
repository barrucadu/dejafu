{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal
-- Copyright   : (c) 2016--2020 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : FlexibleContexts, LambdaCase, RankNTypes, RecordWildCards, ScopedTypeVariables
--
-- Concurrent monads with a fixed scheduler: internal types and
-- functions. This module is NOT considered to form part of the public
-- interface of this library.
module Test.DejaFu.Conc.Internal where

import           Control.Exception                   (Exception,
                                                      MaskingState(..),
                                                      toException)
import qualified Control.Monad.Catch                 as E
import           Data.Foldable                       (foldrM)
import           Data.Functor                        (void)
import           Data.List                           (nub, partition, sortOn)
import qualified Data.Map.Strict                     as M
import           Data.Maybe                          (isJust, isNothing)
import           Data.Monoid                         ((<>))
import           Data.Sequence                       (Seq)
import qualified Data.Sequence                       as Seq
import           GHC.Stack                           (HasCallStack)

import           Test.DejaFu.Conc.Internal.Common
import           Test.DejaFu.Conc.Internal.Memory
import           Test.DejaFu.Conc.Internal.STM
import           Test.DejaFu.Conc.Internal.Threading
import           Test.DejaFu.Internal
import           Test.DejaFu.Schedule
import           Test.DejaFu.Types

--------------------------------------------------------------------------------
-- * Set-up

-- | 'Trace' but as a sequence.
type SeqTrace
  = Seq (Decision, [(ThreadId, Lookahead)], ThreadAction)

-- | The result of running a concurrent program.
data CResult n g a = CResult
  { finalContext :: Context n g
  , finalRef :: Ref n (Maybe (Either Condition a))
  , finalRestore :: Threads n -> n ()
  -- ^ Meaningless if this result doesn't come from a snapshotting
  -- execution.
  , finalTrace :: SeqTrace
  , finalDecision :: Maybe (ThreadId, ThreadAction)
  }

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning a Condition reason on error. Also returned is the
-- final state of the scheduler, and an execution trace.
runConcurrency :: (MonadDejaFu n, HasCallStack)
  => [Invariant n ()]
  -> Bool
  -> Scheduler g
  -> MemType
  -> g
  -> IdSource
  -> Int
  -> ModelConc n a
  -> n (CResult n g a)
runConcurrency invariants forSnapshot sched memtype g idsrc caps ma = do
  let ctx = Context { cSchedState    = g
                    , cIdSource      = idsrc
                    , cThreads       = M.empty
                    , cWriteBuf      = emptyBuffer
                    , cCaps          = caps
                    , cInvariants    = InvariantContext { icActive = invariants, icBlocked = [] }
                    , cNewInvariants = []
                    , cCState        = initialCState
                    }
  (c, ref) <- runRefCont AStop (Just . Right) (runModelConc ma)
  let threads0 = launch' Unmasked initialThread (const c) (cThreads ctx)
  threads <- case forkBoundThread of
    Just fbt -> makeBound fbt initialThread threads0
    Nothing  -> pure threads0
  res <- runThreads forSnapshot sched memtype ref ctx { cThreads = threads }
  killAllThreads (finalContext res)
  pure res

-- | Like 'runConcurrency' but starts from a snapshot.
runConcurrencyWithSnapshot :: (MonadDejaFu n, HasCallStack)
  => Scheduler g
  -> MemType
  -> Context n g
  -> (Threads n -> n ())
  -> ModelConc n a
  -> n (CResult n g a)
runConcurrencyWithSnapshot sched memtype ctx restore ma = do
  (c, ref) <- runRefCont AStop (Just . Right) (runModelConc ma)
  let threads0 = M.delete initialThread (cThreads ctx)
  let threads1 = launch' Unmasked initialThread (const c) threads0
  threads <- case forkBoundThread of
    Just fbt -> do
      let boundThreads = M.filter (isJust . _bound) threads1
      threads' <- makeBound fbt initialThread threads1
      foldrM (makeBound fbt) threads' (M.keys boundThreads)
    Nothing -> pure threads1
  restore threads
  res <- runThreads False sched memtype ref ctx { cThreads = threads }
  killAllThreads (finalContext res)
  pure res

-- | Kill the remaining threads
killAllThreads :: (MonadDejaFu n, HasCallStack) => Context n g -> n ()
killAllThreads ctx =
  let finalThreads = cThreads ctx
  in mapM_ (`kill` finalThreads) (M.keys finalThreads)

-------------------------------------------------------------------------------
-- * Execution

-- | The context a collection of threads are running in.
data Context n g = Context
  { cSchedState    :: g
  , cIdSource      :: IdSource
  , cThreads       :: Threads n
  , cWriteBuf      :: WriteBuffer n
  , cCaps          :: Int
  , cInvariants    :: InvariantContext n
  , cNewInvariants :: [Invariant n ()]
  , cCState        :: ConcurrencyState
  }

-- | Run a collection of threads, until there are no threads left.
runThreads :: (MonadDejaFu n, HasCallStack)
  => Bool
  -> Scheduler g
  -> MemType
  -> Ref n (Maybe (Either Condition a))
  -> Context n g
  -> n (CResult n g a)
runThreads forSnapshot sched memtype ref = schedule (const $ pure ()) Seq.empty Nothing where
  -- signal failure & terminate
  die reason finalR finalT finalD finalC = do
    writeRef ref (Just $ Left reason)
    stop finalR finalT finalD finalC

  -- just terminate; 'ref' must have been written to before calling
  -- this
  stop finalR finalT finalD finalC = pure CResult
    { finalContext  = finalC
    , finalRef      = ref
    , finalRestore  = finalR
    , finalTrace    = finalT
    , finalDecision = finalD
    }

  -- check for termination, pick a thread, and call 'step'
  schedule restore sofar prior ctx
    | isTerminated  = stop restore sofar prior ctx
    | isDeadlocked  = die Deadlock restore sofar prior ctx
    | otherwise =
      let ctx' = ctx { cSchedState = g' }
      in case choice of
           Just chosen -> case M.lookup chosen threadsc of
             Just thread
               | isBlocked thread -> E.throwM ScheduledBlockedThread
               | otherwise ->
                 let decision
                       | Just chosen == (fst <$> prior) = Continue
                       | (fst <$> prior) `notElem` map (Just . fst) runnable' = Start chosen
                       | otherwise = SwitchTo chosen
                     alternatives = filter (\(t, _) -> t /= chosen) runnable'
                 in step decision alternatives chosen thread restore sofar prior ctx'
             Nothing -> E.throwM ScheduledMissingThread
           Nothing -> die Abort restore sofar prior ctx'
    where
      (choice, g')  = scheduleThread sched prior (efromList runnable') (cCState ctx) (cSchedState ctx)
      runnable'     = [(t, lookahead (_continuation a)) | (t, a) <- sortOn fst $ M.assocs runnable]
      runnable      = M.filter (not . isBlocked) threadsc
      threadsc      = addCommitThreads (cWriteBuf ctx) threads
      threads       = cThreads ctx
      isBlocked     = isJust . _blocking
      isTerminated  = initialThread `notElem` M.keys threads
      isDeadlocked  = M.null (M.filter (not . isBlocked) threads)

  -- run the chosen thread for one step and then pass control back to
  -- 'schedule'
  step decision alternatives chosen thread restore sofar prior ctx = do
      (res, actOrTrc, actionSnap) <- stepThread
          forSnapshot
          (isNothing prior)
          sched
          memtype
          chosen
          (_continuation thread)
          ctx
      let sofar' = sofar <> getTrc actOrTrc
      let prior' = getPrior actOrTrc
      let restore' threads' =
            if forSnapshot
            then restore threads' >> actionSnap threads'
            else restore threads'
      let ctx' = fixContext memtype chosen actOrTrc res ctx
      case res of
        Succeeded _ -> checkInvariants (cInvariants ctx') >>= \case
          Right ic ->
            schedule restore' sofar' prior' ctx' { cInvariants = ic }
          Left exc ->
            die (InvariantFailure exc) restore' sofar' prior' ctx'
        Failed failure ->
          die failure restore' sofar' prior' ctx'
    where
      getTrc a = Seq.singleton (decision, alternatives, a)

      getPrior a = Just (chosen, a)

-- | Apply the context update from stepping an action.
fixContext :: MemType -> ThreadId -> ThreadAction -> What n g -> Context n g -> Context n g
fixContext memtype tid act what ctx0 = fixContextCommon $ case what of
    Succeeded ctx@Context{..} -> ctx
      { cThreads =
          if (interruptible <$> M.lookup tid cThreads) /= Just False
          then unblockWaitingOn tid cThreads
          else cThreads
      }
    _ -> ctx0
  where
  fixContextCommon ctx@Context{..} = ctx
    { cThreads    = delCommitThreads cThreads
    , cInvariants = unblockInvariants act cInvariants
    , cCState     = updateCState memtype cCState tid act
    }

-- | @unblockWaitingOn tid@ unblocks every thread blocked in a
-- @throwTo tid@.
unblockWaitingOn :: ThreadId -> Threads n -> Threads n
unblockWaitingOn tid = fmap $ \thread -> case _blocking thread of
  Just (OnMask t) | t == tid -> thread { _blocking = Nothing }
  _ -> thread

--------------------------------------------------------------------------------
-- * Single-step execution

-- | What a thread did, for execution purposes.
data What n g
  = Succeeded (Context n g)
  -- ^ Action succeeded: continue execution.
  | Failed Condition
  -- ^ Action caused computation to fail: stop.

-- | Run a single thread one step, by dispatching on the type of
-- 'Action'.
--
-- Each case looks very similar.  This is deliberate, so that the
-- essential differences between actions are more apparent, and not
-- hidden by accidental differences in how things are expressed.
--
-- Note: the returned snapshot action will definitely not do the right
-- thing with relaxed memory.
stepThread :: forall n g. (MonadDejaFu n, HasCallStack)
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
  -> Action n
  -- ^ Action to step
  -> Context n g
  -- ^ The execution context.
  -> n (What n g, ThreadAction, Threads n -> n ())
-- start a new thread, assigning it the next 'ThreadId'
stepThread _ _ _ _ tid (AFork n a b) = \ctx@Context{..} -> pure $
  let (idSource', newtid) = nextTId n cIdSource
      threads' = launch tid newtid a cThreads
  in ( Succeeded ctx { cThreads = goto (b newtid) tid threads', cIdSource = idSource' }
     , Fork newtid
     , const (pure ())
     )

-- start a new bound thread, assigning it the next 'ThreadId'
stepThread _ _ _ _ tid (AForkOS n a b) = \ctx@Context{..} -> case forkBoundThread of
  Just fbt -> do
    let (idSource', newtid) = nextTId n cIdSource
    let threads' = launch tid newtid a cThreads
    threads'' <- makeBound fbt newtid threads'
    pure ( Succeeded ctx { cThreads = goto (b newtid) tid threads'', cIdSource = idSource' }
         , ForkOS newtid
         , const (pure ())
         )
  Nothing ->
    stepThrow Throw tid (MonadFailException "dejafu is running with bound threads disabled - do not use forkOS") ctx

-- check if we support bound threads
stepThread _ _ _ _ tid (ASupportsBoundThreads c) = \ctx@Context{..} -> do
  let sbt = isJust (forkBoundThread :: Maybe (n (BoundThread n ())))
  pure ( Succeeded ctx { cThreads = goto (c sbt) tid cThreads }
       , SupportsBoundThreads sbt
       , const (pure ())
       )

-- check if the current thread is bound
stepThread _ _ _ _ tid (AIsBound c) = \ctx@Context{..} -> do
  let isBound = isJust . _bound $ elookup tid cThreads
  pure ( Succeeded ctx { cThreads = goto (c isBound) tid cThreads }
       , IsCurrentThreadBound isBound
       , const (pure ())
       )

-- get the 'ThreadId' of the current thread
stepThread _ _ _ _ tid (AMyTId c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto (c tid) tid cThreads }
       , MyThreadId
       , const (pure ())
       )

-- get the number of capabilities
stepThread _ _ _ _ tid (AGetNumCapabilities c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto (c cCaps) tid cThreads }
       , GetNumCapabilities cCaps
       , const (pure ())
       )

-- set the number of capabilities
stepThread _ _ _ _ tid (ASetNumCapabilities i c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto c tid cThreads, cCaps = i }
       , SetNumCapabilities i
       , const (pure ())
       )

-- yield the current thread
stepThread _ _ _ _ tid (AYield c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto c tid cThreads }
       , Yield
       , const (pure ())
       )

-- yield the current thread (delay is ignored)
stepThread _ _ _ _ tid (ADelay n c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto c tid cThreads }
       , ThreadDelay n
       , const (pure ())
       )

-- create a new @MVar@, using the next 'MVarId'.
stepThread _ _ _ _ tid (ANewMVar n c) = \ctx@Context{..} -> do
  let (idSource', newmvid) = nextMVId n cIdSource
  ref <- newRef Nothing
  let mvar = ModelMVar newmvid ref
  pure ( Succeeded ctx { cThreads = goto (c mvar) tid cThreads, cIdSource = idSource' }
       , NewMVar newmvid
       , const (writeRef ref Nothing)
       )

-- put a value into a @MVar@, blocking the thread until it's empty.
stepThread _ _ _ _ tid (APutMVar mvar@ModelMVar{..} a c) = synchronised $ \ctx@Context{..} -> do
  (success, threads', woken, effect) <- putIntoMVar mvar a c tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , if success then PutMVar mvarId woken else BlockedPutMVar mvarId
       , const effect
       )

-- try to put a value into a @MVar@, without blocking.
stepThread _ _ _ _ tid (ATryPutMVar mvar@ModelMVar{..} a c) = synchronised $ \ctx@Context{..} -> do
  (success, threads', woken, effect) <- tryPutIntoMVar mvar a c tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , TryPutMVar mvarId success woken
       , const effect
       )

-- get the value from a @MVar@, without emptying, blocking the thread
-- until it's full.
stepThread _ _ _ _ tid (AReadMVar mvar@ModelMVar{..} c) = synchronised $ \ctx@Context{..} -> do
  (success, threads', _, _) <- readFromMVar mvar c tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , if success then ReadMVar mvarId else BlockedReadMVar mvarId
       , const (pure ())
       )

-- try to get the value from a @MVar@, without emptying, without
-- blocking.
stepThread _ _ _ _ tid (ATryReadMVar mvar@ModelMVar{..} c) = synchronised $ \ctx@Context{..} -> do
  (success, threads', _, _) <- tryReadFromMVar mvar c tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , TryReadMVar mvarId success
       , const (pure ())
       )

-- take the value from a @MVar@, blocking the thread until it's full.
stepThread _ _ _ _ tid (ATakeMVar mvar@ModelMVar{..} c) = synchronised $ \ctx@Context{..} -> do
  (success, threads', woken, effect) <- takeFromMVar mvar c tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , if success then TakeMVar mvarId woken else BlockedTakeMVar mvarId
       , const effect
       )

-- try to take the value from a @MVar@, without blocking.
stepThread _ _ _ _ tid (ATryTakeMVar mvar@ModelMVar{..} c) = synchronised $ \ctx@Context{..} -> do
  (success, threads', woken, effect) <- tryTakeFromMVar mvar c tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , TryTakeMVar mvarId success woken
       , const effect
       )

-- create a new @IORef@, using the next 'IORefId'.
stepThread _ _ _ _  tid (ANewIORef n a c) = \ctx@Context{..} -> do
  let (idSource', newiorid) = nextIORId n cIdSource
  let val = (M.empty, 0, a)
  ioref <- newRef val
  let ref = ModelIORef newiorid ioref
  pure ( Succeeded ctx { cThreads = goto (c ref) tid cThreads, cIdSource = idSource' }
       , NewIORef newiorid
       , const (writeRef ioref val)
       )

-- read from a @IORef@.
stepThread _ _ _ _  tid (AReadIORef ref@ModelIORef{..} c) = \ctx@Context{..} -> do
  val <- readIORef ref tid
  pure ( Succeeded ctx { cThreads = goto (c val) tid cThreads }
       , ReadIORef iorefId
       , const (pure ())
       )

-- read from a @IORef@ for future compare-and-swap operations.
stepThread _ _ _ _ tid (AReadIORefCas ref@ModelIORef{..} c) = \ctx@Context{..} -> do
  tick <- readForTicket ref tid
  pure ( Succeeded ctx { cThreads = goto (c tick) tid cThreads }
       , ReadIORefCas iorefId
       , const (pure ())
       )

-- modify a @IORef@.
stepThread _ _ _ _ tid (AModIORef ref@ModelIORef{..} f c) = synchronised $ \ctx@Context{..} -> do
  (new, val) <- f <$> readIORef ref tid
  effect <- writeImmediate ref new
  pure ( Succeeded ctx { cThreads = goto (c val) tid cThreads }
       , ModIORef iorefId
       , const effect
       )

-- modify a @IORef@ using a compare-and-swap.
stepThread _ _ _ _ tid (AModIORefCas ref@ModelIORef{..} f c) = synchronised $ \ctx@Context{..} -> do
  tick@(ModelTicket _ _ old) <- readForTicket ref tid
  let (new, val) = f old
  (_, _, effect) <- casIORef ref tid tick new
  pure ( Succeeded ctx { cThreads = goto (c val) tid cThreads }
       , ModIORefCas iorefId
       , const effect
       )

-- write to a @IORef@ without synchronising.
stepThread _ _ _ memtype tid (AWriteIORef ref@ModelIORef{..} a c) = \ctx@Context{..} -> case memtype of
  -- write immediately.
  SequentialConsistency -> do
    effect <- writeImmediate ref a
    pure ( Succeeded ctx { cThreads = goto c tid cThreads }
         , WriteIORef iorefId
         , const effect
         )
  -- add to buffer using thread id.
  TotalStoreOrder -> do
    wb' <- bufferWrite cWriteBuf (tid, Nothing) ref a
    pure ( Succeeded ctx { cThreads = goto c tid cThreads, cWriteBuf = wb' }
         , WriteIORef iorefId
         , const (pure ())
         )
  -- add to buffer using both thread id and IORef id
  PartialStoreOrder -> do
    wb' <- bufferWrite cWriteBuf (tid, Just iorefId) ref a
    pure ( Succeeded ctx { cThreads = goto c tid cThreads, cWriteBuf = wb' }
         , WriteIORef iorefId
         , const (pure ())
         )

-- perform a compare-and-swap on a @IORef@.
stepThread _ _ _ _ tid (ACasIORef ref@ModelIORef{..} tick a c) = synchronised $ \ctx@Context{..} -> do
  (suc, tick', effect) <- casIORef ref tid tick a
  pure ( Succeeded ctx { cThreads = goto (c (suc, tick')) tid cThreads }
       , CasIORef iorefId suc
       , const effect
       )

-- commit a @IORef@ write
stepThread _ _ _ memtype _ (ACommit t c) = \ctx@Context{..} -> do
  wb' <- case memtype of
    -- shouldn't ever get here
    SequentialConsistency ->
      fatal "stepThread.ACommit" "Attempting to commit under SequentialConsistency"
    -- commit using the thread id.
    TotalStoreOrder ->
      commitWrite cWriteBuf (t, Nothing)
    -- commit using the IORef id.
    PartialStoreOrder ->
      commitWrite cWriteBuf (t, Just c)
  pure ( Succeeded ctx { cWriteBuf = wb' }
       , CommitIORef t c
       , const (pure ())
       )

-- run a STM transaction atomically.
stepThread _ _ _ _ tid (AAtom stm c) = synchronised $ \ctx@Context{..} -> do
  (res, effect, idSource', trace) <- runTransaction stm cIdSource
  case res of
    Success _ written val -> do
      let (threads', woken) = wake (OnTVar written) cThreads
      pure ( Succeeded ctx { cThreads = goto (c val) tid threads', cIdSource = idSource' }
           , STM trace woken
           , const effect
           )
    Retry touched -> do
      let threads' = block (OnTVar touched) tid cThreads
      pure ( Succeeded ctx { cThreads = threads', cIdSource = idSource'}
           , BlockedSTM trace
           , const effect
           )
    Exception e -> do
      res' <- stepThrow (ThrownSTM trace) tid e ctx
      pure $ case res' of
        (Succeeded ctx', act, effect') -> (Succeeded ctx' { cIdSource = idSource' }, act, effect')
        (Failed err, act, effect') -> (Failed err, act, effect')

-- lift an action from the underlying monad into the @Conc@
-- computation.
stepThread _ _ _ _ tid (ALift na) = \ctx@Context{..} -> do
  let effect threads = runLiftedAct tid threads na
  a <- effect cThreads
  pure (Succeeded ctx { cThreads = goto a tid cThreads }
       , LiftIO
       , void <$> effect
       )

-- throw an exception, and propagate it to the appropriate handler.
stepThread _ _ _ _ tid (AThrow e) = stepThrow Throw tid e

-- throw an exception to the target thread, and propagate it to the
-- appropriate handler.
stepThread _ _ _ _ tid (AThrowTo t e c) = synchronised $ \ctx@Context{..} ->
  let threads' = goto c tid cThreads
      blocked  = block (OnMask t) tid cThreads
  in case M.lookup t cThreads of
       Just thread
         | interruptible thread || t == tid -> stepThrow (ThrowTo t) t e ctx { cThreads = threads' }
         | otherwise -> pure
           ( Succeeded ctx { cThreads = blocked }
           , BlockedThrowTo t
           , const (pure ())
           )
       Nothing -> pure
         (Succeeded ctx { cThreads = threads' }
         , ThrowTo t Nothing
         , const (pure ())
         )

-- run a subcomputation in an exception-catching context.
stepThread _ _ _ _ tid (ACatching h ma c) = \ctx@Context{..} -> pure $
  let a     = runModelConc ma (APopCatching . c)
      e exc = runModelConc (h exc) c
  in ( Succeeded ctx { cThreads = goto a tid (catching e tid cThreads) }
     , Catching
     , const (pure ())
     )

-- pop the top exception handler from the thread's stack.
stepThread _ _ _ _ tid (APopCatching a) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto a tid (uncatching tid cThreads) }
       , PopCatching
       , const (pure ())
       )

-- execute a subcomputation with a new masking state, and give it a
-- function to run a computation with the current masking state.
stepThread _ _ _ _ tid (AMasking m ma c) = \ctx@Context{..} -> pure $
  let resetMask typ ms = ModelConc $ \k -> AResetMask typ True ms $ k ()
      umask mb = resetMask True m' >> mb >>= \b -> resetMask False m >> pure b
      m' = _masking $ elookup tid cThreads
      a  = runModelConc (ma umask) (AResetMask False False m' . c)
  in ( Succeeded ctx { cThreads = goto a tid (mask m tid cThreads) }
     , SetMasking False m
     , const (pure ())
     )

-- reset the masking thread of the state.
stepThread _ _ _ _ tid (AResetMask b1 b2 m c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto c tid (mask m tid cThreads) }
       , (if b1 then SetMasking else ResetMasking) b2 m
       , const (pure ())
       )

-- get the current masking state.
stepThread _ _ _ _ tid (AGetMasking c) = \ctx@Context{..} -> pure $
  let m = _masking $ elookup tid cThreads
  in ( Succeeded ctx { cThreads = goto (c m) tid cThreads }
     , GetMaskingState m
     , const (pure ())
     )

-- execute a 'return' or 'pure'.
stepThread _ _ _ _ tid (AReturn c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto c tid cThreads }
       , Return
       , const (pure ())
       )

-- kill the current thread.
stepThread _ _ _ _ tid (AStop na) = \ctx@Context{..} -> do
  na
  threads' <- kill tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , Stop
       , const (pure ())
       )

-- register an invariant to be checked in the next execution
stepThread _ _ _ _ tid (ANewInvariant inv c) = \ctx@Context{..} ->
  pure ( Succeeded ctx
         { cThreads = goto c tid cThreads
         , cNewInvariants = inv : cNewInvariants
         }
       , RegisterInvariant
       , const (pure ())
       )

-- | Handle an exception being thrown from an @AAtom@, @AThrow@, or
-- @AThrowTo@.
stepThrow :: (MonadDejaFu n, Exception e)
  => (Maybe MaskingState -> ThreadAction)
  -- ^ Action to include in the trace.
  -> ThreadId
  -- ^ The thread receiving the exception.
  -> e
  -- ^ Exception to raise.
  -> Context n g
  -- ^ The execution context.
  -> n (What n g, ThreadAction, Threads n -> n ())
stepThrow act tid e ctx@Context{..} = case propagate some tid cThreads of
    Just ts' -> pure
      ( Succeeded ctx { cThreads = ts' }
      , act (Just . _masking $ elookup tid ts')
      , const (pure ())
      )
    Nothing
      | tid == initialThread -> pure
        ( Failed (UncaughtException some)
        , act Nothing
        , const (pure ())
        )
      | otherwise -> do
          ts' <- kill tid cThreads
          pure ( Succeeded ctx { cThreads = ts' }
               , act Nothing
               , const (pure ())
               )
  where
    some = toException e

-- | Helper for actions impose a write barrier.
synchronised :: MonadDejaFu n
  => (Context n g -> n x)
  -- ^ Action to run after the write barrier.
  -> Context n g
  -- ^ The original execution context.
  -> n x
synchronised ma ctx@Context{..} = do
  writeBarrier cWriteBuf
  ma ctx { cWriteBuf = emptyBuffer }

--------------------------------------------------------------------------------
-- * Invariants

-- | The state of the invariants
data InvariantContext n = InvariantContext
  { icActive  :: [Invariant n ()]
  , icBlocked :: [(Invariant n (), ([IORefId], [MVarId], [TVarId]))]
  }

-- | @unblockInvariants act@ unblocks every invariant which could have
-- its result changed by @act@.
unblockInvariants ::  ThreadAction -> InvariantContext n -> InvariantContext n
unblockInvariants act ic = InvariantContext active blocked where
  active = map fst unblocked ++ icActive ic

  (unblocked, blocked) = (`partition` icBlocked ic) $
    \(_, (ioridsB, mvidsB, tvidsB)) ->
      maybe False (`elem` ioridsB) (iorefOf (simplifyAction act)) ||
      maybe False (`elem` mvidsB)  (mvarOf (simplifyAction act))  ||
      any (`elem` tvidsB) (tvarsOf act)

-- | Check all active invariants, returning an arbitrary failure if
-- multiple ones fail.
checkInvariants :: MonadDejaFu n
  => InvariantContext n
  -> n (Either E.SomeException (InvariantContext n))
checkInvariants ic = go (icActive ic) >>= \case
    Right blocked -> pure (Right (InvariantContext [] (blocked ++ icBlocked ic)))
    Left exc -> pure (Left exc)
  where
    go (inv:is) = checkInvariant inv >>= \case
      Right o -> fmap ((inv,o):) <$> go is
      Left exc -> pure (Left exc)
    go [] = pure (Right [])

-- | Check an invariant.
checkInvariant :: MonadDejaFu n
  => Invariant n a
  -> n (Either E.SomeException ([IORefId], [MVarId], [TVarId]))
checkInvariant inv = doInvariant inv >>= \case
  (Right _, iorefs, mvars, tvars) -> pure (Right (iorefs, mvars, tvars))
  (Left exc, _, _, _) -> pure (Left exc)

-- | Run an invariant (more primitive)
doInvariant :: MonadDejaFu n
  => Invariant n a
  -> n (Either E.SomeException a, [IORefId], [MVarId], [TVarId])
doInvariant inv = do
    (c, ref) <- runRefCont IStop (Just . Right) (runInvariant inv)
    (iorefs, mvars, tvars) <- go ref c [] [] []
    val <- readRef ref
    pure (efromJust val, nub iorefs, nub mvars, nub tvars)
  where
    go ref act iorefs mvars tvars = do
      (res, iorefs', mvars', tvars') <- stepInvariant act
      let newIORefs = iorefs' ++ iorefs
      let newMVars  = mvars'  ++ mvars
      let newTVars  = tvars'  ++ tvars
      case res of
        Right (Just act') ->
          go ref act' newIORefs newMVars newTVars
        Right Nothing ->
          pure (newIORefs, newMVars, newTVars)
        Left exc -> do
          writeRef ref (Just (Left exc))
          pure (newIORefs, newMVars, newTVars)

-- | Run an invariant for one step
stepInvariant :: MonadDejaFu n
  => IAction n
  -> n (Either E.SomeException (Maybe (IAction n)), [IORefId], [MVarId], [TVarId])
stepInvariant (IInspectIORef ioref@ModelIORef{..} k) = do
  a <- readIORefGlobal ioref
  pure (Right (Just (k a)), [iorefId], [], [])
stepInvariant (IInspectMVar ModelMVar{..} k) = do
  a <- readRef mvarRef
  pure (Right (Just (k a)), [], [mvarId], [])
stepInvariant (IInspectTVar ModelTVar{..} k) = do
  a <- readRef tvarRef
  pure (Right (Just (k a)), [], [], [tvarId])
stepInvariant (ICatch h nx k) = doInvariant nx >>= \case
  (Right a, iorefs, mvars, tvars) ->
    pure (Right (Just (k a)), iorefs, mvars, tvars)
  (Left exc, iorefs, mvars, tvars) -> case E.fromException exc of
    Just exc' -> doInvariant (h exc') >>= \case
      (Right a, iorefs', mvars', tvars') ->
        pure (Right (Just (k a)), iorefs' ++ iorefs, mvars' ++ mvars, tvars' ++ tvars)
      (Left exc'', iorefs', mvars', tvars') ->
        pure (Left exc'', iorefs' ++ iorefs, mvars' ++ mvars, tvars' ++ tvars)
    Nothing -> pure (Left exc, iorefs, mvars, tvars)
stepInvariant (IThrow exc) =
  pure (Left (toException exc), [], [], [])
stepInvariant (IStop finalise) = do
  finalise
  pure (Right Nothing, [], [], [])

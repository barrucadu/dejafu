{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal
-- Copyright   : (c) 2016--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : MultiParamTypeClasses, MultiWayIf, RankNTypes, RecordWildCards, ScopedTypeVariables
--
-- Concurrent monads with a fixed scheduler: internal types and
-- functions. This module is NOT considered to form part of the public
-- interface of this library.
module Test.DejaFu.Conc.Internal where

import           Control.Exception                   (Exception,
                                                      MaskingState(..),
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
  -> ModelConc n r a
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
  -> ModelConc n r a
  -> n (CResult n r g a)
runConcurrency' forSnapshot sched memtype ctx ma = do
  (c, ref) <- runRefCont AStop (Just . Right) (runModelConc ma)
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
-- Each case looks very similar.  This is deliberate, so that the
-- essential differences between actions are more apparent, and not
-- hidden by accidental differences in how things are expressed.
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
-- start a new thread, assigning it the next 'ThreadId'
stepThread _ _ _ _ tid (AFork n a b) = \ctx@Context{..} -> pure $
  let (idSource', newtid) = nextTId n cIdSource
      threads' = launch tid newtid a cThreads
  in ( Succeeded ctx { cThreads = goto (b newtid) tid threads', cIdSource = idSource' }
     , Single (Fork newtid)
     , const (pure ())
     )

-- start a new bound thread, assigning it the next 'ThreadId'
stepThread _ _ _ _ tid (AForkOS n a b) = \ctx@Context{..} -> do
  let (idSource', newtid) = nextTId n cIdSource
  let threads' = launch tid newtid a cThreads
  threads'' <- makeBound newtid threads'
  pure ( Succeeded ctx { cThreads = goto (b newtid) tid threads'', cIdSource = idSource' }
       , Single (ForkOS newtid)
       , const (pure ())
       )

-- check if the current thread is bound
stepThread _ _ _ _ tid (AIsBound c) = \ctx@Context{..} -> do
  let isBound = isJust . _bound $ elookup "stepThread.AIsBound" tid cThreads
  pure ( Succeeded ctx { cThreads = goto (c isBound) tid cThreads }
       , Single (IsCurrentThreadBound isBound)
       , const (pure ())
       )

-- get the 'ThreadId' of the current thread
stepThread _ _ _ _ tid (AMyTId c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto (c tid) tid cThreads }
       , Single MyThreadId
       , const (pure ())
       )

-- get the number of capabilities
stepThread _ _ _ _ tid (AGetNumCapabilities c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto (c cCaps) tid cThreads }
       , Single (GetNumCapabilities cCaps)
       , const (pure ())
       )

-- set the number of capabilities
stepThread _ _ _ _ tid (ASetNumCapabilities i c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto c tid cThreads, cCaps = i }
       , Single (SetNumCapabilities i)
       , const (pure ())
       )

-- yield the current thread
stepThread _ _ _ _ tid (AYield c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto c tid cThreads }
       , Single Yield
       , const (pure ())
       )

-- yield the current thread (delay is ignored)
stepThread _ _ _ _ tid (ADelay n c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto c tid cThreads }
       , Single (ThreadDelay n)
       , const (pure ())
       )

-- create a new @MVar@, using the next 'MVarId'.
stepThread _ _ _ _ tid (ANewMVar n c) = \ctx@Context{..} -> do
  let (idSource', newmvid) = nextMVId n cIdSource
  ref <- newRef Nothing
  let mvar = ModelMVar newmvid ref
  pure ( Succeeded ctx { cThreads = goto (c mvar) tid cThreads, cIdSource = idSource' }
       , Single (NewMVar newmvid)
       , const (writeRef ref Nothing)
       )

-- put a value into a @MVar@, blocking the thread until it's empty.
stepThread _ _ _ _ tid (APutMVar mvar@ModelMVar{..} a c) = synchronised $ \ctx@Context{..} -> do
  (success, threads', woken, effect) <- putIntoMVar mvar a c tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , Single (if success then PutMVar mvarId woken else BlockedPutMVar mvarId)
       , const effect
       )

-- try to put a value into a @MVar@, without blocking.
stepThread _ _ _ _ tid (ATryPutMVar mvar@ModelMVar{..} a c) = synchronised $ \ctx@Context{..} -> do
  (success, threads', woken, effect) <- tryPutIntoMVar mvar a c tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , Single (TryPutMVar mvarId success woken)
       , const effect
       )

-- get the value from a @MVar@, without emptying, blocking the thread
-- until it's full.
stepThread _ _ _ _ tid (AReadMVar mvar@ModelMVar{..} c) = synchronised $ \ctx@Context{..} -> do
  (success, threads', _, _) <- readFromMVar mvar c tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , Single (if success then ReadMVar mvarId else BlockedReadMVar mvarId)
       , const (pure ())
       )

-- try to get the value from a @MVar@, without emptying, without
-- blocking.
stepThread _ _ _ _ tid (ATryReadMVar mvar@ModelMVar{..} c) = synchronised $ \ctx@Context{..} -> do
  (success, threads', _, _) <- tryReadFromMVar mvar c tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , Single (TryReadMVar mvarId success)
       , const (pure ())
       )

-- take the value from a @MVar@, blocking the thread until it's full.
stepThread _ _ _ _ tid (ATakeMVar mvar@ModelMVar{..} c) = synchronised $ \ctx@Context{..} -> do
  (success, threads', woken, effect) <- takeFromMVar mvar c tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , Single (if success then TakeMVar mvarId woken else BlockedTakeMVar mvarId)
       , const effect
       )

-- try to take the value from a @MVar@, without blocking.
stepThread _ _ _ _ tid (ATryTakeMVar mvar@ModelMVar{..} c) = synchronised $ \ctx@Context{..} -> do
  (success, threads', woken, effect) <- tryTakeFromMVar mvar c tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , Single (TryTakeMVar mvarId success woken)
       , const effect
       )

-- create a new @CRef@, using the next 'CRefId'.
stepThread _ _ _ _  tid (ANewCRef n a c) = \ctx@Context{..} -> do
  let (idSource', newcrid) = nextCRId n cIdSource
  let val = (M.empty, 0, a)
  ref <- newRef val
  let cref = ModelCRef newcrid ref
  pure ( Succeeded ctx { cThreads = goto (c cref) tid cThreads, cIdSource = idSource' }
       , Single (NewCRef newcrid)
       , const (writeRef ref val)
       )

-- read from a @CRef@.
stepThread _ _ _ _  tid (AReadCRef cref@ModelCRef{..} c) = \ctx@Context{..} -> do
  val <- readCRef cref tid
  pure ( Succeeded ctx { cThreads = goto (c val) tid cThreads }
       , Single (ReadCRef crefId)
       , const (pure ())
       )

-- read from a @CRef@ for future compare-and-swap operations.
stepThread _ _ _ _ tid (AReadCRefCas cref@ModelCRef{..} c) = \ctx@Context{..} -> do
  tick <- readForTicket cref tid
  pure ( Succeeded ctx { cThreads = goto (c tick) tid cThreads }
       , Single (ReadCRefCas crefId)
       , const (pure ())
       )

-- modify a @CRef@.
stepThread _ _ _ _ tid (AModCRef cref@ModelCRef{..} f c) = synchronised $ \ctx@Context{..} -> do
  (new, val) <- f <$> readCRef cref tid
  effect <- writeImmediate cref new
  pure ( Succeeded ctx { cThreads = goto (c val) tid cThreads }
       , Single (ModCRef crefId)
       , const effect
       )

-- modify a @CRef@ using a compare-and-swap.
stepThread _ _ _ _ tid (AModCRefCas cref@ModelCRef{..} f c) = synchronised $ \ctx@Context{..} -> do
  tick@(ModelTicket _ _ old) <- readForTicket cref tid
  let (new, val) = f old
  (_, _, effect) <- casCRef cref tid tick new
  pure ( Succeeded ctx { cThreads = goto (c val) tid cThreads }
       , Single (ModCRefCas crefId)
       , const effect
       )

-- write to a @CRef@ without synchronising.
stepThread _ _ _ memtype tid (AWriteCRef cref@ModelCRef{..} a c) = \ctx@Context{..} -> case memtype of
  -- write immediately.
  SequentialConsistency -> do
    effect <- writeImmediate cref a
    pure ( Succeeded ctx { cThreads = goto c tid cThreads }
         , Single (WriteCRef crefId)
         , const effect
         )
  -- add to buffer using thread id.
  TotalStoreOrder -> do
    wb' <- bufferWrite cWriteBuf (tid, Nothing) cref a
    pure ( Succeeded ctx { cThreads = goto c tid cThreads, cWriteBuf = wb' }
         , Single (WriteCRef crefId)
         , const (pure ())
         )
  -- add to buffer using both thread id and cref id
  PartialStoreOrder -> do
    wb' <- bufferWrite cWriteBuf (tid, Just crefId) cref a
    pure ( Succeeded ctx { cThreads = goto c tid cThreads, cWriteBuf = wb' }
         , Single (WriteCRef crefId)
         , const (pure ())
         )

-- perform a compare-and-swap on a @CRef@.
stepThread _ _ _ _ tid (ACasCRef cref@ModelCRef{..} tick a c) = synchronised $ \ctx@Context{..} -> do
  (suc, tick', effect) <- casCRef cref tid tick a
  pure ( Succeeded ctx { cThreads = goto (c (suc, tick')) tid cThreads }
       , Single (CasCRef crefId suc)
       , const effect
       )

-- commit a @CRef@ write
stepThread _ _ _ memtype _ (ACommit t c) = \ctx@Context{..} -> do
  wb' <- case memtype of
    -- shouldn't ever get here
    SequentialConsistency ->
      fatal "stepThread.ACommit" "Attempting to commit under SequentialConsistency"
    -- commit using the thread id.
    TotalStoreOrder ->
      commitWrite cWriteBuf (t, Nothing)
    -- commit using the cref id.
    PartialStoreOrder ->
      commitWrite cWriteBuf (t, Just c)
  pure ( Succeeded ctx { cWriteBuf = wb' }
       , Single (CommitCRef t c)
       , const (pure ())
       )

-- run a STM transaction atomically.
stepThread _ _ _ _ tid (AAtom stm c) = synchronised $ \ctx@Context{..} -> do
  let transaction = runTransaction stm cIdSource
  let effect = const (void transaction)
  (res, idSource', trace) <- transaction
  case res of
    Success _ written val -> do
      let (threads', woken) = wake (OnTVar written) cThreads
      pure ( Succeeded ctx { cThreads = goto (c val) tid threads', cIdSource = idSource' }
           , Single (STM trace woken)
           , effect
           )
    Retry touched -> do
      let threads' = block (OnTVar touched) tid cThreads
      pure ( Succeeded ctx { cThreads = threads', cIdSource = idSource'}
           , Single (BlockedSTM trace)
           , effect
           )
    Exception e -> do
      let act = STM trace []
      res' <- stepThrow act tid e ctx
      pure $ case res' of
        (Succeeded ctx', _, effect') -> (Succeeded ctx' { cIdSource = idSource' }, Single act, effect')
        (Failed err, _, effect') -> (Failed err, Single act, effect')
        (Snap _, _, _) -> fatal "stepThread.AAtom" "Unexpected snapshot while propagating STM exception"

-- lift an action from the underlying monad into the @Conc@
-- computation.
stepThread _ _ _ _ tid (ALift na) = \ctx@Context{..} -> do
  let effect threads = runLiftedAct tid threads na
  a <- effect cThreads
  pure (Succeeded ctx { cThreads = goto a tid cThreads }
       , Single LiftIO
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
         | interruptible thread -> stepThrow (ThrowTo t) t e ctx { cThreads = threads' }
         | otherwise -> pure
           ( Succeeded ctx { cThreads = blocked }
           , Single (BlockedThrowTo t)
           , const (pure ())
           )
       Nothing -> pure
         (Succeeded ctx { cThreads = threads' }
         , Single (ThrowTo t)
         , const (pure ())
         )

-- run a subcomputation in an exception-catching context.
stepThread _ _ _ _ tid (ACatching h ma c) = \ctx@Context{..} -> pure $
  let a     = runModelConc ma (APopCatching . c)
      e exc = runModelConc (h exc) c
  in ( Succeeded ctx { cThreads = goto a tid (catching e tid cThreads) }
     , Single Catching
     , const (pure ())
     )

-- pop the top exception handler from the thread's stack.
stepThread _ _ _ _ tid (APopCatching a) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto a tid (uncatching tid cThreads) }
       , Single PopCatching
       , const (pure ())
       )

-- execute a subcomputation with a new masking state, and give it a
-- function to run a computation with the current masking state.
stepThread _ _ _ _ tid (AMasking m ma c) = \ctx@Context{..} -> pure $
  let resetMask typ ms = ModelConc $ \k -> AResetMask typ True ms $ k ()
      umask mb = resetMask True m' >> mb >>= \b -> resetMask False m >> pure b
      m' = _masking $ elookup "stepThread.AMasking" tid cThreads
      a  = runModelConc (ma umask) (AResetMask False False m' . c)
  in ( Succeeded ctx { cThreads = goto a tid (mask m tid cThreads) }
     , Single (SetMasking False m)
     , const (pure ())
     )

-- reset the masking thread of the state.
stepThread _ _ _ _ tid (AResetMask b1 b2 m c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto c tid (mask m tid cThreads) }
       , Single ((if b1 then SetMasking else ResetMasking) b2 m)
       , const (pure ())
       )

-- execute a 'return' or 'pure'.
stepThread _ _ _ _ tid (AReturn c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto c tid cThreads }
       , Single Return
       , const (pure ())
       )

-- kill the current thread.
stepThread _ _ _ _ tid (AStop na) = \ctx@Context{..} -> do
  na
  threads' <- kill tid cThreads
  pure ( Succeeded ctx { cThreads = threads' }
       , Single Stop
       , const (pure ())
       )

-- run a subconcurrent computation.
stepThread forSnapshot _ sched memtype tid (ASub ma c) = \ctx ->
  if | forSnapshot -> pure (Failed IllegalSubconcurrency, Single Subconcurrency, const (pure ()))
     | M.size (cThreads ctx) > 1 -> pure (Failed IllegalSubconcurrency, Single Subconcurrency, const (pure ()))
     | otherwise -> do
         res <- runConcurrency False sched memtype (cSchedState ctx) (cIdSource ctx) (cCaps ctx) ma
         out <- efromJust "stepThread.ASub" <$> readRef (finalRef res)
         pure ( Succeeded ctx
                { cThreads    = goto (AStopSub (c out)) tid (cThreads ctx)
                , cIdSource   = cIdSource (finalContext res)
                , cSchedState = cSchedState (finalContext res)
                }
              , SubC (finalTrace res) (finalDecision res)
              , const (pure ())
              )

-- after the end of a subconcurrent computation. does nothing, only
-- exists so that: there is an entry in the trace for returning to
-- normal computation; and every item in the trace corresponds to a
-- scheduling point.
stepThread _ _ _ _ tid (AStopSub c) = \ctx@Context{..} ->
  pure ( Succeeded ctx { cThreads = goto c tid cThreads }
       , Single StopSubconcurrency
       , const (pure ())
       )

-- run an action atomically, with a non-preemptive length bounded
-- round robin scheduler, under sequential consistency.
stepThread forSnapshot isFirst _ _ tid (ADontCheck lb ma c) = \ctx ->
  if | isFirst -> do
         -- create a restricted context
         threads' <- kill tid (cThreads ctx)
         let dcCtx = ctx { cThreads = threads', cSchedState = lb }
         res <- runConcurrency' forSnapshot dcSched SequentialConsistency dcCtx ma
         out <- efromJust "stepThread.ADontCheck" <$> readRef (finalRef res)
         case out of
           Right a -> do
             let threads'' = launch' Unmasked tid (const (c a)) (cThreads (finalContext res))
             threads''' <- (if rtsSupportsBoundThreads then makeBound tid else pure) threads''
             pure ( (if forSnapshot then Snap else Succeeded) (finalContext res)
                    { cThreads = threads''', cSchedState = cSchedState ctx }
                  , Single (DontCheck (toList (finalTrace res)))
                  , fromMaybe (const (pure ())) (finalRestore res)
                  )
           Left f -> pure
             ( Failed f
             , Single (DontCheck (toList (finalTrace res)))
             , const (pure ())
             )
     | otherwise -> pure
       ( Failed IllegalDontCheck
       , Single (DontCheck [])
       , const (pure ())
       )

-- | Handle an exception being thrown from an @AAtom@, @AThrow@, or
-- @AThrowTo@.
stepThrow :: (MonadConc n, MonadRef r n, Exception e)
  => ThreadAction
  -- ^ Action to include in the trace.
  -> ThreadId
  -- ^ The thread receiving the exception.
  -> e
  -- ^ Exception to raise.
  -> Context n r g
  -- ^ The execution context.
  -> n (What n r g, Act, Threads n r -> n ())
stepThrow act tid e ctx@Context{..} = case propagate some tid cThreads of
    Just ts' -> pure
      ( Succeeded ctx { cThreads = ts' }
      , Single act
      , const (pure ())
      )
    Nothing
      | tid == initialThread -> pure
        ( Failed (UncaughtException some)
        , Single act
        , const (pure ())
        )
      | otherwise -> do
          ts' <- kill tid cThreads
          pure ( Succeeded ctx { cThreads = ts' }
               , Single act
               , const (pure ())
               )
  where
    some = toException e

-- | Helper for actions impose a write barrier.
synchronised :: (Monad n, MonadRef r n)
  => (Context n r g -> n (What n r g, Act, Threads n r -> n ()))
  -- ^ Action to run after the write barrier.
  -> Context n r g
  -- ^ The original execution context.
  -> n (What n r g, Act, Threads n r -> n ())
synchronised ma ctx@Context{..} = do
  writeBarrier cWriteBuf
  ma ctx { cWriteBuf = emptyBuffer }

-- | scheduler for @ADontCheck@
dcSched :: Scheduler (Maybe Int)
dcSched = Scheduler go where
  go _ _ (Just 0) = (Nothing, Just 0)
  go prior threads s =
    let (t, _) = scheduleThread roundRobinSchedNP prior threads ()
    in (t, fmap (\lb -> lb - 1) s)

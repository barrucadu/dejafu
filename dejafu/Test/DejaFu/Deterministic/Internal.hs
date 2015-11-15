{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Concurrent monads with a fixed scheduler: internal types and
-- functions.
module Test.DejaFu.Deterministic.Internal
 ( -- * Execution
   runFixed
 , runFixed'

 -- * The @Conc@ Monad
 , M(..)
 , CVar(..)
 , CRef(..)
 , Ticket(..)
 , Fixed
 , cont
 , runCont

 -- * Primitive Actions
 , Action(..)

 -- * Identifiers
 , ThreadId
 , CVarId
 , CRefId

 -- * Memory Models
 , MemType(..)

 -- * Scheduling & Traces
 , Scheduler
 , Trace
 , Decision(..)
 , ThreadAction(..)
 , Lookahead(..)
 , Trace'
 , toTrace
 , showTrace
 , showFail

 -- * Synchronised and Unsynchronised Actions
 , ActionType(..)
 , isBarrier
 , synchronises
 , crefOf
 , cvarOf
 , simplify
 , simplify'

 -- * Failures
 , Failure(..)
 ) where

import Control.Exception (MaskingState(..), SomeException(..))
import Data.List (sort)
import Data.List.Extra
import Data.Maybe (fromJust, isJust, fromMaybe, isNothing, listToMaybe)
import Data.Typeable (cast)
import Test.DejaFu.STM (CTVarId, Result(..))
import Test.DejaFu.Internal
import Test.DejaFu.Deterministic.Internal.Common
import Test.DejaFu.Deterministic.Internal.Memory
import Test.DejaFu.Deterministic.Internal.Threading

import qualified Data.IntMap.Strict as I

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}
{-# ANN module ("HLint: ignore Use const"           :: String) #-}

--------------------------------------------------------------------------------
-- * Execution

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning a 'Just' if it terminates, and 'Nothing' if a
-- deadlock is detected. Also returned is the final state of the
-- scheduler, and an execution trace.
runFixed :: (Functor n, Monad n) => Fixed n r s -> (forall x. s x -> CTVarId -> n (Result x, CTVarId))
         -> Scheduler g -> MemType -> g -> M n r s a -> n (Either Failure a, g, Trace')
runFixed fixed runstm sched memtype s ma = (\(e,g,_,t) -> (e,g,t)) <$> runFixed' fixed runstm sched memtype s initialIdSource ma

-- | Same as 'runFixed', be parametrised by an 'IdSource'.
runFixed' :: forall n r s g a. (Functor n, Monad n)
  => Fixed n r s -> (forall x. s x -> CTVarId -> n (Result x, CTVarId))
  -> Scheduler g -> MemType -> g -> IdSource -> M n r s a -> n (Either Failure a, g, IdSource, Trace')
runFixed' fixed runstm sched memtype s idSource ma = do
  ref <- newRef fixed Nothing

  let c       = ma >>= liftN fixed . writeRef fixed ref . Just . Right
  let threads = launch' Unmasked 0 ((\a _ -> a) $ runCont c $ const AStop) I.empty

  (s', idSource', trace) <- runThreads fixed runstm sched memtype s threads idSource ref
  out <- readRef fixed ref

  return (fromJust out, s', idSource', reverse trace)

-- | Run a collection of threads, until there are no threads left.
--
-- Note: this returns the trace in reverse order, because it's more
-- efficient to prepend to a list than append. As this function isn't
-- exposed to users of the library, this is just an internal gotcha to
-- watch out for.
runThreads :: (Functor n, Monad n) => Fixed n r s -> (forall x. s x -> CTVarId -> n (Result x, CTVarId))
           -> Scheduler g -> MemType -> g -> Threads n r s -> IdSource -> r (Maybe (Either Failure a)) -> n (g, IdSource, Trace')
runThreads fixed runstm sched memtype origg origthreads idsrc ref = go idsrc [] Nothing origg origthreads emptyBuffer where
  go idSource sofar prior g threads wb
    | isTerminated  = stop
    | isDeadlocked  = die Deadlock
    | isSTMLocked   = die STMDeadlock
    | isNonexistant = die InternalError
    | isBlocked     = die InternalError
    | otherwise = do
      stepped <- stepThread fixed runstm memtype (_continuation $ fromJust thread) idSource chosen threads wb
      case stepped of
        Right (threads', idSource', act, wb') -> loop threads' idSource' act wb'

        Left UncaughtException
          | chosen == 0 -> die UncaughtException
          | otherwise -> loop (kill chosen threads) idSource Killed wb

        Left failure -> die failure

    where
      (chosen, g')  = sched g ((\p (_,_,a) -> (p,a)) <$> prior <*> listToMaybe sofar) $ unsafeToNonEmpty runnable'
      runnable'     = [(t, nextActions t) | t <- sort $ I.keys runnable]
      runnable      = I.filter (isNothing . _blocking) threadsc
      thread        = I.lookup chosen threadsc
      threadsc      = addCommitThreads wb threads
      isBlocked     = isJust . _blocking $ fromJust thread
      isNonexistant = isNothing thread
      isTerminated  = 0 `notElem` I.keys threads
      isDeadlocked  = isLocked 0 threads && (((~= OnCVarFull  undefined) <$> I.lookup 0 threads) == Just True ||
                                           ((~=  OnCVarEmpty undefined) <$> I.lookup 0 threads) == Just True ||
                                           ((~=  OnMask      undefined) <$> I.lookup 0 threads) == Just True)
      isSTMLocked   = isLocked 0 threads && ((~=  OnCTVar    []) <$> I.lookup 0 threads) == Just True

      unblockWaitingOn tid = fmap unblock where
        unblock thrd = case _blocking thrd of
          Just (OnMask t) | t == tid -> thrd { _blocking = Nothing }
          _ -> thrd

      decision
        | Just chosen == prior = Continue
        | prior `notElem` map (Just . fst) runnable' = Start chosen
        | otherwise = SwitchTo chosen

      alternatives
        | Just chosen == prior = [(SwitchTo t, na) | (t, na) <- runnable', Just t /= prior]
        | prior `notElem` map (Just . fst) runnable' = [(Start t, na) | (t, na) <- runnable', t /= chosen]
        | otherwise = [(if Just t == prior then Continue else SwitchTo t, na) | (t, na) <- runnable', t /= chosen]

      nextActions t = lookahead . _continuation . fromJust $ I.lookup t threadsc

      stop = return (g, idSource, sofar)
      die reason = writeRef fixed ref (Just $ Left reason) >> stop

      loop threads' idSource' act wb' =
        let sofar' = ((decision, alternatives, act) : sofar)
            threads'' = if (interruptible <$> I.lookup chosen threads') /= Just False then unblockWaitingOn chosen threads' else threads'
        in go idSource' sofar' (Just chosen) g' (delCommitThreads threads'') wb'

--------------------------------------------------------------------------------
-- * Single-step execution

-- | Run a single thread one step, by dispatching on the type of
-- 'Action'.
stepThread :: forall n r s. (Functor n, Monad n) => Fixed n r s
  -> (forall x. s x -> CTVarId -> n (Result x, CTVarId))
  -- ^ Run a 'MonadSTM' transaction atomically.
  -> MemType
  -- ^ The memory model
  -> Action n r s
  -- ^ Action to step
  -> IdSource
  -- ^ Source of fresh IDs
  -> ThreadId
  -- ^ ID of the current thread
  -> Threads n r s
  -- ^ Current state of threads
  -> WriteBuffer r
  -- ^ @CRef@ write buffer
  -> n (Either Failure (Threads n r s, IdSource, ThreadAction, WriteBuffer r))
stepThread fixed runstm memtype action idSource tid threads wb = case action of
  AFork    a b     -> stepFork        a b
  AMyTId   c       -> stepMyTId       c
  AYield   c       -> stepYield       c
  ANewVar  c       -> stepNewVar      c
  APutVar  var a c -> stepPutVar      var a c
  ATryPutVar var a c -> stepTryPutVar var a c
  AReadVar var c   -> stepReadVar     var c
  ATakeVar var c   -> stepTakeVar     var c
  ATryTakeVar var c -> stepTryTakeVar var c
  ANewRef  a c     -> stepNewRef      a c
  AReadRef ref c   -> stepReadRef     ref c
  AReadRefCas ref c -> stepReadRefCas ref c
  APeekTicket tick c -> stepPeekTicket tick c
  AModRef  ref f c -> stepModRef      ref f c
  AModRefCas ref f c -> stepModRefCas ref f c
  AWriteRef ref a c -> stepWriteRef   ref a c
  ACasRef ref tick a c -> stepCasRef ref tick a c
  ACommit  t c     -> stepCommit      t c
  AAtom    stm c   -> stepAtom        stm c
  ALift    na      -> stepLift        na
  AThrow   e       -> stepThrow       e
  AThrowTo t e c   -> stepThrowTo     t e c
  ACatching h ma c -> stepCatching    h ma c
  APopCatching a   -> stepPopCatching a
  AMasking m ma c  -> stepMasking     m ma c
  AResetMask b1 b2 m c -> stepResetMask b1 b2 m c
  AReturn     c    -> stepReturn c
  AKnowsAbout v c  -> stepKnowsAbout  v c
  AForgets    v c  -> stepForgets v c
  AAllKnown   c    -> stepAllKnown c
  AStop            -> stepStop

  where
    -- | Start a new thread, assigning it the next 'ThreadId'
    stepFork a b = return $ Right (goto (b newtid) tid threads', idSource', Fork newtid, wb) where
      threads' = launch tid newtid a threads
      (idSource', newtid) = nextTId idSource

    -- | Get the 'ThreadId' of the current thread
    stepMyTId c = simple (goto (c tid) tid threads) MyThreadId

    -- | Yield the current thread
    stepYield c = simple (goto c tid threads) Yield

    -- | Put a value into a @CVar@, blocking the thread until it's
    -- empty.
    stepPutVar cvar@(CVar (cvid, _)) a c = synchronised $ do
      (success, threads', woken) <- putIntoCVar cvar a c fixed tid threads
      simple threads' $ if success then PutVar cvid woken else BlockedPutVar cvid

    -- | Try to put a value into a @CVar@, without blocking.
    stepTryPutVar cvar@(CVar (cvid, _)) a c = synchronised $ do
      (success, threads', woken) <- tryPutIntoCVar cvar a c fixed tid threads
      simple threads' $ TryPutVar cvid success woken

    -- | Get the value from a @CVar@, without emptying, blocking the
    -- thread until it's full.
    stepReadVar cvar@(CVar (cvid, _)) c = synchronised $ do
      (success, threads', _) <- readFromCVar cvar c fixed tid threads
      simple threads' $ if success then ReadVar cvid else BlockedReadVar cvid

    -- | Take the value from a @CVar@, blocking the thread until it's
    -- full.
    stepTakeVar cvar@(CVar (cvid, _)) c = synchronised $ do
      (success, threads', woken) <- takeFromCVar cvar c fixed tid threads
      simple threads' $ if success then TakeVar cvid woken else BlockedTakeVar cvid

    -- | Try to take the value from a @CVar@, without blocking.
    stepTryTakeVar cvar@(CVar (cvid, _)) c = synchronised $ do
      (success, threads', woken) <- tryTakeFromCVar cvar c fixed tid threads
      simple threads' $ TryTakeVar cvid success woken

    -- | Read from a @CRef@.
    stepReadRef cref@(CRef (crid, _)) c = do
      val <- readCRef fixed cref tid
      simple (goto (c val) tid threads) $ ReadRef crid

    -- | Read from a @CRef@ for future compare-and-swap operations.
    stepReadRefCas cref@(CRef (crid, _)) c = do
      tick <- readForTicket fixed cref tid
      simple (goto (c tick) tid threads) $ ReadRefCas crid

    -- | Extract the value from a @Ticket@.
    stepPeekTicket (Ticket (crid, _, a)) c = simple (goto (c a) tid threads) $ PeekTicket crid

    -- | Modify a @CRef@.
    stepModRef cref@(CRef (crid, _)) f c = synchronised $ do
      (new, val) <- f <$> readCRef fixed cref tid
      writeImmediate fixed cref new
      simple (goto (c val) tid threads) $ ModRef crid

    -- | Modify a @CRef@ using a compare-and-swap.
    stepModRefCas cref@(CRef (crid, _)) f c = synchronised $ do
      tick@(Ticket (_, _, old)) <- readForTicket fixed cref tid
      let (new, val) = f old
      casCRef fixed cref tid tick new
      simple (goto (c val) tid threads) $ ModRefCas crid

    -- | Write to a @CRef@ without synchronising
    stepWriteRef cref@(CRef (crid, _)) a c = case memtype of
      -- Write immediately.
      SequentialConsistency -> do
        writeImmediate fixed cref a
        simple (goto c tid threads) $ WriteRef crid

      -- Add to buffer using thread id.
      TotalStoreOrder -> do
        wb' <- bufferWrite fixed wb tid cref a tid
        return $ Right (goto c tid threads, idSource, WriteRef crid, wb')

      -- Add to buffer using cref id
      PartialStoreOrder -> do
        wb' <- bufferWrite fixed wb crid cref a tid
        return $ Right (goto c tid threads, idSource, WriteRef crid, wb')

    -- | Perform a compare-and-swap on a @CRef@.
    stepCasRef cref@(CRef (crid, _)) tick a c = synchronised $ do
      (suc, tick') <- casCRef fixed cref tid tick a
      simple (goto (c (suc, tick')) tid threads) $ CasRef crid suc

    -- | Commit a @CRef@ write
    stepCommit c t = do
      wb' <- case memtype of
        -- Shouldn't ever get here
        SequentialConsistency ->
          error "Attempting to commit under SequentialConsistency"

        -- Commit using the thread id.
        TotalStoreOrder -> commitWrite fixed wb t

        -- Commit using the cref id.
        PartialStoreOrder -> commitWrite fixed wb c

      return $ Right (threads, idSource, CommitRef t c, wb')

    -- | Run a STM transaction atomically.
    stepAtom stm c = synchronised $ do
      let oldctvid = _nextCTVId idSource
      (res, newctvid) <- runstm stm oldctvid
      case res of
        Success readen written val
          | any (<oldctvid) readen || any (<oldctvid) written ->
            let (threads', woken) = wake (OnCTVar written) threads
            in return $ Right (knows (map Right written) tid $ goto (c val) tid threads', idSource { _nextCTVId = newctvid }, STM woken, wb)
          | otherwise ->
           return $ Right (knows (map Right written) tid $ goto (c val) tid threads, idSource { _nextCTVId = newctvid }, FreshSTM, wb)
        Retry touched ->
          let threads' = block (OnCTVar touched) tid threads
          in return $ Right (threads', idSource { _nextCTVId = newctvid }, BlockedSTM, wb)
        Exception e -> stepThrow e

    -- | Run a subcomputation in an exception-catching context.
    stepCatching h ma c = simple threads' Catching where
      a     = runCont ma      (APopCatching . c)
      e exc = runCont (h exc) (APopCatching . c)

      threads' = goto a tid (catching e tid threads)

    -- | Pop the top exception handler from the thread's stack.
    stepPopCatching a = simple threads' PopCatching where
      threads' = goto a tid (uncatching tid threads)

    -- | Throw an exception, and propagate it to the appropriate
    -- handler.
    stepThrow e =
      case propagate (wrap e) tid threads of
        Just threads' -> simple threads' Throw
        Nothing -> return $ Left UncaughtException

    -- | Throw an exception to the target thread, and propagate it to
    -- the appropriate handler.
    stepThrowTo t e c = synchronised $
      let threads' = goto c tid threads
          blocked  = block (OnMask t) tid threads
      in case I.lookup t threads of
           Just thread
             | interruptible thread -> case propagate (wrap e) t threads' of
               Just threads'' -> simple threads'' $ ThrowTo t
               Nothing
                 | t == 0     -> return $ Left UncaughtException
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
                -> ((forall b. M n r s b -> M n r s b) -> M n r s a)
                -> (a -> Action n r s)
                -> n (Either Failure (Threads n r s, IdSource, ThreadAction, WriteBuffer r))
    stepMasking m ma c = simple threads' $ SetMasking False m where
      a = runCont (ma umask) (AResetMask False False m' . c)

      m' = _masking . fromJust $ I.lookup tid threads
      umask mb = resetMask True m' >> mb >>= \b -> resetMask False m >> return b
      resetMask typ ms = cont $ \k -> AResetMask typ True ms $ k ()

      threads' = goto a tid (mask m tid threads)

    -- | Reset the masking thread of the state.
    stepResetMask b1 b2 m c = simple threads' action where
      action   = (if b1 then SetMasking else ResetMasking) b2 m
      threads' = goto c tid (mask m tid threads)

    -- | Create a new @CVar@, using the next 'CVarId'.
    stepNewVar c = do
      let (idSource', newcvid) = nextCVId idSource
      ref <- newRef fixed Nothing
      let cvar = CVar (newcvid, ref)
      return $ Right (knows [Left newcvid] tid $ goto (c cvar) tid threads, idSource', NewVar newcvid, wb)

    -- | Create a new @CRef@, using the next 'CRefId'.
    stepNewRef a c = do
      let (idSource', newcrid) = nextCRId idSource
      ref <- newRef fixed (I.empty, 0, a)
      let cref = CRef (newcrid, ref)
      return $ Right (goto (c cref) tid threads, idSource', NewRef newcrid, wb)

    -- | Lift an action from the underlying monad into the @Conc@
    -- computation.
    stepLift na = do
      a <- na
      simple (goto a tid threads) Lift

    -- | Execute a 'return' or 'pure'.
    stepReturn c = simple (goto c tid threads) Return

    -- | Record that a variable is known about.
    stepKnowsAbout v c = simple (knows [v] tid $ goto c tid threads) KnowsAbout

    -- | Record that a variable will never be touched again.
    stepForgets v c = simple (forgets [v] tid $ goto c tid threads) Forgets

    -- | Record that all shared variables are known.
    stepAllKnown c = simple (fullknown tid $ goto c tid threads) AllKnown

    -- | Kill the current thread.
    stepStop = simple (kill tid threads) Stop

    -- | Helper for actions which don't touch the 'IdSource' or
    -- 'WriteBuffer'
    simple threads' act = return $ Right (threads', idSource, act, wb)

    -- | Helper for actions impose a write barrier.
    synchronised ma = do
      writeBarrier fixed wb
      res <- ma

      return $ case res of
        Right (threads', idSource', act', _) -> Right (threads', idSource', act', emptyBuffer)
        _ -> res

    -- | Helper function for wrapping up exceptions.
    wrap e = fromMaybe (SomeException e) $ cast e

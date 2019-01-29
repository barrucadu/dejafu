{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- GHC doesn't need this to compile the module, but stylish-haskell
-- does to format it.
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal.Program
-- Copyright   : (c) 2019 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, FlexibleContexts, FlexibleInstances, GADTs, LambdaCase, RecordWildCards, TypeFamilies
--
-- Representations of concurrent programs with setup, teardown, and
-- snapshotting.  This module is NOT considered to form part of the
-- public interface of this library.
--
-- This module defines orphan instances for the 'Program' type which
-- lives in "Test.DejaFu.Conc.Internal.Common", to avoid needing to
-- pull a bunch more stuff into that module.
module Test.DejaFu.Conc.Internal.Program where

import           Control.Applicative                 (Applicative(..))
import           Control.Exception                   (MaskingState(..))
import qualified Control.Monad.Catch                 as Ca
import qualified Control.Monad.IO.Class              as IO
import           Control.Monad.Trans.Class           (MonadTrans(..))
import qualified Data.Foldable                       as F
import           Data.List                           (partition)
import qualified Data.Map.Strict                     as M
import           Data.Maybe                          (isNothing)
import           GHC.Stack                           (HasCallStack)

import qualified Control.Monad.Conc.Class            as C
import           Test.DejaFu.Conc.Internal
import           Test.DejaFu.Conc.Internal.Common
import           Test.DejaFu.Conc.Internal.STM       (ModelSTM)
import           Test.DejaFu.Conc.Internal.Threading (Threads, _blocking)
import           Test.DejaFu.Internal
import           Test.DejaFu.Schedule
import           Test.DejaFu.Types

-------------------------------------------------------------------------------
-- Expressing concurrent programs

instance (pty ~ Basic, IO.MonadIO n) => IO.MonadIO (Program pty n) where
  liftIO ma = ModelConc (\c -> ALift (fmap c (IO.liftIO ma)))

instance (pty ~ Basic) => MonadTrans (Program pty) where
  lift ma = ModelConc (\c -> ALift (fmap c ma))

instance (pty ~ Basic) =>  Ca.MonadCatch (Program pty n) where
  catch ma h = ModelConc (ACatching h ma)

instance (pty ~ Basic) => Ca.MonadThrow (Program pty n) where
  throwM e = ModelConc (\_ -> AThrow e)

instance (pty ~ Basic) => Ca.MonadMask (Program pty n) where
  mask                mb = ModelConc (AMasking MaskedInterruptible   (\f -> mb f))
  uninterruptibleMask mb = ModelConc (AMasking MaskedUninterruptible (\f -> mb f))

#if MIN_VERSION_exceptions(0,10,0)
  generalBracket acquire release use = Ca.mask $ \unmasked -> do
    resource <- acquire
    b <- unmasked (use resource) `Ca.catch` (\e -> release resource (Ca.ExitCaseException e) >> Ca.throwM e)
    c <- release resource (Ca.ExitCaseSuccess b)
    pure (b, c)
#elif MIN_VERSION_exceptions(0,9,0)
  -- from https://github.com/fpco/stackage/issues/3315#issuecomment-368583481
  generalBracket acquire release cleanup use = Ca.mask $ \unmasked -> do
    resource <- acquire
    result <- unmasked (use resource) `Ca.catch` (\e -> cleanup resource e >> Ca.throwM e)
    _ <- release resource
    pure result
#endif

instance (pty ~ Basic, Monad n) => C.MonadConc (Program pty n) where
  type MVar     (Program pty n) = ModelMVar n
  type IORef    (Program pty n) = ModelIORef n
  type Ticket   (Program pty n) = ModelTicket
  type STM      (Program pty n) = ModelSTM n
  type ThreadId (Program pty n) = ThreadId

  -- ----------

  forkWithUnmaskN   n ma = ModelConc (AFork n (\umask -> runModelConc (ma umask) (\_ -> AStop (pure ()))))
  forkOnWithUnmaskN n _  = C.forkWithUnmaskN n
  forkOSWithUnmaskN n ma
    | C.rtsSupportsBoundThreads =
      ModelConc (AForkOS n (\umask -> runModelConc (ma umask) (\_ -> AStop (pure ()))))
    | otherwise = fail "RTS doesn't support multiple OS threads (use ghc -threaded when linking)"

  isCurrentThreadBound = ModelConc AIsBound

  -- This implementation lies and returns 2 until a value is set. This
  -- will potentially avoid special-case behaviour for 1 capability,
  -- so it seems a sane choice.
  getNumCapabilities      = ModelConc AGetNumCapabilities
  setNumCapabilities caps = ModelConc (\c -> ASetNumCapabilities caps (c ()))

  myThreadId = ModelConc AMyTId

  yield = ModelConc (\c -> AYield (c ()))
  threadDelay n = ModelConc (\c -> ADelay n (c ()))

  -- ----------

  newIORefN n a = ModelConc (ANewIORef n a)

  readIORef   ref = ModelConc (AReadIORef    ref)
  readForCAS ref = ModelConc (AReadIORefCas ref)

  peekTicket' _ = ticketVal

  writeIORef ref      a = ModelConc (\c -> AWriteIORef ref a (c ()))
  casIORef   ref tick a = ModelConc (ACasIORef ref tick a)

  atomicModifyIORef ref f = ModelConc (AModIORef    ref f)
  modifyIORefCAS    ref f = ModelConc (AModIORefCas ref f)

  -- ----------

  newEmptyMVarN n = ModelConc (ANewMVar n)

  putMVar  var a = ModelConc (\c -> APutMVar var a (c ()))
  readMVar var   = ModelConc (AReadMVar var)
  takeMVar var   = ModelConc (ATakeMVar var)

  tryPutMVar  var a = ModelConc (ATryPutMVar  var a)
  tryReadMVar var   = ModelConc (ATryReadMVar var)
  tryTakeMVar var   = ModelConc (ATryTakeMVar var)

  -- ----------

  throwTo tid e = ModelConc (\c -> AThrowTo tid e (c ()))

  -- ----------

  atomically = ModelConc . AAtom

-------------------------------------------------------------------------------
-- Executing concurrent programs

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning either the final result or the condition which
-- prevented that. Also returned is the final state of the scheduler,
-- and an execution trace.
--
-- If the RTS supports bound threads (ghc -threaded when linking) then
-- the main thread of the concurrent computation will be bound, and
-- @forkOS@ / @forkOSN@ will work during execution.  If not, then the
-- main thread will not be found, and attempting to fork a bound
-- thread will raise an error.
--
-- __Warning:__ Blocking on the action of another thread in 'liftIO'
-- cannot be detected! So if you perform some potentially blocking
-- action in a 'liftIO' the entire collection of threads may deadlock!
-- You should therefore keep @IO@ blocks small, and only perform
-- blocking operations with the supplied primitives, insofar as
-- possible.
--
-- __Note:__ In order to prevent computation from hanging, the runtime
-- will assume that a deadlock situation has arisen if the scheduler
-- attempts to (a) schedule a blocked thread, or (b) schedule a
-- nonexistent thread. In either of those cases, the computation will
-- be halted.
--
-- @since unreleased
runConcurrent :: C.MonadConc n
  => Scheduler s
  -> MemType
  -> s
  -> Program pty n a
  -> n (Either Condition a, s, Trace)
runConcurrent sched memtype s ma@(ModelConc _) = do
  res <- runConcurrency False sched memtype s initialIdSource 2 ma
  out <- efromJust <$> C.readIORef (finalRef res)
  pure ( out
       , cSchedState (finalContext res)
       , F.toList (finalTrace res)
       )
runConcurrent sched memtype s ma = recordSnapshot ma >>= \case
  Just (Left cond, trc) -> pure (Left cond, s, trc)
  Just (Right snap, _)  -> runSnapshot sched memtype s snap
  Nothing -> fatal "failed to record snapshot!"

-- | Runs any setup action and returns a 'Snapshot' which can be
-- passed to 'runSnapshot'.  If there is no setup action (this is a
-- @Program Basic@, then @Nothing@ is returned.  The snapshot captures
-- the state at the end of the setup, so the full program can be run
-- multiple times without repeating the setup.
--
-- The setup action is executed atomically with a deterministic
-- scheduler under sequential consistency.  Any forked threads
-- continue to exist in the main program.
--
-- If the setup action does not successfully produce a value
-- (deadlock, uncaught exception, etc), no snapshot is produced.
--
-- __Snapshotting @IO@:__ A snapshot captures entire state of your
-- concurrent program: the state of every thread, the number of
-- capabilities, the values of any @IORef@s, @MVar@s, and @TVar@s, and
-- records any @IO@ that you performed.
--
-- When restoring a snapshot this @IO@ is replayed, in order.  But the
-- whole snapshotted computation is not.  So the effects of the @IO@
-- take place again, but any return values are ignored.  For example,
-- this program will not do what you want:
--
-- @
-- bad_snapshot = withSetup
--   (do r <- liftIO (newIORef 0)
--       liftIO (modifyIORef r (+1))
--       pure r)
--   (liftIO . readIORef)
-- @
--
-- When the snapshot is taken, the value in the @IORef@ will be 1.
-- When the snapshot is restored for the first time, those @IO@
-- actions will be run again, /but their return values will be
-- discarded/.  The value in the @IORef@ will be 2.  When the snapshot
-- is restored for the second time, the value in the @IORef@ will be
-- 3.  And so on.
--
-- To safely use @IO@ in a snapshotted computation, __the combined
-- effect must be idempotent__.  You should either use actions which
-- set the state to the final value directly, rather than modifying it
-- (eg, using a combination of @liftIO . readIORef@ and @liftIO
-- . writeIORef@ here), or reset the state to a known value.  Both of
-- these approaches will work:
--
-- @
-- good_snapshot1 = withSetup
--   (do let modify r f = liftIO (readIORef r) >>= liftIO . writeIORef r . f
--        r <- liftIO (newIORef 0)
--        modify r (+1)
--        pure r)
--   (liftIO . readIORef)
--
-- good_snapshot2 = withSetup
--   (do r <- liftIO (newIORef 0)
--       liftIO (writeIORef r 0)
--       liftIO (modifyIORef r (+1))
--       pure r)
--   (liftIO . readIORef)
-- @
--
-- @since unreleased
recordSnapshot
  :: C.MonadConc n
  => Program pty n a
  -> n (Maybe (Either Condition (Snapshot pty n a), Trace))
recordSnapshot ModelConc{..} = pure Nothing
recordSnapshot WithSetup{..} =
  let mkSnapshot snap _ = WS snap
  in defaultRecordSnapshot mkSnapshot wsSetup wsProgram
recordSnapshot WithSetupAndTeardown{..} =
  let mkSnapshot snap = WSAT snap . wstTeardown
  in defaultRecordSnapshot mkSnapshot wstSetup wstProgram

-- | Runs a program with snapshotted setup to completion.
--
-- @since unreleased
runSnapshot
  :: C.MonadConc n
  => Scheduler s
  -> MemType
  -> s
  -> Snapshot pty n a
  -> n (Either Condition a, s, Trace)
runSnapshot sched memtype s (WS SimpleSnapshot{..}) = do
  let context = snapContext { cSchedState = s }
  CResult{..} <- runConcurrencyWithSnapshot sched memtype context snapRestore snapNext
  out <- efromJust <$> C.readIORef finalRef
  pure ( out
       , cSchedState finalContext
       , F.toList finalTrace
       )
runSnapshot sched memtype s (WSAT SimpleSnapshot{..} teardown) = do
  let context = snapContext { cSchedState = s }
  intermediateResult <- runConcurrencyWithSnapshot sched memtype context snapRestore snapNext
  let idsrc = cIdSource (finalContext intermediateResult)
  out1 <- efromJust <$> C.readIORef (finalRef intermediateResult)
  teardownResult <- simpleRunConcurrency False idsrc (teardown out1)
  out2 <- efromJust <$> C.readIORef (finalRef teardownResult)
  pure ( out2
       , cSchedState (finalContext intermediateResult)
       , F.toList (finalTrace intermediateResult)
       )

-------------------------------------------------------------------------------
-- Snapshotting

-- | A record of the state of a concurrent program immediately after
-- completing the setup action.
--
-- @since unreleased
data Snapshot pty n a where
  WS   :: SimpleSnapshot n a -> Snapshot (WithSetup x) n a
  WSAT :: SimpleSnapshot n a -> (Either Condition a -> ModelConc n y) -> Snapshot (WithSetupAndTeardown x a) n y

data SimpleSnapshot n a = SimpleSnapshot
  { snapContext :: Context n ()
  , snapRestore :: Threads n -> n ()
  , snapNext    :: ModelConc n a
  }

-- | Get the 'Context' from a 'Snapshot'.
contextFromSnapshot :: Snapshot p n a -> Context n ()
contextFromSnapshot (WS SimpleSnapshot{..})     = snapContext
contextFromSnapshot (WSAT SimpleSnapshot{..} _) = snapContext

-- | Get the threads which exist in a snapshot, partitioned into
-- runnable and not runnable.
threadsFromSnapshot :: Snapshot p n a -> ([ThreadId], [ThreadId])
threadsFromSnapshot snap = (initialThread : runnable, blocked) where
  (runnable, blocked) = partition isRunnable (M.keys threads)
  threads = cThreads (contextFromSnapshot snap)
  isRunnable tid = isNothing (_blocking =<< M.lookup tid threads)

-- | 'recordSnapshot' implemented generically.
--
-- Throws an error if the snapshot could not be produced.
defaultRecordSnapshot :: C.MonadConc n
  => (SimpleSnapshot n a -> x -> snap)
  -> ModelConc n x
  -> (x -> ModelConc n a)
  -> n (Maybe (Either Condition snap, Trace))
defaultRecordSnapshot mkSnapshot setup program = do
  CResult{..} <- simpleRunConcurrency True initialIdSource setup
  let trc = F.toList finalTrace
  out <- C.readIORef finalRef
  pure . Just $ case out of
    Just (Right a) ->
      let snap = mkSnapshot (SimpleSnapshot finalContext finalRestore (program a)) a
      in (Right snap, trc)
    Just (Left f) -> (Left f, trc)
    -- alternative behaviour: return @Nothing@ here.  but this should
    -- never fail, so it should be an error if it does.
    Nothing -> fatal "failed to produce snapshot"

-------------------------------------------------------------------------------
-- Utilities

-- | Run a concurrent program with a deterministic scheduler in
-- snapshotting or non-snapshotting mode.
simpleRunConcurrency ::(C.MonadConc n, HasCallStack)
  => Bool
  -> IdSource
  -> ModelConc n a
  -> n (CResult n () a)
simpleRunConcurrency forSnapshot idsrc =
  runConcurrency forSnapshot roundRobinSchedNP SequentialConsistency () idsrc 2

wrap :: (((a -> Action n) -> Action n) -> ((a -> Action n) -> Action n)) -> ModelConc n a -> ModelConc n a
wrap f = ModelConc . f . runModelConc

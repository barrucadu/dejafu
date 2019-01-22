{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal.Program
-- Copyright   : (c) 2019 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : FlexibleContexts, GADTs, LambdaCase, RecordWildCards
--
-- Representations of concurrent programs with setup, teardown, and
-- snapshotting.  This module is NOT considered to form part of the
-- public interface of this library.
module Test.DejaFu.Conc.Internal.Program where

import qualified Data.Foldable                       as F
import           Data.List                           (partition)
import qualified Data.Map.Strict                     as M
import           Data.Maybe                          (isNothing)
import           GHC.Stack                           (HasCallStack)

import qualified Control.Monad.Conc.Class            as C
import           Test.DejaFu.Conc.Internal
import           Test.DejaFu.Conc.Internal.Common
import           Test.DejaFu.Conc.Internal.Threading (Threads, _blocking)
import           Test.DejaFu.Internal
import           Test.DejaFu.Schedule
import           Test.DejaFu.Types

-------------------------------------------------------------------------------
-- Setup & teardown

-- | A concurrent program with some set-up action.
--
-- In terms of results, this is the same as @setup >>= program@.
-- However, the setup action will be __snapshotted__ (see
-- 'recordSnapshot' and 'runSnapshot') by the testing functions.  This
-- means that even if dejafu runs this program many many times, the
-- setup action will only be run the first time, and its effects
-- remembered for subsequent executions.
--
-- @since unreleased
data WithSetup x n a = WithSetup
  { wsSetup   :: ModelConc n x
  , wsProgram :: x -> ModelConc n a
  }

-- | A concurrent program with some set-up and teardown actions.
--
-- This is similar to
--
-- @
-- do
--   x <- setup
--   y <- program x
--   teardown x y
-- @
--
-- But with two differences:
--
--   * The setup action can be __snapshotted__, as described for
--     'WithSetup'
--
--   * The teardown action will be executed even if the main action
--     fails to produce a value.
--
-- @since unreleased
data WithSetupAndTeardown x a n y = WithSetupAndTeardown
  { wstSetup    :: ModelConc n x
  , wstProgram  :: x -> ModelConc n a
  , wstTeardown :: x -> Either Condition a -> ModelConc n y
  }

-------------------------------------------------------------------------------
-- Programs

-- | A representation of a concurrent program for testing.
--
-- To construct these, see 'Test.DejaFu.Conc.basic',
-- 'Test.DejaFu.Conc.withSetup' and
-- 'Test.DejaFu.Conc.withSetupAndTeardown'.
--
-- @since unreleased
class Program p where
  -- | Run a concurrent computation with a given 'Scheduler' and
  -- initial state, returning either the final result or the condition
  -- which prevented that. Also returned is the final state of the
  -- scheduler, and an execution trace.
  --
  -- If the RTS supports bound threads (ghc -threaded when linking)
  -- then the main thread of the concurrent computation will be bound,
  -- and @forkOS@ / @forkOSN@ will work during execution.  If not,
  -- then the main thread will not be found, and attempting to fork a
  -- bound thread will raise an error.
  --
  -- __Warning:__ Blocking on the action of another thread in 'liftIO'
  -- cannot be detected! So if you perform some potentially blocking
  -- action in a 'liftIO' the entire collection of threads may
  -- deadlock!  You should therefore keep @IO@ blocks small, and only
  -- perform blocking operations with the supplied primitives, insofar
  -- as possible.
  --
  -- __Note:__ In order to prevent computation from hanging, the
  -- runtime will assume that a deadlock situation has arisen if the
  -- scheduler attempts to (a) schedule a blocked thread, or (b)
  -- schedule a nonexistent thread. In either of those cases, the
  -- computation will be halted.
  --
  -- @since unreleased
  runConcurrent :: C.MonadConc n
    => Scheduler s
    -> MemType
    -> s
    -> p n a
    -> n (Either Condition a, s, Trace)

  -- | Runs any setup action and returns a 'Snapshot' which can be
  -- passed to 'runSnapshot'.  The snapshot captures the state at the
  -- end of the setup, so the full program can be run multiple times
  -- without repeating the setup.
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
  -- capabilities, the values of any @IORef@s, @MVar@s, and @TVar@s,
  -- and records any @IO@ that you performed.
  --
  -- When restoring a snapshot this @IO@ is replayed, in order.  But
  -- the whole snapshotted computation is not.  So the effects of the
  -- @IO@ take place again, but any return values are ignored.  For
  -- example, this program will not do what you want:
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
  -- discarded/.  The value in the @IORef@ will be 2.  When the
  -- snapshot is restored for the second time, the value in the
  -- @IORef@ will be 3.  And so on.
  --
  -- To safely use @IO@ in a snapshotted computation, __the combined
  -- effect must be idempotent__.  You should either use actions which
  -- set the state to the final value directly, rather than modifying
  -- it (eg, using a combination of @liftIO . readIORef@ and @liftIO
  -- . writeIORef@ here), or reset the state to a known value.  Both
  -- of these approaches will work:
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
    => p n a
    -> n (Maybe (Either Condition (Snapshot p n a), Trace))
  recordSnapshot _ = pure Nothing

  -- | Runs a program with snapshotted setup to completion.
  --
  -- @since unreleased
  runSnapshot
    :: C.MonadConc n
    => Scheduler s
    -> MemType
    -> s
    -> Snapshot p n a
    -> n (Either Condition a, s, Trace)
  runSnapshot _ _ _ _ = fatal "This type does not support snapshots"

instance Program (WithSetup x) where
  runConcurrent = defaultRunConcurrent

  recordSnapshot WithSetup{..} =
    let mkSnapshot snap _ = WS snap
    in defaultRecordSnapshot mkSnapshot wsSetup wsProgram

  runSnapshot sched memtype s (WS SimpleSnapshot{..}) = do
    let context = snapContext { cSchedState = s }
    CResult{..} <- runConcurrencyWithSnapshot sched memtype context snapRestore snapNext
    out <- efromJust <$> C.readIORef finalRef
    pure ( out
         , cSchedState finalContext
         , F.toList finalTrace
         )

instance Program (WithSetupAndTeardown x a) where
  runConcurrent = defaultRunConcurrent

  recordSnapshot WithSetupAndTeardown{..} =
    let mkSnapshot snap = WSAT snap . wstTeardown
    in defaultRecordSnapshot mkSnapshot wstSetup wstProgram

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
data Snapshot p n a where
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

-- | 'runConcurrent' implemented in terms of 'recordSnapshot' /
-- 'runSnapshot'.
--
-- Throws an error if 'recordSnapshot' returns @Nothing@.
defaultRunConcurrent :: (Program p, C.MonadConc n)
  => Scheduler s
  -> MemType
  -> s
  -> p n a
  -> n (Either Condition a, s, Trace)
defaultRunConcurrent sched memtype s ma = recordSnapshot ma >>= \case
  Just (Left cond, trc) -> pure (Left cond, s, trc)
  Just (Right snap, _)  -> runSnapshot sched memtype s snap
  Nothing -> fatal "failed to record snapshot!"

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

-- | Run a concurrent program with a deterministic scheduler in
-- snapshotting or non-snapshotting mode.
simpleRunConcurrency ::(C.MonadConc n, HasCallStack)
  => Bool
  -> IdSource
  -> ModelConc n a
  -> n (CResult n () a)
simpleRunConcurrency forSnapshot idsrc =
  runConcurrency forSnapshot roundRobinSchedNP SequentialConsistency () idsrc 2

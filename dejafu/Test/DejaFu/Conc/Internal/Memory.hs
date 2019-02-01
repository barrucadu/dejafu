{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal.Memory
-- Copyright   : (c) 2016--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : BangPatterns, GADTs, FlexibleContexts, LambdaCase, RecordWildCards
--
-- Operations over @IORef@s and @MVar@s. This module is NOT considered
-- to form part of the public interface of this library.
--
-- Relaxed memory operations over @IORef@s are implemented with an
-- explicit write buffer: one per thread for TSO, and one per
-- thread/variable combination for PSO. Unsynchronised writes append
-- to this buffer, and periodically separate threads commit from these
-- buffers to the \"actual\" @IORef@.
--
-- This model comes from /Dynamic Partial Order Reduction for Relaxed
-- Memory Models/, N. Zhang, M. Kusano, and C. Wang (2015).
module Test.DejaFu.Conc.Internal.Memory where

import qualified Control.Monad.Conc.Class            as C
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as M
import           Data.Maybe                          (maybeToList)
import           Data.Monoid                         ((<>))
import           Data.Sequence                       (Seq, ViewL(..), singleton,
                                                      viewl, (><))
import           GHC.Stack                           (HasCallStack)

import           Test.DejaFu.Conc.Internal.Common
import           Test.DejaFu.Conc.Internal.Threading
import           Test.DejaFu.Internal
import           Test.DejaFu.Types

--------------------------------------------------------------------------------
-- * Manipulating @IORef@s

-- | In non-sequentially-consistent memory models, non-synchronised
-- writes get buffered.
--
-- The @IORefId@ parameter is only used under PSO. Under TSO each
-- thread has a single buffer.
newtype WriteBuffer n = WriteBuffer
  { buffer :: Map (ThreadId, Maybe IORefId) (Seq (BufferedWrite n)) }

-- | A buffered write is a reference to the variable, and the value to
-- write. Universally quantified over the value type so that the only
-- thing which can be done with it is to write it to the reference.
data BufferedWrite n where
  BufferedWrite :: ThreadId -> ModelIORef n a -> a -> BufferedWrite n

-- | An empty write buffer.
emptyBuffer :: WriteBuffer n
emptyBuffer = WriteBuffer M.empty

-- | Add a new write to the end of a buffer.
bufferWrite :: C.MonadConc n => WriteBuffer n -> (ThreadId, Maybe IORefId) -> ModelIORef n a -> a -> n (WriteBuffer n)
bufferWrite (WriteBuffer wb) k@(tid, _) ref@ModelIORef{..} new = do
  -- Construct the new write buffer
  let write = singleton $ BufferedWrite tid ref new
  let buffer' = M.insertWith (flip (><)) k write wb

  -- Write the thread-local value to the @IORef@'s update map.
  (locals, count, def) <- C.readIORef iorefRef
  C.writeIORef iorefRef (M.insert tid new locals, count, def)

  pure (WriteBuffer buffer')

-- | Commit the write at the head of a buffer.
commitWrite :: C.MonadConc n => WriteBuffer n -> (ThreadId, Maybe IORefId) -> n (WriteBuffer n)
commitWrite w@(WriteBuffer wb) k = case maybe EmptyL viewl $ M.lookup k wb of
  BufferedWrite _ ref a :< rest -> do
    _ <- writeImmediate ref a
    pure . WriteBuffer $ M.insert k rest wb
  EmptyL -> pure w

-- | Read from a @IORef@, returning a newer thread-local non-committed
-- write if there is one.
readIORef :: C.MonadConc n => ModelIORef n a -> ThreadId -> n a
readIORef ref tid = do
  (val, _) <- readIORefPrim ref tid
  pure val

-- | Read from a @IORef@, returning a @Ticket@ representing the current
-- view of the thread.
readForTicket :: C.MonadConc n => ModelIORef n a -> ThreadId -> n (ModelTicket a)
readForTicket ref@ModelIORef{..} tid = do
  (val, count) <- readIORefPrim ref tid
  pure (ModelTicket iorefId count val)

-- | Perform a compare-and-swap on a @IORef@ if the ticket is still
-- valid. This is strict in the \"new\" value argument.
casIORef :: C.MonadConc n => ModelIORef n a -> ThreadId -> ModelTicket a -> a -> n (Bool, ModelTicket a, n ())
casIORef ref tid (ModelTicket _ cc _) !new = do
  tick'@(ModelTicket _ cc' _) <- readForTicket ref tid

  if cc == cc'
  then do
    effect <- writeImmediate ref new
    tick'' <- readForTicket ref tid
    pure (True, tick'', effect)
  else pure (False, tick', pure ())

-- | Read the local state of a @IORef@.
readIORefPrim :: C.MonadConc n => ModelIORef n a -> ThreadId -> n (a, Integer)
readIORefPrim ModelIORef{..} tid = do
  (vals, count, def) <- C.readIORef iorefRef
  pure (M.findWithDefault def tid vals, count)

-- | Read the global state of a @IORef@.
readIORefGlobal :: C.MonadConc n => ModelIORef n a -> n a
readIORefGlobal ModelIORef{..} = do
  (_, _, def) <- C.readIORef iorefRef
  pure def

-- | Write and commit to a @IORef@ immediately, clearing the update map
-- and incrementing the write count.
writeImmediate :: C.MonadConc n => ModelIORef n a -> a -> n (n ())
writeImmediate ModelIORef{..} a = do
  (_, count, _) <- C.readIORef iorefRef
  let effect = C.writeIORef iorefRef (M.empty, count + 1, a)
  effect
  pure effect

-- | Flush all writes in the buffer.
writeBarrier :: C.MonadConc n => WriteBuffer n -> n ()
writeBarrier (WriteBuffer wb) = mapM_ flush $ M.elems wb where
  flush = mapM_ $ \(BufferedWrite _ ref a) -> writeImmediate ref a

-- | Add phantom threads to the thread list to commit pending writes.
addCommitThreads :: WriteBuffer n -> Threads n -> Threads n
addCommitThreads (WriteBuffer wb) ts = ts <> M.fromList phantoms where
  phantoms = [ (uncurry commitThreadId k, mkthread c)
             | (k, b) <- M.toList wb
             , c <- maybeToList (go $ viewl b)
             ]
  go (BufferedWrite tid ModelIORef{..} _ :< _) = Just $ ACommit tid iorefId
  go EmptyL = Nothing

-- | The ID of a commit thread.
commitThreadId :: ThreadId -> Maybe IORefId -> ThreadId
commitThreadId (ThreadId (Id _ t)) = ThreadId . Id Nothing . negate . go where
  go (Just (IORefId (Id _ c))) = t + 1 + c * 10000
  go Nothing = t + 1

-- | Remove phantom threads.
delCommitThreads :: Threads n -> Threads n
delCommitThreads = M.filterWithKey $ \k _ -> k >= initialThread

--------------------------------------------------------------------------------
-- * Manipulating @MVar@s

-- these are a bit clearer than a Bool
data Blocking = Blocking | NonBlocking
data Emptying = Emptying | NonEmptying

-- | Put into a @MVar@, blocking if full.
putIntoMVar :: C.MonadConc n
  => ModelMVar n a
  -> a
  -> Action n
  -> ThreadId
  -> Threads n
  -> n (Bool, Threads n, [ThreadId], n ())
putIntoMVar cvar a c = mutMVar Blocking cvar a (const c)

-- | Try to put into a @MVar@, not blocking if full.
tryPutIntoMVar :: C.MonadConc n
  => ModelMVar n a
  -> a
  -> (Bool -> Action n)
  -> ThreadId
  -> Threads n
  -> n (Bool, Threads n, [ThreadId], n ())
tryPutIntoMVar = mutMVar NonBlocking

-- | Read from a @MVar@, blocking if empty.
readFromMVar :: (C.MonadConc n, HasCallStack)
  => ModelMVar n a
  -> (a -> Action n)
  -> ThreadId
  -> Threads n
  -> n (Bool, Threads n, [ThreadId], n ())
readFromMVar cvar c = seeMVar NonEmptying Blocking cvar (c . efromJust)

-- | Try to read from a @MVar@, not blocking if empty.
tryReadFromMVar :: C.MonadConc n
  => ModelMVar n a
  -> (Maybe a -> Action n)
  -> ThreadId
  -> Threads n
  -> n (Bool, Threads n, [ThreadId], n ())
tryReadFromMVar = seeMVar NonEmptying NonBlocking

-- | Take from a @MVar@, blocking if empty.
takeFromMVar :: (C.MonadConc n, HasCallStack)
  => ModelMVar n a
  -> (a -> Action n)
  -> ThreadId
  -> Threads n
  -> n (Bool, Threads n, [ThreadId], n ())
takeFromMVar cvar c = seeMVar Emptying Blocking cvar (c . efromJust)

-- | Try to take from a @MVar@, not blocking if empty.
tryTakeFromMVar :: C.MonadConc n
  => ModelMVar n a
  -> (Maybe a -> Action n)
  -> ThreadId
  -> Threads n
  -> n (Bool, Threads n, [ThreadId], n ())
tryTakeFromMVar = seeMVar Emptying NonBlocking

-- | Mutate a @MVar@, in either a blocking or nonblocking way.
mutMVar :: C.MonadConc n
  => Blocking
  -> ModelMVar n a
  -> a
  -> (Bool -> Action n)
  -> ThreadId
  -> Threads n
  -> n (Bool, Threads n, [ThreadId], n ())
mutMVar blocking ModelMVar{..} a c threadid threads = C.readIORef mvarRef >>= \case
  Just _ -> case blocking of
    Blocking ->
      let threads' = block (OnMVarEmpty mvarId) threadid threads
      in pure (False, threads', [], pure ())
    NonBlocking ->
      pure (False, goto (c False) threadid threads, [], pure ())
  Nothing -> do
    let effect = C.writeIORef mvarRef $ Just a
    let (threads', woken) = wake (OnMVarFull mvarId) threads
    effect
    pure (True, goto (c True) threadid threads', woken, effect)

-- | Read a @MVar@, in either a blocking or nonblocking
-- way.
seeMVar :: C.MonadConc n
  => Emptying
  -> Blocking
  -> ModelMVar n a
  -> (Maybe a -> Action n)
  -> ThreadId
  -> Threads n
  -> n (Bool, Threads n, [ThreadId], n ())
seeMVar emptying blocking ModelMVar{..} c threadid threads = C.readIORef mvarRef >>= \case
  val@(Just _) -> do
    let effect = case emptying of
          Emptying -> C.writeIORef mvarRef Nothing
          NonEmptying -> pure ()
    let (threads', woken) = wake (OnMVarEmpty mvarId) threads
    effect
    pure (True, goto (c val) threadid threads', woken, effect)
  Nothing -> case blocking of
    Blocking ->
      let threads' = block (OnMVarFull mvarId) threadid threads
      in pure (False, threads', [], pure ())
    NonBlocking ->
      pure (False, goto (c Nothing) threadid threads, [], pure ())

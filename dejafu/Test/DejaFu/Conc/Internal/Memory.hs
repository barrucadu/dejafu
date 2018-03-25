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
-- Operations over @CRef@s and @MVar@s. This module is NOT considered
-- to form part of the public interface of this library.
--
-- Relaxed memory operations over @CRef@s are implemented with an
-- explicit write buffer: one per thread for TSO, and one per
-- thread/variable combination for PSO. Unsynchronised writes append
-- to this buffer, and periodically separate threads commit from these
-- buffers to the \"actual\" @CRef@.
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
-- * Manipulating @CRef@s

-- | In non-sequentially-consistent memory models, non-synchronised
-- writes get buffered.
--
-- The @CRefId@ parameter is only used under PSO. Under TSO each
-- thread has a single buffer.
newtype WriteBuffer n = WriteBuffer
  { buffer :: Map (ThreadId, Maybe CRefId) (Seq (BufferedWrite n)) }

-- | A buffered write is a reference to the variable, and the value to
-- write. Universally quantified over the value type so that the only
-- thing which can be done with it is to write it to the reference.
data BufferedWrite n where
  BufferedWrite :: ThreadId -> ModelCRef n a -> a -> BufferedWrite n

-- | An empty write buffer.
emptyBuffer :: WriteBuffer n
emptyBuffer = WriteBuffer M.empty

-- | Add a new write to the end of a buffer.
bufferWrite :: C.MonadConc n => WriteBuffer n -> (ThreadId, Maybe CRefId) -> ModelCRef n a -> a -> n (WriteBuffer n)
bufferWrite (WriteBuffer wb) k@(tid, _) cref@ModelCRef{..} new = do
  -- Construct the new write buffer
  let write = singleton $ BufferedWrite tid cref new
  let buffer' = M.insertWith (flip (><)) k write wb

  -- Write the thread-local value to the @CRef@'s update map.
  (locals, count, def) <- C.readCRef crefRef
  C.writeCRef crefRef (M.insert tid new locals, count, def)

  pure (WriteBuffer buffer')

-- | Commit the write at the head of a buffer.
commitWrite :: C.MonadConc n => WriteBuffer n -> (ThreadId, Maybe CRefId) -> n (WriteBuffer n)
commitWrite w@(WriteBuffer wb) k = case maybe EmptyL viewl $ M.lookup k wb of
  BufferedWrite _ cref a :< rest -> do
    _ <- writeImmediate cref a
    pure . WriteBuffer $ M.insert k rest wb
  EmptyL -> pure w

-- | Read from a @CRef@, returning a newer thread-local non-committed
-- write if there is one.
readCRef :: C.MonadConc n => ModelCRef n a -> ThreadId -> n a
readCRef cref tid = do
  (val, _) <- readCRefPrim cref tid
  pure val

-- | Read from a @CRef@, returning a @Ticket@ representing the current
-- view of the thread.
readForTicket :: C.MonadConc n => ModelCRef n a -> ThreadId -> n (ModelTicket a)
readForTicket cref@ModelCRef{..} tid = do
  (val, count) <- readCRefPrim cref tid
  pure (ModelTicket crefId count val)

-- | Perform a compare-and-swap on a @CRef@ if the ticket is still
-- valid. This is strict in the \"new\" value argument.
casCRef :: C.MonadConc n => ModelCRef n a -> ThreadId -> ModelTicket a -> a -> n (Bool, ModelTicket a, n ())
casCRef cref tid (ModelTicket _ cc _) !new = do
  tick'@(ModelTicket _ cc' _) <- readForTicket cref tid

  if cc == cc'
  then do
    effect <- writeImmediate cref new
    tick'' <- readForTicket cref tid
    pure (True, tick'', effect)
  else pure (False, tick', pure ())

-- | Read the local state of a @CRef@.
readCRefPrim :: C.MonadConc n => ModelCRef n a -> ThreadId -> n (a, Integer)
readCRefPrim ModelCRef{..} tid = do
  (vals, count, def) <- C.readCRef crefRef
  pure (M.findWithDefault def tid vals, count)

-- | Write and commit to a @CRef@ immediately, clearing the update map
-- and incrementing the write count.
writeImmediate :: C.MonadConc n => ModelCRef n a -> a -> n (n ())
writeImmediate ModelCRef{..} a = do
  (_, count, _) <- C.readCRef crefRef
  let effect = C.writeCRef crefRef (M.empty, count + 1, a)
  effect
  pure effect

-- | Flush all writes in the buffer.
writeBarrier :: C.MonadConc n => WriteBuffer n -> n ()
writeBarrier (WriteBuffer wb) = mapM_ flush $ M.elems wb where
  flush = mapM_ $ \(BufferedWrite _ cref a) -> writeImmediate cref a

-- | Add phantom threads to the thread list to commit pending writes.
addCommitThreads :: WriteBuffer n -> Threads n -> Threads n
addCommitThreads (WriteBuffer wb) ts = ts <> M.fromList phantoms where
  phantoms = [ (uncurry commitThreadId k, mkthread c)
             | (k, b) <- M.toList wb
             , c <- maybeToList (go $ viewl b)
             ]
  go (BufferedWrite tid ModelCRef{..} _ :< _) = Just $ ACommit tid crefId
  go EmptyL = Nothing

-- | The ID of a commit thread.
commitThreadId :: ThreadId -> Maybe CRefId -> ThreadId
commitThreadId (ThreadId (Id _ t)) = ThreadId . Id Nothing . negate . go where
  go (Just (CRefId (Id _ c))) = t + 1 + c * 10000
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
mutMVar blocking ModelMVar{..} a c threadid threads = C.readCRef mvarRef >>= \case
  Just _ -> case blocking of
    Blocking ->
      let threads' = block (OnMVarEmpty mvarId) threadid threads
      in pure (False, threads', [], pure ())
    NonBlocking ->
      pure (False, goto (c False) threadid threads, [], pure ())
  Nothing -> do
    let effect = C.writeCRef mvarRef $ Just a
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
seeMVar emptying blocking ModelMVar{..} c threadid threads = C.readCRef mvarRef >>= \case
  val@(Just _) -> do
    let effect = case emptying of
          Emptying -> C.writeCRef mvarRef Nothing
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

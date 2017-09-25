{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal.Memory
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : BangPatterns, GADTs, MultiParamTypeClasses
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

import           Control.Monad.Ref                   (MonadRef, readRef,
                                                      writeRef)
import           Data.Map.Strict                     (Map)
import           Data.Maybe                          (fromJust, maybeToList)
import           Data.Monoid                         ((<>))
import           Data.Sequence                       (Seq, ViewL(..), singleton,
                                                      viewl, (><))

import           Test.DejaFu.Common
import           Test.DejaFu.Conc.Internal.Common
import           Test.DejaFu.Conc.Internal.Threading

import qualified Data.Map.Strict                     as M

--------------------------------------------------------------------------------
-- * Manipulating @CRef@s

-- | In non-sequentially-consistent memory models, non-synchronised
-- writes get buffered.
--
-- The @CRefId@ parameter is only used under PSO. Under TSO each
-- thread has a single buffer.
newtype WriteBuffer r = WriteBuffer
  { buffer :: Map (ThreadId, Maybe CRefId) (Seq (BufferedWrite r)) }

-- | A buffered write is a reference to the variable, and the value to
-- write. Universally quantified over the value type so that the only
-- thing which can be done with it is to write it to the reference.
data BufferedWrite r where
  BufferedWrite :: ThreadId -> CRef r a -> a -> BufferedWrite r

-- | An empty write buffer.
emptyBuffer :: WriteBuffer r
emptyBuffer = WriteBuffer M.empty

-- | Add a new write to the end of a buffer.
bufferWrite :: MonadRef r n => WriteBuffer r -> (ThreadId, Maybe CRefId) -> CRef r a -> a -> n (WriteBuffer r)
bufferWrite (WriteBuffer wb) k@(tid, _) cref@(CRef _ ref) new = do
  -- Construct the new write buffer
  let write = singleton $ BufferedWrite tid cref new
  let buffer' = M.insertWith (flip (><)) k write wb

  -- Write the thread-local value to the @CRef@'s update map.
  (locals, count, def) <- readRef ref
  writeRef ref (M.insert tid new locals, count, def)

  pure (WriteBuffer buffer')

-- | Commit the write at the head of a buffer.
commitWrite :: MonadRef r n => WriteBuffer r -> (ThreadId, Maybe CRefId) -> n (WriteBuffer r)
commitWrite w@(WriteBuffer wb) k = case maybe EmptyL viewl $ M.lookup k wb of
  BufferedWrite _ cref a :< rest -> do
    writeImmediate cref a
    pure . WriteBuffer $ M.insert k rest wb

  EmptyL -> pure w

-- | Read from a @CRef@, returning a newer thread-local non-committed
-- write if there is one.
readCRef :: MonadRef r n => CRef r a -> ThreadId -> n a
readCRef cref tid = do
  (val, _) <- readCRefPrim cref tid
  pure val

-- | Read from a @CRef@, returning a @Ticket@ representing the current
-- view of the thread.
readForTicket :: MonadRef r n => CRef r a -> ThreadId -> n (Ticket a)
readForTicket cref@(CRef crid _) tid = do
  (val, count) <- readCRefPrim cref tid
  pure (Ticket crid count val)

-- | Perform a compare-and-swap on a @CRef@ if the ticket is still
-- valid. This is strict in the \"new\" value argument.
casCRef :: MonadRef r n => CRef r a -> ThreadId -> Ticket a -> a -> n (Bool, Ticket a)
casCRef cref tid (Ticket _ cc _) !new = do
  tick'@(Ticket _ cc' _) <- readForTicket cref tid

  if cc == cc'
  then do
    writeImmediate cref new
    tick'' <- readForTicket cref tid
    pure (True, tick'')
  else pure (False, tick')

-- | Read the local state of a @CRef@.
readCRefPrim :: MonadRef r n => CRef r a -> ThreadId -> n (a, Integer)
readCRefPrim (CRef _ ref) tid = do
  (vals, count, def) <- readRef ref

  pure (M.findWithDefault def tid vals, count)

-- | Write and commit to a @CRef@ immediately, clearing the update map
-- and incrementing the write count.
writeImmediate :: MonadRef r n => CRef r a -> a -> n ()
writeImmediate (CRef _ ref) a = do
  (_, count, _) <- readRef ref
  writeRef ref (M.empty, count + 1, a)

-- | Flush all writes in the buffer.
writeBarrier :: MonadRef r n => WriteBuffer r -> n ()
writeBarrier (WriteBuffer wb) = mapM_ flush $ M.elems wb where
  flush = mapM_ $ \(BufferedWrite _ cref a) -> writeImmediate cref a

-- | Add phantom threads to the thread list to commit pending writes.
addCommitThreads :: WriteBuffer r -> Threads n r -> Threads n r
addCommitThreads (WriteBuffer wb) ts = ts <> M.fromList phantoms where
  phantoms = [ (ThreadId Nothing $ negate tid, mkthread c)
             | ((_, b), tid) <- zip (M.toList wb) [1..]
             , c <- maybeToList (go $ viewl b)
             ]
  go (BufferedWrite tid (CRef crid _) _ :< _) = Just $ ACommit tid crid
  go EmptyL = Nothing

-- | Remove phantom threads.
delCommitThreads :: Threads n r -> Threads n r
delCommitThreads = M.filterWithKey $ \k _ -> k >= initialThread

--------------------------------------------------------------------------------
-- * Manipulating @MVar@s

-- these are a bit clearer than a Bool
data Blocking = Blocking | NonBlocking
data Emptying = Emptying | NonEmptying

-- | Put into a @MVar@, blocking if full.
putIntoMVar :: MonadRef r n => MVar r a -> a -> Action n r
            -> ThreadId -> Threads n r -> n (Bool, Threads n r, [ThreadId])
putIntoMVar cvar a c = mutMVar Blocking cvar a (const c)

-- | Try to put into a @MVar@, not blocking if full.
tryPutIntoMVar :: MonadRef r n => MVar r a -> a -> (Bool -> Action n r)
               -> ThreadId -> Threads n r -> n (Bool, Threads n r, [ThreadId])
tryPutIntoMVar = mutMVar NonBlocking

-- | Read from a @MVar@, blocking if empty.
readFromMVar :: MonadRef r n => MVar r a -> (a -> Action n r)
            -> ThreadId -> Threads n r -> n (Bool, Threads n r, [ThreadId])
readFromMVar cvar c = seeMVar NonEmptying Blocking cvar (c . fromJust)

-- | Try to read from a @MVar@, not blocking if empty.
tryReadFromMVar :: MonadRef r n => MVar r a -> (Maybe a -> Action n r)
                -> ThreadId -> Threads n r -> n (Bool, Threads n r, [ThreadId])
tryReadFromMVar = seeMVar NonEmptying NonBlocking

-- | Take from a @MVar@, blocking if empty.
takeFromMVar :: MonadRef r n => MVar r a -> (a -> Action n r)
             -> ThreadId -> Threads n r -> n (Bool, Threads n r, [ThreadId])
takeFromMVar cvar c = seeMVar Emptying Blocking cvar (c . fromJust)

-- | Try to take from a @MVar@, not blocking if empty.
tryTakeFromMVar :: MonadRef r n => MVar r a -> (Maybe a -> Action n r)
                -> ThreadId -> Threads n r -> n (Bool, Threads n r, [ThreadId])
tryTakeFromMVar = seeMVar Emptying NonBlocking

-- | Mutate a @MVar@, in either a blocking or nonblocking way.
mutMVar :: MonadRef r n
        => Blocking -> MVar r a -> a -> (Bool -> Action n r)
        -> ThreadId -> Threads n r -> n (Bool, Threads n r, [ThreadId])
mutMVar blocking (MVar cvid ref) a c threadid threads = do
  val <- readRef ref

  case val of
    Just _ -> case blocking of
      Blocking ->
        let threads' = block (OnMVarEmpty cvid) threadid threads
        in pure (False, threads', [])
      NonBlocking ->
        pure (False, goto (c False) threadid threads, [])

    Nothing -> do
      writeRef ref $ Just a
      let (threads', woken) = wake (OnMVarFull cvid) threads
      pure (True, goto (c True) threadid threads', woken)

-- | Read a @MVar@, in either a blocking or nonblocking
-- way.
seeMVar :: MonadRef r n
        => Emptying -> Blocking -> MVar r a -> (Maybe a -> Action n r)
        -> ThreadId -> Threads n r -> n (Bool, Threads n r, [ThreadId])
seeMVar emptying blocking (MVar cvid ref) c threadid threads = do
  val <- readRef ref

  case val of
    Just _ -> do
      case emptying of
        Emptying    -> writeRef ref Nothing
        NonEmptying -> pure ()
      let (threads', woken) = wake (OnMVarEmpty cvid) threads
      pure (True, goto (c val) threadid threads', woken)

    Nothing -> case blocking of
      Blocking ->
        let threads' = block (OnMVarFull cvid) threadid threads
        in pure (False, threads', [])
      NonBlocking ->
        pure (False, goto (c Nothing) threadid threads, [])

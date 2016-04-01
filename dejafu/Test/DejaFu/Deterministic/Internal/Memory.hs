{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs        #-}

-- | Operations over @CRef@s and @MVar@s
--
-- Relaxed memory operations over @CRef@s are implemented with an
-- explicit write buffer: one per thread for TSO, and one per
-- thread/variable combination for PSO. Unsynchronised writes append
-- to this buffer, and periodically separate threads commit from these
-- buffers to the \"actual\" @CRef@.
--
-- This model comes from /Dynamic Partial Order Reduction for Relaxed
-- Memory Models/, N. Zhang, M. Kusano, and C. Wang (2015).
module Test.DejaFu.Deterministic.Internal.Memory where

import Control.Monad (when)
import Data.Map.Strict (Map)
import Data.Maybe (isJust, fromJust)
import Data.Monoid ((<>))
import Data.Sequence (Seq, ViewL(..), (><), singleton, viewl)
import Test.DejaFu.Deterministic.Internal.Common
import Test.DejaFu.Deterministic.Internal.Threading
import Test.DejaFu.Internal

import qualified Data.Map.Strict as M

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
bufferWrite :: Monad n => Fixed n r s -> WriteBuffer r -> (ThreadId, Maybe CRefId) -> CRef r a -> a -> n (WriteBuffer r)
bufferWrite fixed (WriteBuffer wb) k@(tid, _) cref@(CRef _ ref) new = do
  -- Construct the new write buffer
  let write = singleton $ BufferedWrite tid cref new
  let buffer' = M.insertWith (flip (><)) k write wb

  -- Write the thread-local value to the @CRef@'s update map.
  (locals, count, def) <- readRef fixed ref
  writeRef fixed ref (M.insert tid new locals, count, def)

  return $ WriteBuffer buffer'

-- | Commit the write at the head of a buffer.
commitWrite :: Monad n => Fixed n r s -> WriteBuffer r -> (ThreadId, Maybe CRefId) -> n (WriteBuffer r)
commitWrite fixed w@(WriteBuffer wb) k = case maybe EmptyL viewl $ M.lookup k wb of
  BufferedWrite _ cref a :< rest -> do
    writeImmediate fixed cref a
    return . WriteBuffer $ M.insert k rest wb

  EmptyL -> return w

-- | Read from a @CRef@, returning a newer thread-local non-committed
-- write if there is one.
readCRef :: Monad n => Fixed n r s -> CRef r a -> ThreadId -> n a
readCRef fixed cref tid = do
  (val, _) <- readCRefPrim fixed cref tid
  return val

-- | Read from a @CRef@, returning a @Ticket@ representing the current
-- view of the thread.
readForTicket :: Monad n => Fixed n r s -> CRef r a -> ThreadId -> n (Ticket a)
readForTicket fixed cref@(CRef crid _) tid = do
  (val, count) <- readCRefPrim fixed cref tid
  return $ Ticket crid count val

-- | Perform a compare-and-swap on a @CRef@ if the ticket is still
-- valid. This is strict in the \"new\" value argument.
casCRef :: Monad n => Fixed n r s -> CRef r a -> ThreadId -> Ticket a -> a -> n (Bool, Ticket a)
casCRef fixed cref tid (Ticket _ cc _) !new = do
  tick'@(Ticket _ cc' _) <- readForTicket fixed cref tid

  if cc == cc'
  then do
    writeImmediate fixed cref new
    tick'' <- readForTicket fixed cref tid
    return (True, tick'')
  else return (False, tick')

-- | Read the local state of a @CRef@.
readCRefPrim :: Monad n => Fixed n r s -> CRef r a -> ThreadId -> n (a, Integer)
readCRefPrim fixed (CRef _ ref) tid = do
  (vals, count, def) <- readRef fixed ref

  return (M.findWithDefault def tid vals, count)

-- | Write and commit to a @CRef@ immediately, clearing the update map
-- and incrementing the write count.
writeImmediate :: Monad n => Fixed n r s -> CRef r a -> a -> n ()
writeImmediate fixed (CRef _ ref) a = do
  (_, count, _) <- readRef fixed ref
  writeRef fixed ref (M.empty, count + 1, a)

-- | Flush all writes in the buffer.
writeBarrier :: Monad n => Fixed n r s -> WriteBuffer r -> n ()
writeBarrier fixed (WriteBuffer wb) = mapM_ flush $ M.elems wb where
  flush = mapM_ $ \(BufferedWrite _ cref a) -> writeImmediate fixed cref a

-- | Add phantom threads to the thread list to commit pending writes.
addCommitThreads :: WriteBuffer r -> Threads n r s -> Threads n r s
addCommitThreads (WriteBuffer wb) ts = ts <> M.fromList phantoms where
  phantoms = [ (ThreadId Nothing $ negate tid, mkthread $ fromJust c)
             | ((k, b), tid) <- zip (M.toList wb) [1..]
             , let c = go $ viewl b
             , isJust c]
  go (BufferedWrite tid (CRef crid _) _ :< _) = Just $ ACommit tid crid
  go EmptyL = Nothing

-- | Remove phantom threads.
delCommitThreads :: Threads n r s -> Threads n r s
delCommitThreads = M.filterWithKey $ \k _ -> k >= initialThread

--------------------------------------------------------------------------------
-- * Manipulating @MVar@s

-- | Put into a @MVar@, blocking if full.
putIntoMVar :: Monad n => MVar r a -> a -> Action n r s
             -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
putIntoMVar cvar a c = mutMVar True cvar a (const c)

-- | Try to put into a @MVar@, not blocking if full.
tryPutIntoMVar :: Monad n => MVar r a -> a -> (Bool -> Action n r s)
                 -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
tryPutIntoMVar = mutMVar False

-- | Read from a @MVar@, blocking if empty.
readFromMVar :: Monad n => MVar r a -> (a -> Action n r s)
              -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
readFromMVar cvar c = seeMVar False True cvar (c . fromJust)

-- | Take from a @MVar@, blocking if empty.
takeFromMVar :: Monad n => MVar r a -> (a -> Action n r s)
              -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
takeFromMVar cvar c = seeMVar True True cvar (c . fromJust)

-- | Try to take from a @MVar@, not blocking if empty.
tryTakeFromMVar :: Monad n => MVar r a -> (Maybe a -> Action n r s)
                  -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
tryTakeFromMVar = seeMVar True False

-- | Mutate a @MVar@, in either a blocking or nonblocking way.
mutMVar :: Monad n
         => Bool -> MVar r a -> a -> (Bool -> Action n r s)
         -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
mutMVar blocking (MVar cvid ref) a c fixed threadid threads = do
  val <- readRef fixed ref

  case val of
    Just _
      | blocking ->
        let threads' = block (OnMVarEmpty cvid) threadid threads
        in return (False, threads', [])

      | otherwise ->
        return (False, goto (c False) threadid threads, [])

    Nothing -> do
      writeRef fixed ref $ Just a
      let (threads', woken) = wake (OnMVarFull cvid) threads
      return (True, goto (c True) threadid threads', woken)

-- | Read a @MVar@, in either a blocking or nonblocking
-- way.
seeMVar :: Monad n
         => Bool -> Bool -> MVar r a -> (Maybe a -> Action n r s)
         -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
seeMVar emptying blocking (MVar cvid ref) c fixed threadid threads = do
  val <- readRef fixed ref

  case val of
    Just _ -> do
      when emptying $ writeRef fixed ref Nothing
      let (threads', woken) = wake (OnMVarEmpty cvid) threads
      return (True, goto (c val) threadid threads', woken)

    Nothing
      | blocking ->
        let threads' = block (OnMVarFull cvid) threadid threads
        in return (False, threads', [])

      | otherwise ->
        return (False, goto (c Nothing) threadid threads, [])

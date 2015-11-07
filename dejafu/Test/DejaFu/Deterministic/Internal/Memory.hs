{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}

-- | Operations over @CRef@s and @CVar@s
module Test.DejaFu.Deterministic.Internal.Memory where

import Control.Monad (when)
import Data.IntMap.Strict (IntMap)
import Data.Maybe (isJust, fromJust)
import Data.Monoid ((<>))
import Data.Sequence (Seq, ViewL(..), (><), singleton, viewl)
import Test.DejaFu.Deterministic.Internal.Common
import Test.DejaFu.Deterministic.Internal.Threading
import Test.DejaFu.Internal

import qualified Data.IntMap.Strict as I
import qualified Data.Map as M

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (mapM_)
import Prelude hiding (mapM_)
#endif

--------------------------------------------------------------------------------
-- * Manipulating @CRef@s

-- | In non-sequentially-consistent memory models, non-synchronised
-- writes get buffered.
--
-- In TSO, the keys are @ThreadId@s. In PSO, the keys are @CRefId@s.
newtype WriteBuffer r = WriteBuffer { buffer :: IntMap (Seq (BufferedWrite r)) }

-- | A buffered write is a reference to the variable, and the value to
-- write. Universally quantified over the value type so that the only
-- thing which can be done with it is to write it to the reference.
data BufferedWrite r where
  BufferedWrite :: ThreadId -> CRef r a -> a -> BufferedWrite r

-- | An empty write buffer.
emptyBuffer :: WriteBuffer r
emptyBuffer = WriteBuffer I.empty

-- | Add a new write to the end of a buffer.
bufferWrite :: Monad n => Fixed n r s -> WriteBuffer r -> Int -> CRef r a -> a -> ThreadId -> n (WriteBuffer r)
bufferWrite fixed (WriteBuffer wb) i cref@(CRef (_, ref)) new tid = do
  -- Construct the new write buffer
  let write = singleton $ BufferedWrite tid cref new
  let buffer' = I.insertWith (><) i write wb

  -- Write the thread-local value to the @CRef@'s update map.
  (map, def) <- readRef fixed ref
  writeRef fixed ref (I.insert tid new map, def)

  return $ WriteBuffer buffer'

-- | Commit the write at the head of a buffer.
commitWrite :: Monad n => Fixed n r s -> WriteBuffer r -> Int -> n (WriteBuffer r)
commitWrite fixed w@(WriteBuffer wb) i = case maybe EmptyL viewl $ I.lookup i wb of
  BufferedWrite _ cref a :< rest -> do
    writeImmediate fixed cref a
    return . WriteBuffer $ I.insert i rest wb
    
  EmptyL -> return w

-- | Read from a @CRef@, returning a newer thread-local non-committed
-- write if there is one.
readCRef :: Monad n => Fixed n r s -> CRef r a -> ThreadId -> n a
readCRef fixed (CRef (_, ref)) tid = do
  (map, def) <- readRef fixed ref
  return $ I.findWithDefault def tid map

-- | Write and commit to a @CRef@ immediately, clearing the update
-- map.
writeImmediate :: Monad n => Fixed n r s -> CRef r a -> a -> n ()
writeImmediate fixed (CRef (_, ref)) a = writeRef fixed ref (I.empty, a)

-- | Flush all writes in the buffer.
writeBarrier :: Monad n => Fixed n r s -> WriteBuffer r -> n ()
writeBarrier fixed (WriteBuffer wb) = mapM_ flush $ I.elems wb where
  flush = mapM_ $ \(BufferedWrite _ cref a) -> writeImmediate fixed cref a

-- | Add phantom threads to the thread list to commit pending writes.
addCommitThreads :: WriteBuffer r -> Threads n r s -> Threads n r s
addCommitThreads (WriteBuffer wb) ts = ts <> M.fromList phantoms where
  phantoms = [(negate k - 1, mkthread $ fromJust c) | (k, b) <- I.toList wb, let c = go $ viewl b, isJust c]
  go (BufferedWrite tid (CRef (crid, _)) _ :< _) = Just $ ACommit tid crid
  go EmptyL = Nothing

-- | Remove phantom threads.
delCommitThreads :: Threads n r s -> Threads n r s
delCommitThreads = M.filterWithKey $ \k _ -> k >= 0

--------------------------------------------------------------------------------
-- * Manipulating @CVar@s

-- | Put a value into a @CVar@, in either a blocking or nonblocking
-- way.
putIntoCVar :: Monad n
            => Bool -> CVar r a -> a -> (Bool -> Action n r s)
            -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
putIntoCVar blocking (CVar (cvid, ref)) a c fixed threadid threads = do
  val <- readRef fixed ref

  case val of
    Just _
      | blocking ->
        let threads' = block (OnCVarEmpty cvid) threadid threads
        in return (False, threads', [])

      | otherwise ->
        return (False, goto (c False) threadid threads, [])

    Nothing -> do
      writeRef fixed ref $ Just a
      let (threads', woken) = wake (OnCVarFull cvid) threads
      return (True, goto (c True) threadid threads', woken)

-- | Take a value from a @CVar@, in either a blocking or nonblocking
-- way.
readFromCVar :: Monad n
             => Bool -> Bool -> CVar r a -> (Maybe a -> Action n r s)
             -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
readFromCVar emptying blocking (CVar (cvid, ref)) c fixed threadid threads = do
  val <- readRef fixed ref

  case val of
    Just _ -> do
      when emptying $ writeRef fixed ref Nothing
      let (threads', woken) = wake (OnCVarEmpty cvid) threads
      return (True, goto (c val) threadid threads', woken)

    Nothing
      | blocking ->
        let threads' = block (OnCVarFull cvid) threadid threads
        in return (False, threads', [])

      | otherwise ->
        return (False, goto (c Nothing) threadid threads, [])

{-# LANGUAGE GADTs #-}

-- | Operations over @CRef@s and @CVar@s
module Test.DejaFu.Deterministic.Internal.Memory where

import Control.Monad (when)
import Data.IntMap.Strict (IntMap)
import Data.Sequence (Seq, ViewL(..), (><), singleton, viewl)
import Test.DejaFu.Deterministic.Internal.Common
import Test.DejaFu.Deterministic.Internal.Threading
import Test.DejaFu.Internal

import qualified Data.IntMap.Strict as I

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
  BufferedWrite :: R r a -> a -> BufferedWrite r

-- | An empty write buffer.
emptyBuffer :: WriteBuffer r
emptyBuffer = WriteBuffer I.empty

-- | Add a new write to the end of a buffer.
bufferWrite :: Monad n => Fixed n r s -> WriteBuffer r -> Int -> R r a -> a -> ThreadId -> n (WriteBuffer r)
bufferWrite fixed (WriteBuffer wb) i cref@(_, ref) new tid = do
  -- Construct the new write buffer
  let write = singleton $ BufferedWrite cref new
  let buffer' = I.insertWith (><) i write wb

  -- Write the thread-local value to the @CRef@'s update map.
  (map, def) <- readRef fixed ref
  writeRef fixed ref (I.insert tid new map, def)

  return $ WriteBuffer buffer'

-- | Commit the write at the head of a buffer.
commitWrite :: Monad n => Fixed n r s -> WriteBuffer r -> Int -> n (WriteBuffer r)
commitWrite fixed w@(WriteBuffer wb) i = case maybe EmptyL viewl $ I.lookup i wb of
  BufferedWrite (_, ref) a :< rest -> do
    -- Write the value tot he @CRef@ and clear the update map.
    writeRef fixed ref (I.empty, a)

    -- Return a new write buffer with the head gone.
    return . WriteBuffer $ I.insert i rest wb
    
  EmptyL -> return w

-- | Read from a @CRef@, returning a newer thread-local non-committed
-- write if there is one.
readCRef :: Monad n => Fixed n r s -> R r a -> ThreadId -> n a
readCRef fixed (_, ref) tid = do
  (map, def) <- readRef fixed ref
  return $ I.findWithDefault def tid map

--------------------------------------------------------------------------------
-- * Manipulating @CVar@s

-- | Put a value into a @CVar@, in either a blocking or nonblocking
-- way.
putIntoCVar :: Monad n
            => Bool -> V r a -> a -> (Bool -> Action n r s)
            -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
putIntoCVar blocking (cvid, ref) a c fixed threadid threads = do
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
             => Bool -> Bool -> V r a -> (Maybe a -> Action n r s)
             -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
readFromCVar emptying blocking (cvid, ref) c fixed threadid threads = do
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

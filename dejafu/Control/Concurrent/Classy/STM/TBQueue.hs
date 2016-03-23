-- | 'TBQueue' is a bounded version of 'TQueue'. The queue has a maximum
-- capacity set when it is created.  If the queue already contains the
-- maximum number of elements, then 'writeTBQueue' blocks until an
-- element is removed from the queue.
--
-- The implementation is based on the traditional purely-functional
-- queue representation that uses two lists to obtain amortised /O(1)/
-- enqueue and dequeue operations.
module Control.Concurrent.Classy.STM.TBQueue
  ( -- * TBQueue
    TBQueue
  , newTBQueue
  , readTBQueue
  , tryReadTBQueue
  , peekTBQueue
  , tryPeekTBQueue
  , writeTBQueue
  , unGetTBQueue
  , isEmptyTBQueue
  , isFullTBQueue
  ) where

import Control.Monad.STM.Class

-- | 'TBQueue' is an abstract type representing a bounded FIFO
-- channel.
data TBQueue stm a
   = TBQueue (TVar stm Int)
             (TVar stm [a])
             (TVar stm Int)
             (TVar stm [a])

-- | Build and returns a new instance of 'TBQueue'
newTBQueue :: MonadSTM stm
  => Int   -- ^ maximum number of elements the queue can hold
  -> stm (TBQueue stm a)
newTBQueue size = do
  readT  <- newTVar []
  writeT <- newTVar []
  rsize <- newTVar 0
  wsize <- newTVar size
  pure (TBQueue rsize readT wsize writeT)

-- | Write a value to a 'TBQueue'; retries if the queue is full.
writeTBQueue :: MonadSTM stm => TBQueue stm a -> a -> stm ()
writeTBQueue (TBQueue rsize _ wsize writeT) a = do
  w <- readTVar wsize
  if w /= 0
  then writeTVar wsize (w - 1)
  else do
    r <- readTVar rsize
    if r /= 0
    then do
      writeTVar rsize 0
      writeTVar wsize (r - 1)
    else retry
  listend <- readTVar writeT
  writeTVar writeT (a:listend)

-- | Read the next value from the 'TBQueue'.
readTBQueue :: MonadSTM stm => TBQueue stm a -> stm a
readTBQueue (TBQueue rsize readT _ writeT) = do
  xs <- readTVar readT
  r  <- readTVar rsize
  writeTVar rsize (r + 1)
  case xs of
    (x:xs') -> do
      writeTVar readT xs'
      pure x
    [] -> do
      ys <- readTVar writeT
      case ys of
        [] -> retry
        _  -> do
          let (z:zs) = reverse ys
          writeTVar writeT []
          writeTVar readT zs
          pure z

-- | A version of 'readTBQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTBQueue :: MonadSTM stm => TBQueue stm a -> stm (Maybe a)
tryReadTBQueue c = (Just <$> readTBQueue c) `orElse` pure Nothing

-- | Get the next value from the @TBQueue@ without removing it,
-- retrying if the channel is empty.
peekTBQueue :: MonadSTM stm => TBQueue stm a -> stm a
peekTBQueue c = do
  x <- readTBQueue c
  unGetTBQueue c x
  return x

-- | A version of 'peekTBQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryPeekTBQueue :: MonadSTM stm => TBQueue stm a -> stm (Maybe a)
tryPeekTBQueue c = do
  m <- tryReadTBQueue c
  case m of
    Nothing -> pure Nothing
    Just x  -> do
      unGetTBQueue c x
      pure m

-- | Put a data item back onto a channel, where it will be the next item read.
-- Retries if the queue is full.
unGetTBQueue :: MonadSTM stm => TBQueue stm a -> a -> stm ()
unGetTBQueue (TBQueue rsize readT wsize _) a = do
  r <- readTVar rsize
  if r > 0
  then writeTVar rsize (r - 1)
  else do
    w <- readTVar wsize
    if w > 0
    then writeTVar wsize (w - 1)
    else retry
  xs <- readTVar readT
  writeTVar readT (a:xs)

-- | Returns 'True' if the supplied 'TBQueue' is empty.
isEmptyTBQueue :: MonadSTM stm => TBQueue stm a -> stm Bool
isEmptyTBQueue (TBQueue _ readT _ writeT) = do
  xs <- readTVar readT
  case xs of
    (_:_) -> pure False
    [] -> null <$> readTVar writeT

-- | Returns 'True' if the supplied 'TBQueue' is full.
isFullTBQueue :: MonadSTM stm => TBQueue stm a -> stm Bool
isFullTBQueue (TBQueue rsize _ wsize _) = do
  w <- readTVar wsize
  if w > 0
  then pure False
  else (>0) <$> readTVar rsize

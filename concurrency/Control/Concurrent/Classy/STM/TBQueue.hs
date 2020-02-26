-- |
-- Module      : Control.Concurrent.Classy.STM.TBQueue
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : portable
--
-- 'TBQueue' is a bounded version of 'TQueue'. The queue has a maximum
-- capacity set when it is created.  If the queue already contains the
-- maximum number of elements, then 'writeTBQueue' blocks until an
-- element is removed from the queue.
--
-- The implementation is based on the traditional purely-functional
-- queue representation that uses two lists to obtain amortised /O(1)/
-- enqueue and dequeue operations.
--
-- __Deviations:__ @TBQueue@ as defined here does not have an @Eq@
-- instance, this is because the @MonadSTM@ @TVar@ type does not have
-- an @Eq@ constraint. Furthermore, the @newTBQueueIO@ function is not
-- provided.
module Control.Concurrent.Classy.STM.TBQueue
  ( -- * TBQueue
    TBQueue
  , newTBQueue
  , readTBQueue
  , tryReadTBQueue
  , flushTBQueue
  , peekTBQueue
  , tryPeekTBQueue
  , writeTBQueue
  , unGetTBQueue
  , lengthTBQueue
  , isEmptyTBQueue
  , isFullTBQueue
  ) where

import           Control.Monad.STM.Class
import           Numeric.Natural

-- | 'TBQueue' is an abstract type representing a bounded FIFO
-- channel.
--
-- @since unreleased
data TBQueue stm a
   = TBQueue (TVar stm Natural)
             (TVar stm [a])
             (TVar stm Natural)
             (TVar stm [a])
             !Natural

-- | Builds and returns a new instance of 'TBQueue'
--
-- @since unreleased
newTBQueue :: MonadSTM stm
  => Natural -- ^ maximum number of elements the queue can hold
  -> stm (TBQueue stm a)
newTBQueue size = do
  readT  <- newTVar []
  writeT <- newTVar []
  rsize <- newTVar 0
  wsize <- newTVar size
  pure (TBQueue rsize readT wsize writeT size)

-- | Write a value to a 'TBQueue'; retries if the queue is full.
--
-- @since 1.0.0.0
writeTBQueue :: MonadSTM stm => TBQueue stm a -> a -> stm ()
writeTBQueue (TBQueue rsize _ wsize writeT _) a = do
  w <- readTVar wsize
  if w > 0
  then writeTVar wsize $! w - 1
  else do
    r <- readTVar rsize
    if r > 0
    then do
      writeTVar rsize 0
      writeTVar wsize $! r - 1
    else retry
  listend <- readTVar writeT
  writeTVar writeT (a:listend)

-- | Read the next value from the 'TBQueue'.
--
-- @since 1.0.0.0
readTBQueue :: MonadSTM stm => TBQueue stm a -> stm a
readTBQueue (TBQueue rsize readT _ writeT _) = do
  xs <- readTVar readT
  r  <- readTVar rsize
  writeTVar rsize $! r + 1
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
--
-- @since 1.0.0.0
tryReadTBQueue :: MonadSTM stm => TBQueue stm a -> stm (Maybe a)
tryReadTBQueue c = (Just <$> readTBQueue c) `orElse` pure Nothing

-- | Efficiently read the entire contents of a 'TBQueue' into a list. This
-- function never retries.
--
-- @since 1.6.1.0
flushTBQueue :: MonadSTM stm => TBQueue stm a -> stm [a]
flushTBQueue (TBQueue rsize r wsize w size) = do
  xs <- readTVar r
  ys <- readTVar w
  if null xs && null ys
    then pure []
    else do
      writeTVar r []
      writeTVar w []
      writeTVar rsize 0
      writeTVar wsize size
      pure (xs ++ reverse ys)

-- | Get the next value from the @TBQueue@ without removing it,
-- retrying if the channel is empty.
--
-- @since 1.0.0.0
peekTBQueue :: MonadSTM stm => TBQueue stm a -> stm a
peekTBQueue c = do
  x <- readTBQueue c
  unGetTBQueue c x
  pure x

-- | A version of 'peekTBQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
--
-- @since 1.0.0.0
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
--
-- @since 1.0.0.0
unGetTBQueue :: MonadSTM stm => TBQueue stm a -> a -> stm ()
unGetTBQueue (TBQueue rsize readT wsize _ _) a = do
  r <- readTVar rsize
  if r > 0
  then writeTVar rsize $! r - 1
  else do
    w <- readTVar wsize
    if w > 0
    then writeTVar wsize $! w - 1
    else retry
  xs <- readTVar readT
  writeTVar readT (a:xs)

-- |Return the length of a 'TBQueue'.
--
-- @since unreleased
lengthTBQueue :: MonadSTM stm => TBQueue stm a -> stm Natural
lengthTBQueue (TBQueue rsize _ wsize _ size) = do
  r <- readTVar rsize
  w <- readTVar wsize
  pure $! size - r - w

-- | Returns 'True' if the supplied 'TBQueue' is empty.
--
-- @since 1.0.0.0
isEmptyTBQueue :: MonadSTM stm => TBQueue stm a -> stm Bool
isEmptyTBQueue (TBQueue _ readT _ writeT _) = do
  xs <- readTVar readT
  case xs of
    (_:_) -> pure False
    [] -> null <$> readTVar writeT

-- | Returns 'True' if the supplied 'TBQueue' is full.
--
-- @since 1.0.0.0
isFullTBQueue :: MonadSTM stm => TBQueue stm a -> stm Bool
isFullTBQueue (TBQueue rsize _ wsize _ _) = do
  w <- readTVar wsize
  if w > 0
  then pure False
  else (>0) <$> readTVar rsize

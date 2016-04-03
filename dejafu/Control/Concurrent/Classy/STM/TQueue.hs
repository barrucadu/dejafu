-- | A 'TQueue' is like a 'TChan', with two important differences:
--
--  * it has faster throughput than both 'TChan' and 'Chan' (although
--    the costs are amortised, so the cost of individual operations
--    can vary a lot).
--
--  * it does /not/ provide equivalents of the 'dupTChan' and
--    'cloneTChan' operations.
--
-- The implementation is based on the traditional purely-functional
-- queue representation that uses two lists to obtain amortised /O(1)/
-- enqueue and dequeue operations.
--
-- __Deviations:__ @TQueue@ as defined here does not have an @Eq@
-- instance, this is because the @MonadSTM@ @TVar@ type does not have
-- an @Eq@ constraint. Furthermore, the @newTQueueIO@ function is not
-- provided.
module Control.Concurrent.Classy.STM.TQueue
  ( -- * TQueue
    TQueue
  , newTQueue
  , readTQueue
  , tryReadTQueue
  , peekTQueue
  , tryPeekTQueue
  , writeTQueue
  , unGetTQueue
  , isEmptyTQueue
  ) where

import Control.Monad.STM.Class

-- | 'TQueue' is an abstract type representing an unbounded FIFO channel.
data TQueue stm a = TQueue (TVar stm [a])
                           (TVar stm [a])

-- | Build and returns a new instance of 'TQueue'
newTQueue :: MonadSTM stm => stm (TQueue stm a)
newTQueue = do
  readT  <- newTVar []
  writeT <- newTVar []
  pure (TQueue readT writeT)

-- | Write a value to a 'TQueue'.
writeTQueue :: MonadSTM stm => TQueue stm a -> a -> stm ()
writeTQueue (TQueue _ writeT) a = do
  listend <- readTVar writeT
  writeTVar writeT (a:listend)

-- | Read the next value from the 'TQueue'.
readTQueue :: MonadSTM stm => TQueue stm a -> stm a
readTQueue (TQueue readT writeT) = do
  xs <- readTVar readT
  case xs of
    (x:xs') -> do
      writeTVar readT xs'
      pure x
    [] -> do
      ys <- readTVar writeT
      case ys of
        [] -> retry
        _  -> case reverse ys of
               [] -> error "readTQueue"
               (z:zs) -> do
                 writeTVar writeT []
                 writeTVar readT zs
                 pure z

-- | A version of 'readTQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTQueue :: MonadSTM stm => TQueue stm a -> stm (Maybe a)
tryReadTQueue c = (Just <$> readTQueue c) `orElse` pure Nothing

-- | Get the next value from the @TQueue@ without removing it,
-- retrying if the channel is empty.
peekTQueue :: MonadSTM stm => TQueue stm a -> stm a
peekTQueue c = do
  x <- readTQueue c
  unGetTQueue c x
  pure x

-- | A version of 'peekTQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryPeekTQueue :: MonadSTM stm => TQueue stm a -> stm (Maybe a)
tryPeekTQueue c = do
  m <- tryReadTQueue c
  case m of
    Nothing -> pure Nothing
    Just x  -> do
      unGetTQueue c x
      pure m

-- |Put a data item back onto a channel, where it will be the next item read.
unGetTQueue :: MonadSTM stm => TQueue stm a -> a -> stm ()
unGetTQueue (TQueue readT _) a = do
  xs <- readTVar readT
  writeTVar readT (a:xs)

-- |Returns 'True' if the supplied 'TQueue' is empty.
isEmptyTQueue :: MonadSTM stm => TQueue stm a -> stm Bool
isEmptyTQueue (TQueue readT writeT) = do
  xs <- readTVar readT
  case xs of
    (_:_) -> pure False
    [] -> null <$> readTVar writeT

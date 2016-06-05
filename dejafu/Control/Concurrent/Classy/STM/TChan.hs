-- |
-- Module      : Control.Concurrent.Classy.STM.TChan
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : portable
--
-- Transactional channels
--
-- __Deviations:__ @TChan@ as defined here does not have an @Eq@
-- instance, this is because the @MonadSTM@ @TVar@ type does not have
-- an @Eq@ constraint. Furthermore, the @newTChanIO@ and
-- @newBroadcastTChanIO@ functions are not provided.
module Control.Concurrent.Classy.STM.TChan
  ( -- * TChans
    TChan

  -- * Construction
  , newTChan
  , newBroadcastTChan
  , dupTChan
  , cloneTChan

  -- * Reading and writing
  , readTChan
  , tryReadTChan
  , peekTChan
  , tryPeekTChan
  , writeTChan
  , unGetTChan
  , isEmptyTChan
  ) where

import Control.Monad.STM.Class

-- | 'TChan' is an abstract type representing an unbounded FIFO
-- channel.
data TChan stm a = TChan (TVar stm (TVarList stm a))
                         (TVar stm (TVarList stm a))

type TVarList stm a = TVar stm (TList stm a)
data TList stm a = TNil | TCons a (TVarList stm a)

-- |Build and return a new instance of 'TChan'
newTChan :: MonadSTM stm => stm (TChan stm a)
newTChan = do
  hole   <- newTVar TNil
  readH  <- newTVar hole
  writeH <- newTVar hole
  pure (TChan readH writeH)

-- | Create a write-only 'TChan'.  More precisely, 'readTChan' will 'retry'
-- even after items have been written to the channel.  The only way to
-- read a broadcast channel is to duplicate it with 'dupTChan'.
newBroadcastTChan :: MonadSTM stm => stm (TChan stm a)
newBroadcastTChan = do
    hole   <- newTVar TNil
    readT  <- newTVar (error "reading from a TChan created by newBroadcastTChan; use dupTChan first")
    writeT <- newTVar hole
    pure (TChan readT writeT)

-- | Write a value to a 'TChan'.
writeTChan :: MonadSTM stm => TChan stm a -> a -> stm ()
writeTChan (TChan _ writeT) a = do
  listend  <- readTVar writeT
  listend' <- newTVar TNil
  writeTVar listend (TCons a listend')
  writeTVar writeT listend'

-- | Read the next value from the 'TChan'.
readTChan :: MonadSTM stm => TChan stm a -> stm a
readTChan tchan = tryReadTChan tchan >>= maybe retry pure

-- | A version of 'readTChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTChan :: MonadSTM stm => TChan stm a -> stm (Maybe a)
tryReadTChan (TChan readT _) = do
  listhead <- readTVar readT
  hd <- readTVar listhead
  case hd of
    TNil       -> pure Nothing
    TCons a tl -> do
      writeTVar readT tl
      pure (Just a)

-- | Get the next value from the 'TChan' without removing it,
-- retrying if the channel is empty.
peekTChan :: MonadSTM stm => TChan stm a -> stm a
peekTChan tchan = tryPeekTChan tchan >>= maybe retry pure

-- | A version of 'peekTChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryPeekTChan :: MonadSTM stm => TChan stm a -> stm (Maybe a)
tryPeekTChan (TChan readT _) = do
  listhead <- readTVar readT
  hd <- readTVar listhead
  pure $ case hd of
    TNil      -> Nothing
    TCons a _ -> Just a

-- | Duplicate a 'TChan': the duplicate channel begins empty, but data written to
-- either channel from then on will be available from both.  Hence
-- this creates a kind of broadcast channel, where data written by
-- anyone is seen by everyone else.
dupTChan :: MonadSTM stm => TChan stm a -> stm (TChan stm a)
dupTChan (TChan _ writeT) = do
  hole   <- readTVar writeT
  readT' <- newTVar hole
  return (TChan readT' writeT)

-- | Put a data item back onto a channel, where it will be the next
-- item read.
unGetTChan :: MonadSTM stm => TChan stm a -> a -> stm ()
unGetTChan (TChan readT _) a = do
   listhead <- readTVar readT
   head' <- newTVar (TCons a listhead)
   writeTVar readT head'

-- | Returns 'True' if the supplied 'TChan' is empty.
isEmptyTChan :: MonadSTM stm => TChan stm a -> stm Bool
isEmptyTChan (TChan readT _) = do
  listhead <- readTVar readT
  hd <- readTVar listhead
  pure $ case hd of
    TNil -> True
    TCons _ _ -> False

-- | Clone a 'TChan': similar to 'dupTChan', but the cloned channel starts with the
-- same content available as the original channel.
cloneTChan :: MonadSTM stm => TChan stm a -> stm (TChan stm a)
cloneTChan (TChan readT writeT) = do
  readpos <- readTVar readT
  readT' <- newTVar readpos
  pure (TChan readT' writeT)

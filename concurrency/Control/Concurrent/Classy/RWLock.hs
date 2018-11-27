--------------------------------------------------------------------------------

-- Copyright © 2010-2012 Bas van Dijk & Roel van Dijk
-- Copyright © 2018 DFINITY Stiftung
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * The names of Bas van Dijk, Roel van Dijk and the names of
--       contributors may NOT be used to endorse or promote products
--       derived from this software without specific prior written
--       permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

-------------------------------------------------------------------------------

-- |
-- Module     : Control.Concurrent.Classy.RWLock
-- Copyright  : © 2010-2011 Bas van Dijk & Roel van Dijk
--            , © 2018 DFINITY Stiftung
-- Maintainer : DFINITY USA Research <team@dfinity.org>
--
-- Multiple-reader, single-writer locks. Used to protect shared resources which
-- may be concurrently read, but only sequentially written.
--
-- All functions are /exception safe/. Throwing asynchronous exceptions will not
-- compromise the internal state of an 'RWLock'. This means it is perfectly safe
-- to kill a thread that is blocking on, for example, 'acquireRead'.

-------------------------------------------------------------------------------

module Control.Concurrent.Classy.RWLock
  ( -- * @RWLock@
    RWLock

    -- * Creating locks
  , newRWLock
  , newAcquiredRead
  , newAcquiredWrite

    -- * Read access

    -- ** Blocking
  , acquireRead
  , releaseRead
  , withRead
  , waitRead

    -- ** Non-blocking
  , tryAcquireRead
  , tryWithRead

    -- * Write access

    -- ** Blocking
  , acquireWrite
  , releaseWrite
  , withWrite
  , waitWrite

    -- ** Non-blocking
  , tryAcquireWrite
  , tryWithWrite
  ) where

-------------------------------------------------------------------------------

import           Control.Applicative            (pure, (<*>))
import           Control.Monad                  (Monad, (>>))
import           Data.Bool                      (Bool(False, True))
import           Data.Eq                        (Eq, (==))
import           Data.Function                  (on, ($))
import           Data.Functor                   ((<$>))
import           Data.Int                       (Int)
import           Data.List                      ((++))
import           Data.Maybe                     (Maybe(Just, Nothing))
import           Data.Ord                       (Ord)
import           Data.Typeable                  (Typeable)
import           Prelude                        (String, error, pred, succ)
import           Text.Read                      (Read)
import           Text.Show                      (Show)

import qualified Control.Concurrent.Classy.MVar as MVar
import           Control.Monad.Catch            (bracket_, mask, mask_,
                                                 onException)
import           Control.Monad.Conc.Class       (MonadConc(MVar))

import           Control.Concurrent.Classy.Lock (Lock)
import qualified Control.Concurrent.Classy.Lock as Lock

-------------------------------------------------------------------------------

-- |
-- Multiple-reader, single-writer lock. Is in one of three states:
--
-- * \"Free\": Read or write access can be acquired without blocking.
--
-- * \"Read\": One or more threads have acquired read access.
--   Blocks write access.
--
-- * \"Write\": A single thread has acquired write access.
--   Blocks other threads from acquiring both read and write access.
data RWLock m
  = RWLock
    { _state     :: MVar m State
    , _readLock  :: Lock m
    , _writeLock :: Lock m
    }
  deriving (Typeable)

-- TODO: could the fields of RWLock be strict / unpacked?

instance (Eq (MVar m State)) => Eq (RWLock m) where
  (==) = (==) `on` _state

-------------------------------------------------------------------------------

-- |
-- Internal state of the 'RWLock'.
data State
  = Free
  | Read !Int
  | Write
  deriving (Eq, Ord, Show, Read)

-------------------------------------------------------------------------------

-- |
-- Create a new 'RWLock' in the \"free\" state; either read or write access
-- can be acquired without blocking.
newRWLock :: (MonadConc m) => m (RWLock m)
newRWLock = do
  state <- MVar.newMVar Free
  rlock <- Lock.newLock
  RWLock state rlock <$> Lock.newLock

-- |
-- Create a new 'RWLock' in the \"read\" state; only read can be acquired
-- without blocking.
newAcquiredRead :: (MonadConc m) => m (RWLock m)
newAcquiredRead = do
  state <- MVar.newMVar (Read 1)
  rlock <- Lock.newAcquired
  RWLock state rlock <$> Lock.newLock

-- |
-- Create a new 'RWLock' in the \"write\" state; either acquiring read or
-- write will block.
newAcquiredWrite :: (MonadConc m) => m (RWLock m)
newAcquiredWrite = do
  state <- MVar.newMVar Write
  rlock <- Lock.newLock
  RWLock state rlock <$> Lock.newAcquired

-------------------------------------------------------------------------------

-- |
-- Acquire the read lock.
--
-- Blocks if another thread has acquired write access.
-- If @acquireRead@ terminates without throwing an exception the state of
-- the 'RWLock' will be \"read\".
--
-- Implementation note: throws an exception when more than @'maxBound' :: 'Int'@
-- simultaneous threads acquire the read lock. But that is unlikely.
acquireRead :: (MonadConc m) => RWLock m -> m ()
acquireRead RWLock { _state, _readLock, _writeLock } = mask_ go
  where
    go = do
      st <- MVar.takeMVar _state
      case st of
        Free     -> do Lock.acquire _readLock
                       MVar.putMVar _state $ Read 1
        (Read n) ->    MVar.putMVar _state $ Read (succ n)
        Write    -> do MVar.putMVar _state st
                       Lock.wait _writeLock
                       go

-- |
-- Try to acquire the read lock; non blocking.
--
-- Like 'acquireRead', but doesn't block. Returns 'True' if the resulting
-- state is \"read\", 'False' otherwise.
tryAcquireRead :: (MonadConc m) => RWLock m -> m Bool
tryAcquireRead RWLock { _state, _readLock } = mask_ $ do
  st <- MVar.takeMVar _state
  case st of
    Free   -> do Lock.acquire _readLock
                 MVar.putMVar _state $ Read 1
                 pure True
    Read n -> do MVar.putMVar _state $ Read (succ n)
                 pure True
    Write  -> do MVar.putMVar _state st
                 pure False

-- |
-- Release the read lock.
--
-- If the calling thread was the last one to relinquish read access the state
-- will revert to \"free\".
--
-- It is an error to release read access to an 'RWLock' which is not in
-- the \"read\" state.
releaseRead :: (MonadConc m) => RWLock m -> m ()
releaseRead RWLock { _state, _readLock } = mask_ $ do
  st <- MVar.takeMVar _state
  case st of
    Read 1 -> do Lock.release _readLock
                 MVar.putMVar _state Free
    Read n ->    MVar.putMVar _state $ Read (pred n)
    _      -> do MVar.putMVar _state st
                 throw "releaseRead" "already released"

-- |
-- A convenience function wich first acquires read access and then performs the
-- computation. When the computation terminates, whether normally or by raising
-- an exception, the read lock is released.
withRead :: (MonadConc m) => RWLock m -> m a -> m a
withRead = bracket_ <$> acquireRead <*> releaseRead

-- |
-- A non-blocking 'withRead'. First tries to acquire the lock. If that fails,
-- 'Nothing' is returned. If it succeeds, the computation is performed.
-- When the computation terminates, whether normally or by raising an exception,
-- the lock is released and 'Just' the result of the computation is returned.
tryWithRead :: (MonadConc m) => RWLock m -> m a -> m (Maybe a)
tryWithRead l a = mask $ \restore -> do
  acquired <- tryAcquireRead l
  if acquired
    then do r <- restore a `onException` releaseRead l
            releaseRead l
            pure $ Just r
    else pure Nothing

-- |
-- * When the state is \"write\", @waitRead@ /blocks/ until a call to
--   'releaseWrite' in another thread changes the state to \"free\".
--
-- * When the state is \"free\" or \"read\" @waitRead@ returns immediately.
--
-- @waitRead@ does not alter the state of the lock.
--
-- Note that @waitRead@ is just a convenience function defined as:
--
-- @waitRead l = 'mask_' '$' 'acquireRead' l '>>' 'releaseRead' l@
waitRead :: (MonadConc m) => RWLock m -> m ()
waitRead l = mask_ (acquireRead l >> releaseRead l)

-------------------------------------------------------------------------------

-- |
-- Acquire the write lock.
--
-- Blocks if another thread has acquired either read or write access.
-- If @acquireWrite@ terminates without throwing an exception the state of
-- the 'RWLock' will be \"write\".
acquireWrite :: (MonadConc m) => RWLock m -> m ()
acquireWrite RWLock { _state, _readLock, _writeLock } = mask_ go'
  where
    go' = do
      st <- MVar.takeMVar _state
      case st of
        Free   -> do Lock.acquire _writeLock
                     MVar.putMVar _state Write
        Read _ -> do MVar.putMVar _state st
                     Lock.wait _readLock
                     go'
        Write  -> do MVar.putMVar _state st
                     Lock.wait _writeLock
                     go'

-- |
-- Try to acquire the write lock; non blocking.
--
-- Like 'acquireWrite', but doesn't block.
-- Returns 'True' if the resulting state is \"write\", 'False' otherwise.
tryAcquireWrite :: (MonadConc m) => RWLock m -> m Bool
tryAcquireWrite RWLock { _state, _writeLock } = mask_ $ do
  st <- MVar.takeMVar _state
  case st of
    Free -> do Lock.acquire _writeLock
               MVar.putMVar _state Write
               pure True
    _    -> do MVar.putMVar _state st
               pure False

-- |
-- Release the write lock.
--
-- If @releaseWrite@ terminates without throwing an exception the state
-- will be \"free\".
--
-- It is an error to release write access to an 'RWLock' which is not
-- in the \"write\" state.
releaseWrite :: (MonadConc m) => RWLock m -> m ()
releaseWrite RWLock { _state, _writeLock } = mask_ $ do
  st <- MVar.takeMVar _state
  case st of
    Write -> do Lock.release _writeLock
                MVar.putMVar _state Free
    _     -> do MVar.putMVar _state st
                throw "releaseWrite" "already released"

-- |
-- A convenience function wich first acquires write access and then performs
-- the computation. When the computation terminates, whether normally or by
-- raising an exception, the write lock is released.
withWrite :: (MonadConc m) => RWLock m -> m a -> m a
withWrite = bracket_ <$> acquireWrite <*> releaseWrite

-- |
-- A non-blocking 'withWrite'. First tries to acquire the lock. If that fails,
-- 'Nothing' is returned. If it succeeds, the computation is performed.
-- When the computation terminates, whether normally or by raising an exception,
-- the lock is released and 'Just' the result of the computation is returned.
tryWithWrite :: (MonadConc m) => RWLock m -> m a -> m (Maybe a)
tryWithWrite l a = mask $ \restore -> do
  acquired <- tryAcquireWrite l
  if acquired
    then do r <- restore a `onException` releaseWrite l
            releaseWrite l
            pure $ Just r
    else pure Nothing

-- |
-- * When the state is \"write\" or \"read\" @waitWrite@ /blocks/ until a call
--   to 'releaseWrite' or 'releaseRead' in another thread changes the state
--   to \"free\".
--
-- * When the state is \"free\" @waitWrite@ returns immediately.
--
-- @waitWrite@ does not alter the state of the lock.
--
-- Note that @waitWrite@ is just a convenience function defined as:
--
-- @waitWrite l = 'mask_' '$' 'acquireWrite' l '>>' 'releaseWrite' l@
waitWrite :: (MonadConc m) => RWLock m -> m ()
waitWrite l = mask_ (acquireWrite l >> releaseWrite l)

--------------------------------------------------------------------------------

throw :: (Monad m) => String -> String -> m a
throw func msg
  = error ("Control.Concurrent.Classy.RWLock." ++ func ++ ": " ++ msg)

--------------------------------------------------------------------------------

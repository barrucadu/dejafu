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

--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------

-- |
-- Module     : Control.Concurrent.Classy.Lock
-- Copyright  : © 2010-2011 Bas van Dijk & Roel van Dijk
--            , © 2018 DFINITY Stiftung
-- Maintainer : DFINITY USA Research <team@dfinity.org>
--
-- This module provides the 'Lock' synchronisation mechanism. It was inspired by
-- the Python and Java @Lock@ objects and should behave in a similar way. See:
--
-- <http://docs.python.org/3.1/library/threading.html#lock-objects>
--
-- and:
--
-- <http://java.sun.com/javase/7/docs/api/java/util/concurrent/locks/Lock.html>
--
-- All functions are /exception safe/. Throwing asynchronous exceptions will not
-- compromise the internal state of a 'Lock'.

--------------------------------------------------------------------------------

module Control.Concurrent.Classy.Lock
  ( -- * @Lock@
    Lock

    -- * Creating locks
  , newLock
  , newAcquired

    -- * Locking and unlocking
  , acquire
  , tryAcquire
  , release

    -- * Convenience functions
  , with
  , tryWith
  , wait

    -- * Querying locks
  , locked
  ) where

--------------------------------------------------------------------------------

import           Control.Applicative            (pure, (<*>))
import           Control.Monad                  (when)
import           Data.Bool                      (Bool, not)
import           Data.Eq                        (Eq((==)))
import           Data.Function                  (($), (.))
import           Data.Functor                   (fmap, (<$>))
import           Data.Maybe                     (Maybe(Just, Nothing), isJust)
import           Data.Typeable                  (Typeable)
import           Prelude                        (error)

import qualified Control.Concurrent.Classy.MVar as MVar
import           Control.Monad.Catch            (bracket_, mask, onException)
import           Control.Monad.Conc.Class       (MonadConc(MVar))

--------------------------------------------------------------------------------

-- | A lock is in one of two states: \"locked\" or \"unlocked\".
newtype Lock m
  = Lock
    { _fromLock :: MVar m ()
    }
  deriving (Typeable)

instance (Eq (MVar m ())) => Eq (Lock m) where
  (==) (Lock a) (Lock b) = a == b

--------------------------------------------------------------------------------

-- | Create a lock in the \"unlocked\" state.
newLock :: (MonadConc m) => m (Lock m)
newLock = Lock <$> MVar.newMVar ()

-- | Create a lock in the \"locked\" state.
newAcquired :: (MonadConc m) => m (Lock m)
newAcquired = Lock <$> MVar.newEmptyMVar

--------------------------------------------------------------------------------

-- |
-- Acquires the 'Lock'. Blocks if another thread has acquired the 'Lock'.
--
-- @acquire@ behaves as follows:
--
-- * When the state is \"unlocked\" @acquire@ changes the state to \"locked\".
--
-- * When the state is \"locked\" @acquire@ /blocks/ until a call to 'release'
--   in another thread wakes the calling thread. Upon awakening it will change
--   the state to \"locked\".
--
-- There are two further important properties of @acquire@:
--
-- * @acquire@ is single-wakeup. That is, if there are multiple threads blocked
--   on @acquire@ and the lock is released, only one thread will be woken up.
--   The runtime guarantees that the woken thread completes its @acquire@
--   operation.
--
-- * When multiple threads are blocked on @acquire@, they are woken up in FIFO
--   order. This is useful for providing fairness properties of abstractions
--   built using locks. Note that this differs from the Python implementation
--   where the wake-up order is undefined.
acquire :: (MonadConc m) => Lock m -> m ()
acquire = MVar.takeMVar . _fromLock

-- |
-- A non-blocking 'acquire'.
--
-- * When the state is \"unlocked\" @tryAcquire@ changes the state to \"locked\"
--   and returns 'True'.
--
-- * When the state is \"locked\" @tryAcquire@ leaves the state unchanged and
--   returns 'False'.
tryAcquire :: (MonadConc m) => Lock m -> m Bool
tryAcquire = fmap isJust . MVar.tryTakeMVar . _fromLock

-- |
-- @release@ changes the state to \"unlocked\" and returns immediately.
--
-- Note that it is an error to release a lock in the \"unlocked\" state!
--
-- If there are any threads blocked on 'acquire' the thread that first called
-- @acquire@ will be woken up.
release :: (MonadConc m) => Lock m -> m ()
release (Lock mv) = do
  b <- MVar.tryPutMVar mv ()
  when (not b) $
    error "Control.Concurrent.Classy.Lock.release: cannot release an unlocked Lock!"

--------------------------------------------------------------------------------

-- |
-- A convenience function which first acquires the lock and then performs the
-- computation. When the computation terminates, whether normally or by raising an
-- exception, the lock is released.
--
-- Note that: @with = 'bracket_' '<$>' 'acquire' '<*>' 'release'@.
with :: (MonadConc m) => Lock m -> m a -> m a
with = bracket_ <$> acquire <*> release

-- |
-- A non-blocking 'with'. @tryWith@ is a convenience function which first tries
-- to acquire the lock. If that fails, 'Nothing' is returned. If it succeeds,
-- the computation is performed. When the computation terminates, whether
-- normally or by raising an exception, the lock is released and 'Just' the
-- result of the computation is returned.
tryWith :: (MonadConc m) => Lock m -> m a -> m (Maybe a)
tryWith l a = mask $ \restore -> do
  acquired <- tryAcquire l
  if acquired
    then do r <- restore a `onException` release l
            release l
            pure (Just r)
    else pure Nothing

-- |
-- * When the state is \"locked\", @wait@ /blocks/ until a call to 'release'
--   in another thread changes it to \"unlocked\".
--
-- * @wait@ is multiple-wakeup, so when multiple waiters are blocked on
--   a @Lock@, all of them are woken up at the same time.
--
-- * When the state is \"unlocked\" @wait@ returns immediately.
--
-- @wait@ does not alter the state of the lock.
wait :: (MonadConc m) => Lock m -> m ()
wait (Lock mv) = MVar.readMVar mv

--------------------------------------------------------------------------------

-- |
-- Determines if the lock is in the \"locked\" state.
--
-- Note that this is only a snapshot of the state. By the time a program reacts
-- on its result it may already be out of date.
locked :: (MonadConc m) => Lock m -> m Bool
locked = MVar.isEmptyMVar . _fromLock

--------------------------------------------------------------------------------

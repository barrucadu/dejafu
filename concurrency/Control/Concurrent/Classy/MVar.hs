-- |
-- Module      : Control.Concurrent.Classy.MVar
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : portable
--
-- An @'MVar' t@ is mutable location that is either empty or contains
-- a value of type @t@.  It has two fundamental operations: 'putMVar'
-- which fills an 'MVar' if it is empty and blocks otherwise, and
-- 'takeMVar' which empties an 'MVar' if it is full and blocks
-- otherwise.  They can be used in multiple different ways:
--
--   1. As synchronized mutable variables,
--
--   2. As channels, with 'takeMVar' and 'putMVar' as receive and
--      send, and
--
--   3. As a binary semaphore @'MVar' ()@, with 'takeMVar' and
--      'putMVar' as wait and signal.
--
-- __Deviations:__ There is no @Eq@ instance for @MonadConc@ the
-- @MVar@ type. Furthermore, the @mkWeakMVar@ and @addMVarFinalizer@
-- functions are not provided. Finally, normal @MVar@s have a fairness
-- guarantee, which dejafu does not currently make use of when
-- generating schedules to test, so your program may be tested with
-- /unfair/ schedules.
module Control.Concurrent.Classy.MVar
 ( -- *@MVar@s
  MVar
 , newEmptyMVar
 , newEmptyMVarN
 , newMVar
 , newMVarN
 , takeMVar
 , putMVar
 , readMVar
 , swapMVar
 , tryTakeMVar
 , tryPutMVar
 , isEmptyMVar
 , withMVar
 , withMVarMasked
 , modifyMVar_
 , modifyMVar
 , modifyMVarMasked_
 , modifyMVarMasked
 ) where

import           Control.Monad.Catch      (onException)
import           Control.Monad.Conc.Class
import           Data.Maybe               (isJust)

-- | Swap the contents of a @MVar@, and return the value taken. This
-- function is atomic only if there are no other producers fro this
-- @MVar@.
--
-- @since 1.0.0.0
swapMVar :: MonadConc m => MVar m a -> a -> m a
swapMVar cvar a = mask_ $ do
  old <- takeMVar cvar
  putMVar cvar a
  pure old

-- | Check if a @MVar@ is empty.
--
-- The boolean value returned is just a snapshot of the state of the
-- @MVar@, it may have been emptied (or filled) by the time you
-- actually access it. Generally prefer 'tryPutMVar', 'tryTakeMVar',
-- and 'tryReadMVar'.
--
-- @since 1.0.0.0
isEmptyMVar :: MonadConc m => MVar m a -> m Bool
isEmptyMVar = fmap isJust . tryReadMVar

-- | Operate on the contents of a @MVar@, replacing the contents after
-- finishing. This operation is exception-safe: it will replace the
-- original contents of the @MVar@ if an exception is raised. However,
-- it is only atomic if there are no other producers for this @MVar@.
--
-- @since 1.0.0.0
{-# INLINE withMVar #-}
withMVar :: MonadConc m => MVar m a -> (a -> m b) -> m b
withMVar cvar f = mask $ \restore -> do
  val <- takeMVar cvar
  out <- restore (f val) `onException` putMVar cvar val
  putMVar cvar val

  pure out

-- | Like 'withMVar', but the @IO@ action in the second argument is
-- executed with asynchronous exceptions masked.
--
-- @since 1.0.0.0
{-# INLINE withMVarMasked #-}
withMVarMasked :: MonadConc m => MVar m a -> (a -> m b) -> m b
withMVarMasked cvar f = mask_ $ do
  val <- takeMVar cvar
  out <- f val `onException` putMVar cvar val
  putMVar cvar val

  pure out

-- | An exception-safe wrapper for modifying the contents of a @MVar@.
-- Like 'withMVar', 'modifyMVar' will replace the original contents of
-- the @MVar@ if an exception is raised during the operation. This
-- function is only atomic if there are no other producers for this
-- @MVar@.
--
-- @since 1.0.0.0
{-# INLINE modifyMVar_ #-}
modifyMVar_ :: MonadConc m => MVar m a -> (a -> m a) -> m ()
modifyMVar_ cvar f = modifyMVar cvar $ fmap (\a -> (a,())) . f

-- | A slight variation on 'modifyMVar_' that allows a value to be
-- returned (@b@) in addition to the modified value of the @MVar@.
--
-- @since 1.0.0.0
{-# INLINE modifyMVar #-}
modifyMVar :: MonadConc m => MVar m a -> (a -> m (a, b)) -> m b
modifyMVar cvar f = mask $ \restore -> do
  val <- takeMVar cvar
  (val', out) <- restore (f val) `onException` putMVar cvar val
  putMVar cvar val'
  pure out

-- | Like 'modifyMVar_', but the @IO@ action in the second argument is
-- executed with asynchronous exceptions masked.
--
-- @since 1.0.0.0
{-# INLINE modifyMVarMasked_ #-}
modifyMVarMasked_ :: MonadConc m => MVar m a -> (a -> m a) -> m ()
modifyMVarMasked_ cvar f = modifyMVarMasked cvar $ fmap (\a -> (a,())) . f

-- | Like 'modifyMVar', but the @IO@ action in the second argument is
-- executed with asynchronous exceptions masked.
--
-- @since 1.0.0.0
{-# INLINE modifyMVarMasked #-}
modifyMVarMasked :: MonadConc m => MVar m a -> (a -> m (a, b)) -> m b
modifyMVarMasked cvar f = mask_ $ do
  val <- takeMVar cvar
  (val', out) <- f val `onException` putMVar cvar val
  putMVar cvar val'
  pure out

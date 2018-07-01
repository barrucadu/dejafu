-- |
-- Module      : Control.Concurrent.Classy.IORef
-- Copyright   : (c) 2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : portable
--
-- Mutable references in a concurrency monad.
--
-- __Deviations:__ There is no @Eq@ instance for @MonadConc@ the
-- @IORef@ type. Furthermore, the @mkWeakIORef@ function is not
-- provided.
module Control.Concurrent.Classy.IORef
  ( -- * IORefs
    newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  , modifyIORef'
  , atomicModifyIORef
  , atomicModifyIORef'
  , atomicWriteIORef

  -- * Memory Model

  -- | In a concurrent program, @IORef@ operations may appear
  -- out-of-order to another thread, depending on the memory model of
  -- the underlying processor architecture. For example, on x86 (which
  -- uses total store order), loads can move ahead of stores. Consider
  -- this example:
  --
  -- > iorefs :: MonadConc m => m (Bool, Bool)
  -- > iorefs = do
  -- >   r1 <- newIORef False
  -- >   r2 <- newIORef False
  -- >
  -- >   x <- spawn $ writeIORef r1 True >> readIORef r2
  -- >   y <- spawn $ writeIORef r2 True >> readIORef r1
  -- >
  -- >   (,) <$> readMVar x <*> readMVar y
  --
  -- Under a sequentially consistent memory model the possible results
  -- are @(True, True)@, @(True, False)@, and @(False, True)@. Under
  -- total or partial store order, @(False, False)@ is also a possible
  -- result, even though there is no interleaving of the threads which
  -- can lead to this.
  --
  -- We can see this by testing with different memory models:
  --
  -- > > autocheckWay defaultWay SequentialConsistency relaxed
  -- > [pass] Never Deadlocks
  -- > [pass] No Exceptions
  -- > [fail] Consistent Result
  -- >        (False,True) S0---------S1----S0--S2----S0--
  -- >
  -- >        (True,True) S0---------S1-P2----S1---S0---
  -- >
  -- >        (True,False) S0---------S2----S1----S0---
  -- > False
  --
  -- > > autocheckWay defaultWay TotalStoreOrder  relaxed
  -- > [pass] Never Deadlocks
  -- > [pass] No Exceptions
  -- > [fail] Consistent Result
  -- >         (False,True) S0---------S1----S0--S2----S0--
  -- >
  -- >         (False,False) S0---------S1--P2----S1--S0---
  -- >
  -- >         (True,False) S0---------S2----S1----S0---
  -- >
  -- >         (True,True) S0---------S1-C-S2----S1---S0---
  -- > False
  --
  -- Traces for non-sequentially-consistent memory models show where
  -- writes to @IORef@s are /committed/, which makes a write visible to
  -- all threads rather than just the one which performed the
  -- write. Only 'writeIORef' is broken up into separate write and
  -- commit steps, 'atomicModifyIORef' is still atomic and imposes a
  -- memory barrier.
  ) where

import           Control.Monad.Conc.Class

-- | Mutate the contents of a @IORef@.
--
-- Be warned that 'modifyIORef' does not apply the function strictly.
-- This means if the program calls 'modifyIORef' many times, but
-- seldomly uses the value, thunks will pile up in memory resulting in
-- a space leak. This is a common mistake made when using a @IORef@ as
-- a counter. For example, the following will likely produce a stack
-- overflow:
--
-- >ref <- newIORef 0
-- >replicateM_ 1000000 $ modifyIORef ref (+1)
-- >readIORef ref >>= print
--
-- To avoid this problem, use 'modifyIORef'' instead.
--
-- @since unreleased
modifyIORef :: MonadConc m => IORef m a -> (a -> a) -> m ()
modifyIORef ref f = readIORef ref >>= writeIORef ref . f

-- | Strict version of 'modifyIORef'
--
-- @since unreleased
modifyIORef' :: MonadConc m => IORef m a -> (a -> a) -> m ()
modifyIORef' ref f = do
  x <- readIORef ref
  writeIORef ref $! f x

-- | Strict version of 'atomicModifyIORef'. This forces both the value
-- stored in the @IORef@ as well as the value returned.
--
-- @since unreleased
atomicModifyIORef' :: MonadConc m => IORef m a -> (a -> (a,b)) -> m b
atomicModifyIORef' ref f = do
  b <- atomicModifyIORef ref $ \a -> case f a of
    v@(a',_) -> a' `seq` v
  pure $! b

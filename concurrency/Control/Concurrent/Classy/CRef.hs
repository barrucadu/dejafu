-- |
-- Module      : Control.Concurrent.Classy.CRef
-- Copyright   : (c) 2016--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Deprecated re-exports of @IORef@ functions under the old @CRef@
-- names.
module Control.Concurrent.Classy.CRef {-# DEPRECATED "Import Control.Concurrent.Classy.IORef instead" #-}
  ( -- * CRefs
    CRef
  , newCRef
  , newCRefN
  , readCRef
  , writeCRef
  , modifyCRef
  , modifyCRef'
  , atomicModifyCRef
  , atomicModifyCRef'
  , atomicWriteCRef

  -- ** Compare-and-swap
  , casCRef
  , modifyCRefCAS
  , modifyCRefCAS_

  -- * Memory Model

  -- | In a concurrent program, @CRef@ operations may appear
  -- out-of-order to another thread, depending on the memory model of
  -- the underlying processor architecture. For example, on x86 (which
  -- uses total store order), loads can move ahead of stores. Consider
  -- this example:
  --
  -- > crefs :: MonadConc m => m (Bool, Bool)
  -- > crefs = do
  -- >   r1 <- newCRef False
  -- >   r2 <- newCRef False
  -- >
  -- >   x <- spawn $ writeCRef r1 True >> readCRef r2
  -- >   y <- spawn $ writeCRef r2 True >> readCRef r1
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
  -- writes to @CRef@s are /committed/, which makes a write visible to
  -- all threads rather than just the one which performed the
  -- write. Only 'writeCRef' is broken up into separate write and
  -- commit steps, 'atomicModifyCRef' is still atomic and imposes a
  -- memory barrier.
  ) where

import qualified Control.Concurrent.Classy.IORef as IORef
import           Control.Monad.Conc.Class        (IORef, MonadConc, Ticket)
import qualified Control.Monad.Conc.Class        as IORef

-- | Type alias for 'IORef'.
type CRef m a = IORef m a
{-# DEPRECATED CRef "Use IORef instead" #-}

-- | Create a new reference.
newCRef :: MonadConc m => a -> m (CRef m a)
newCRef = IORef.newIORef
{-# DEPRECATED newCRef "Use newIORef instead" #-}

-- | Create a new reference, but it is given a name which may be used
-- to present more useful debugging information.
newCRefN :: MonadConc m => String -> a -> m (CRef m a)
newCRefN = IORef.newIORefN
{-# DEPRECATED newCRefN "Use newIORefN instead" #-}

-- | Read the current value stored in a reference.
readCRef :: MonadConc m => CRef m a -> m a
readCRef = IORef.readIORef
{-# DEPRECATED readCRef "Use readIORef instead" #-}

-- | Write a new value into an @CRef@, without imposing a memory
-- barrier. This means that relaxed memory effects can be observed.
writeCRef :: MonadConc m => CRef m a -> a -> m ()
writeCRef = IORef.writeIORef
{-# DEPRECATED writeCRef "Use writeIORef instead" #-}

-- | Mutate the contents of a @CRef@.
--
-- Be warned that 'modifyCRef' does not apply the function strictly.
-- This means if the program calls 'modifyCRef' many times, but
-- seldomly uses the value, thunks will pile up in memory resulting in
-- a space leak.
modifyCRef :: MonadConc m => CRef m a -> (a -> a) -> m ()
modifyCRef = IORef.modifyIORef
{-# DEPRECATED modifyCRef "Use modifyIORef instead" #-}

-- | Strict version of 'modifyCRef'
modifyCRef' :: MonadConc m => CRef m a -> (a -> a) -> m ()
modifyCRef' = IORef.modifyIORef'
{-# DEPRECATED modifyCRef' "Use modifyIORef' instead" #-}

-- | Atomically modify the value stored in a reference. This imposes
-- a full memory barrier.
atomicModifyCRef :: MonadConc m => CRef m a -> (a -> (a, b)) -> m b
atomicModifyCRef = IORef.atomicModifyIORef
{-# DEPRECATED atomicModifyCRef "Use atomicModifyIORef instead" #-}

-- | Strict version of 'atomicModifyCRef'. This forces both the value
-- stored in the @CRef@ as well as the value returned.
atomicModifyCRef' :: MonadConc m => CRef m a -> (a -> (a,b)) -> m b
atomicModifyCRef' = IORef.atomicModifyIORef'
{-# DEPRECATED atomicModifyCRef' "Use atomicModifyIORef' instead" #-}

-- | Replace the value stored in a reference, with the
-- barrier-to-reordering property that 'atomicModifyIORef' has.
atomicWriteCRef :: MonadConc m => CRef m a -> a -> m ()
atomicWriteCRef = IORef.atomicWriteIORef
{-# DEPRECATED atomicWriteCRef "Use atomicWriteIORef instead" #-}

-- | Perform a machine-level compare-and-swap (CAS) operation on a
-- @CRef@. Returns an indication of success and a @Ticket@ for the
-- most current value in the @CRef@.
-- This is strict in the \"new\" value argument.
casCRef :: MonadConc m => CRef m a -> Ticket m a -> a -> m (Bool, Ticket m a)
casCRef = IORef.casIORef
{-# DEPRECATED casCRef "Use casIORef instead" #-}

-- | A replacement for 'atomicModifyCRef' using a compare-and-swap.
--
-- This is strict in the \"new\" value argument.
modifyCRefCAS :: MonadConc m => CRef m a -> (a -> (a, b)) -> m b
modifyCRefCAS = IORef.modifyIORefCAS
{-# DEPRECATED modifyCRefCAS "Use modifyIORefCAS instead" #-}

-- | A variant of 'modifyCRefCAS' which doesn't return a result.
modifyCRefCAS_ :: MonadConc m => CRef m a -> (a -> a) -> m ()
modifyCRefCAS_ = IORef.modifyIORefCAS_
{-# DEPRECATED modifyCRefCAS_ "Use modifyIORefCAS_ instead" #-}

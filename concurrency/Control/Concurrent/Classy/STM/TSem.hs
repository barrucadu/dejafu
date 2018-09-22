-- |
-- Module      : Control.Concurrent.Classy.STM.TSem
-- Copyright   : (c) 2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : portable
--
-- 'TSem': transactional semaphores.
--
-- __Deviations:__ There is no @Eq@ instance for @TSem@ type.
module Control.Concurrent.Classy.STM.TSem
  ( TSem
  , newTSem
  , waitTSem
  , signalTSem
  , signalTSemN
  ) where

import           Control.Monad           (when)
import           Control.Monad.STM.Class
import           Numeric.Natural         (Natural)

-- | 'TSem' is a transactional semaphore.  It holds a certain number
-- of units, and units may be acquired or released by 'waitTSem' and
-- 'signalTSem' respectively.  When the 'TSem' is empty, 'waitTSem'
-- blocks.
--
-- Note that 'TSem' has no concept of fairness, and there is no
-- guarantee that threads blocked in `waitTSem` will be unblocked in
-- the same order; in fact they will all be unblocked at the same time
-- and will fight over the 'TSem'.  Hence 'TSem' is not suitable if
-- you expect there to be a high number of threads contending for the
-- resource.  However, like other STM abstractions, 'TSem' is
-- composable.
--
-- @since 1.6.1.0
newtype TSem stm = TSem (TVar stm Integer)

-- | Construct new 'TSem' with an initial counter value.
--
-- A positive initial counter value denotes availability of
-- units 'waitTSem' can acquire.
--
-- The initial counter value can be negative which denotes a resource
-- \"debt\" that requires a respective amount of 'signalTSem'
-- operations to counter-balance.
--
-- @since 1.6.1.0
newTSem :: MonadSTM stm => Integer -> stm (TSem stm)
newTSem i = fmap TSem (newTVar $! i)

-- | Wait on 'TSem' (aka __P__ operation).
--
-- This operation acquires a unit from the semaphore (i.e. decreases
-- the internal counter) and blocks (via 'retry') if no units are
-- available (i.e. if the counter is /not/ positive).
--
-- @since 2.4.2
waitTSem :: MonadSTM stm => TSem stm -> stm ()
waitTSem (TSem t) = do
  i <- readTVar t
  when (i <= 0) retry
  writeTVar t $! (i-1)

-- | Signal a 'TSem' (aka __V__ operation).
--
-- This operation adds\/releases a unit back to the semaphore
-- (i.e. increments the internal counter).
--
-- @since 1.6.1.0
signalTSem :: MonadSTM stm => TSem stm -> stm ()
signalTSem (TSem t) = do
  i <- readTVar t
  writeTVar t $! i+1

-- | Multi-signal a 'TSem'
--
-- This operation adds\/releases multiple units back to the semaphore
-- (i.e. increments the internal counter).
--
-- > signalTSem == signalTSemN 1
--
-- @since 1.6.1.0
signalTSemN :: MonadSTM stm => Natural -> TSem stm -> stm ()
signalTSemN 0 _ = pure ()
signalTSemN 1 s = signalTSem s
signalTSemN n (TSem t) = do
  i <- readTVar t
  writeTVar t $! i + toInteger n

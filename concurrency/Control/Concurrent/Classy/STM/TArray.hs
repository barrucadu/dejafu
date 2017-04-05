{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module      : Control.Concurrent.Classy.STM.
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : FlexibleInstances, MultiParamTypeClasses
--
-- TArrays: transactional arrays, for use in STM-like monads.
--
-- __Deviations:__ @TArray@ as defined here does not have an @Eq@
-- instance, this is because the @MonadSTM@ @TVar@ type does not have
-- an @Eq@ constraint.
module Control.Concurrent.Classy.STM.TArray (TArray) where

import Data.Array (Array, bounds)
import Data.Array.Base (listArray, arrEleBottom, unsafeAt, MArray(..),
                        IArray(numElements))
import Data.Ix (rangeSize)

import Control.Monad.STM.Class

-- | @TArray@ is a transactional array, supporting the usual 'MArray'
-- interface for mutable arrays.
--
-- It is currently implemented as @Array ix (TVar stm e)@, but it may
-- be replaced by a more efficient implementation in the future (the
-- interface will remain the same, however).
--
-- @since 1.0.0.0
newtype TArray stm i e = TArray (Array i (TVar stm e))

-- | @since 1.0.0.0
instance MonadSTM stm => MArray (TArray stm) e stm where
  getBounds (TArray a) = pure (bounds a)

  newArray b e = do
    a <- rep (rangeSize b) (newTVar e)
    pure $ TArray (listArray b a)

  newArray_ b = newArray b arrEleBottom

  unsafeRead  (TArray a) = readTVar  . unsafeAt a
  unsafeWrite (TArray a) = writeTVar . unsafeAt a

  getNumElements (TArray a) = pure (numElements a)

-- | Like 'replicateM' but uses an accumulator to prevent stack overflows.
-- Unlike 'replicateM' the returned list is in reversed order.  This
-- doesn't matter though since this function is only used to create
-- arrays with identical elements.
rep :: Monad m => Int -> m a -> m [a]
rep n m = go n [] where
  go 0 xs = pure xs
  go i xs = do
    x <- m
    go (i-1) (x:xs)

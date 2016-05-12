{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Test.DejaFu.Internal
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : RankNTypes
--
-- Dealing with mutable state. This module is NOT considered to form
-- part of the public interface of this library.
module Test.DejaFu.Internal where

import Control.Monad.ST (ST)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

class Monad m => MonadRef r m | m -> r where
  newRef :: a -> m (r a)
  readRef :: r a -> m a
  writeRef :: r a -> a -> m ()

instance MonadRef (STRef t) (ST t) where
  newRef   = newSTRef
  readRef  = readSTRef
  writeRef = writeSTRef

instance MonadRef IORef IO where
  newRef   = newIORef
  readRef  = readIORef
  writeRef = writeIORef

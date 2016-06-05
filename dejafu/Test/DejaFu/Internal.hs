{-# LANGUAGE RankNTypes #-}

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

-- | Mutable references.
data Ref n r m = Ref
  { newRef   :: forall a. a -> n (r a)
  , readRef  :: forall a. r a -> n a
  , writeRef :: forall a. r a -> a -> n ()
  , liftN    :: forall a. n a -> m a
  }

-- | Method dict for 'ST'.
refST :: (forall a. ST t a -> m a) -> Ref (ST t) (STRef t) m
refST lftN = Ref
  { newRef   = newSTRef
  , readRef  = readSTRef
  , writeRef = writeSTRef
  , liftN    = lftN
  }

-- | Method dict for 'IO'.
refIO :: (forall a. IO a -> m a) -> Ref IO IORef m
refIO lftN = Ref
  { newRef   = newIORef
  , readRef  = readIORef
  , writeRef = writeIORef
  , liftN    = lftN
  }

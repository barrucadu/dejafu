{-# LANGUAGE Rank2Types #-}

-- | Dealing with mutable state.
module Control.State where

import Control.Monad.ST (ST)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

-- | Mutable references.
data Ref n r = Ref
  { newRef   :: forall a. a -> n (r a)
  , readRef  :: forall a. r a -> n a
  , writeRef :: forall a. r a -> a -> n ()
  }

-- | Method dict for 'ST'.
refST :: Ref (ST t) (STRef t)
refST = Ref
  { newRef   = newSTRef
  , readRef  = readSTRef
  , writeRef = writeSTRef
  }

-- | Method dict for 'IO'.
refIO :: Ref IO IORef
refIO = Ref
  { newRef   = newIORef
  , readRef  = readIORef
  , writeRef = writeIORef
  }

-- | Wrapped mutable references.
data Wrapper n r m = Wrapper
  { wref :: Ref n r
  , liftN :: forall a. n a -> m a
  }

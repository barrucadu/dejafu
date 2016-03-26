-- | Unbounded, element-strict channels.
-- 
-- Elements will be evaluated to normal form on entering the
-- channel. For some concurrency applications, this is more desirable
-- than passing an unevaluated thunk through the channel.
--
-- The @getChanContents@ function from Control.Concurrent.Chan.Strict
-- is not provided as it relies on @unsafeInterleaveIO@.
module Control.Concurrent.Classy.Chan.Strict
  ( -- * The 'Chan' type
    Chan

  -- * Operations
  , newChan
  , writeChan
  , readChan
  , dupChan

  -- * Stream interface
  , writeList2Chan
  ) where

import Control.Concurrent.Classy.Chan (Chan)
import Control.DeepSeq (NFData, ($!!))
import Control.Monad.Conc.Class (MonadConc)

import qualified Control.Concurrent.Classy.Chan as Chan

-- | Build and returns a new instance of 'Chan'.
newChan :: (MonadConc m, NFData a) => m (Chan m a)
newChan = Chan.newChan

-- | Write a value to a 'Chan'.
writeChan :: (MonadConc m, NFData a) => Chan m a -> a -> m ()
writeChan chan val = Chan.writeChan chan $!! val

-- | Read the next value from the 'Chan'.
readChan :: (MonadConc m, NFData a) => Chan m a -> m a
readChan = Chan.readChan

-- | Duplicate a 'Chan': the duplicate channel begins empty, but data
-- written to either channel from then on will be available from both.
-- Hence this creates a kind of broadcast channel, where data written
-- by anyone is seen by everyone else.
dupChan :: (MonadConc m, NFData a) => Chan m a -> m (Chan m a)
dupChan = Chan.dupChan

-- | Write an entire list of items to a 'Chan'.
writeList2Chan :: (MonadConc m, NFData a) => Chan m a -> [a] -> m ()
writeList2Chan = mapM_ . writeChan

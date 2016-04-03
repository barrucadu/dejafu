-- | Unbounded channels.
--
-- __Deviations:__ @Chan@ as defined here does not have an @Eq@
-- instance, this is because the @MonadConc@ @MVar@ type does not have
-- an @Eq@ constraint. The deprecated @unGetChan@ and @isEmptyCHan@
-- functions are not provided. Furthermore, the @getChanContents@
-- function is not provided as it needs unsafe I/O.
module Control.Concurrent.Classy.Chan
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

import Control.Concurrent.Classy.MVar
import Control.Monad.Catch (mask_)
import Control.Monad.Conc.Class (MonadConc)

-- | 'Chan' is an abstract type representing an unbounded FIFO
-- channel.
data Chan m a
  = Chan (MVar m (Stream m a))
         (MVar m (Stream m a)) -- Invariant: the Stream a is always an empty MVar

type Stream m a = MVar m (ChItem m a)

data ChItem m a = ChItem a (Stream m a)

-- | Build and returns a new instance of 'Chan'.
newChan :: MonadConc m => m (Chan m a)
newChan = do
  hole  <- newEmptyMVar
  readVar  <- newMVar hole
  writeVar <- newMVar hole
  pure (Chan readVar writeVar)

-- | Write a value to a 'Chan'.
writeChan :: MonadConc m => Chan m a -> a -> m ()
writeChan (Chan _ writeVar) val = do
  new_hole <- newEmptyMVar
  mask_ $ do
    old_hole <- takeMVar writeVar
    putMVar old_hole (ChItem val new_hole)
    putMVar writeVar new_hole

-- | Read the next value from the 'Chan'.
readChan :: MonadConc m => Chan m a -> m a
readChan (Chan readVar _) =  modifyMVarMasked readVar $ \read_end -> do
  (ChItem val new_read_end) <- readMVar read_end
  pure (new_read_end, val)

-- | Duplicate a 'Chan': the duplicate channel begins empty, but data
-- written to either channel from then on will be available from both.
-- Hence this creates a kind of broadcast channel, where data written
-- by anyone is seen by everyone else.
dupChan :: MonadConc m => Chan m a -> m (Chan m a)
dupChan (Chan _ writeVar) = do
  hole       <- readMVar writeVar
  newReadVar <- newMVar hole
  pure (Chan newReadVar writeVar)

-- | Write an entire list of items to a 'Chan'.
writeList2Chan :: MonadConc m => Chan m a -> [a] -> m ()
writeList2Chan = mapM_ . writeChan

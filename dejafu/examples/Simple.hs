{-# language NoMonomorphismRestriction #-}

import Test.DejaFu (autocheck, autocheck', MemType )

import DejaFu.Control.Concurrent (forkIO)
import DejaFu.Control.Concurrent.STM (newTVar, readTVar, writeTVar, atomically)

import Control.Monad ( void, forM )

main = do
  autocheck stmAtomic

-- | Transactions are atomic.
-- dejafu type: stmAtomic :: MonadConc m => m Int
stmAtomic = do
  x <- atomically $ newTVar (0::Int)
  forM [1..3] $ \ _ -> forkIO . atomically $ do
      writeTVar x 1
      writeTVar x 2
  atomically $ readTVar x

  

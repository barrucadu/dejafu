{-# language NoMonomorphismRestriction #-}

import Test.DejaFu (autocheck, autocheck', MemType )

import DejaFu.Control.Concurrent (forkIO)
import DejaFu.Control.Concurrent.MVar ( newEmptyMVar, newMVar, takeMVar, putMVar )

-- import Control.Concurrent ( forkIO)
-- import Control.Concurrent.MVar ( newEmptyMVar, newMVar, takeMVar, putMVar )

import Control.Monad ( void, forM, forever )
import Prelude hiding ( take, drop )
import System.IO ( hSetBuffering , stdout, BufferMode(..))

main = do
  -- hSetBuffering stdout LineBuffering ; dining
  autocheck dining

-- | deadlocking non-solution for dining philosophers
dining = do
  let n = 2
  fs <- forM [1..n] $ \ _ -> newMVar ()
  let left p = fs !! (p-1) ; right p = fs !! (mod p n)
  forM [1..n] $ \ p ->
    (if p < n then void . forkIO else id ) $ forever $ do
      -- putStrLn $ "philo " ++ show p ++ " thinking"
      take $ left p ; take $ right p
      -- putStrLn $ "philo " ++ show p ++ " eating"
      drop $ left p ; drop $ right p
  return ()

take f = takeMVar f
drop f = putMVar f ()

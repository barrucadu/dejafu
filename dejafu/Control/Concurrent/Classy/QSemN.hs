-- | Quantity semaphores in which each thread may wait for an arbitrary
-- \"amount\".
module Control.Concurrent.Classy.QSemN
  ( -- * General Quantity Semaphores
    QSemN
  , newQSemN
  , waitQSemN
  , signalQSemN
  ) where

import Control.Monad.Conc.Class (MonadConc)
import Control.Concurrent.Classy.MVar
import Control.Monad.Catch (mask_, onException, uninterruptibleMask_)
import Data.Maybe

-- | 'QSemN' is a quantity semaphore in which the resource is aqcuired
-- and released in units of one. It provides guaranteed FIFO ordering
-- for satisfying blocked `waitQSemN` calls.
--
-- The pattern
--
-- > bracket_ (waitQSemN n) (signalQSemN n) (...)
--
-- is safe; it never loses any of the resource.
newtype QSemN m = QSemN (MVar m (Int, [(Int, MVar m ())], [(Int, MVar m ())]))

-- | Build a new 'QSemN' with a supplied initial quantity.
--  The initial quantity must be at least 0.
newQSemN :: MonadConc m => Int -> m (QSemN m)
newQSemN initial
  | initial < 0 = fail "newQSemN: Initial quantity must be non-negative"
  | otherwise   = QSemN <$> newMVar (initial, [], [])

-- | Wait for the specified quantity to become available
waitQSemN :: MonadConc m => QSemN m -> Int -> m ()
waitQSemN (QSemN m) sz = mask_ $ do
  (quantity, b1, b2) <- takeMVar m
  let remaining = quantity - sz
  if remaining < 0
  -- Enqueue and block the thread
  then do
    b <- newEmptyMVar
    putMVar m (quantity, b1, (sz,b):b2)
    wait b
  -- Claim the resource
  else
    putMVar m (remaining, b1, b2)

  where
    wait b = takeMVar b `onException` uninterruptibleMask_ (do
      (quantity, b1, b2) <- takeMVar m
      r  <- tryTakeMVar b
      r' <- if isJust r
           then signal sz (quantity, b1, b2)
           else putMVar b () >> pure (quantity, b1, b2)
      putMVar m r')

-- | Signal that a given quantity is now available from the 'QSemN'.
signalQSemN :: MonadConc m => QSemN m -> Int -> m ()
signalQSemN (QSemN m) sz = uninterruptibleMask_ $ do
  r  <- takeMVar m
  r' <- signal sz r
  putMVar m r'

-- | Fix the queue and signal as many threads as we can.
signal :: MonadConc m
  => Int
  -> (Int, [(Int,MVar m ())], [(Int,MVar m ())])
  -> m (Int, [(Int,MVar m ())], [(Int,MVar m ())])
signal sz0 (i,a1,a2) = loop (sz0 + i) a1 a2 where
  -- No more resource left, done.
  loop 0 bs b2 = pure (0,  bs, b2)

  -- Fix the queue
  loop sz [] [] = pure (sz, [], [])
  loop sz [] b2 = loop sz (reverse b2) []

  -- Signal as many threads as there is enough resource to satisfy,
  -- stopping as soon as one thread requires more resource than there
  -- is.
  loop sz ((j,b):bs) b2
    | j > sz = do
      r <- isEmptyMVar b
      if r then pure (sz, (j,b):bs, b2)
           else loop sz bs b2
    | otherwise = do
      r <- tryPutMVar b ()
      if r then loop (sz-j) bs b2
           else loop sz bs b2

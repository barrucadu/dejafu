-- | Simple quantity semaphores.
module Control.Concurrent.Classy.QSem
  ( -- * Simple Quantity Semaphores
    QSem
  , newQSem
  , waitQSem
  , signalQSem
  ) where

import Control.Concurrent.Classy.QSemN
import Control.Monad.Conc.Class (MonadConc)

-- | @QSem@ is a quantity semaphore in which the resource is acquired
-- and released in units of one. It provides guaranteed FIFO ordering
-- for satisfying blocked 'waitQSem' calls.
--
-- The pattern
--
-- > bracket_ qaitQSem signalSSem (...)
--
-- is safe; it never loses a unit of the resource.
newtype QSem m = QSem (QSemN m)

-- | Build a new 'QSem' with a supplied initial quantity. The initial
-- quantity must be at least 0.
newQSem :: MonadConc m => Int -> m (QSem m)
newQSem initial
  | initial < 0 = fail "newQSem: Initial quantity mus tbe non-negative."
  | otherwise   = QSem <$> newQSemN initial

-- | Wait for a unit to become available.
waitQSem :: MonadConc m => QSem m -> m ()
waitQSem (QSem qSemN) = waitQSemN qSemN 1

-- | Signal that a unit of the 'QSem' is available.
signalQSem :: MonadConc m => QSem m -> m ()
signalQSem (QSem qSemN) = signalQSemN qSemN 1

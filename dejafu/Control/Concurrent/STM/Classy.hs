-- | Classy software transactional memory.
module Control.Concurrent.STM.Classy
  ( module Control.Monad.STM.Class
  , module Control.Concurrent.STM.Classy.TVar
  , module Control.Concurrent.STM.Classy.TMVar
  , module Control.Concurrent.STM.Classy.TChan
  , module Control.Concurrent.STM.Classy.TQueue
  ) where

import Control.Monad.STM.Class
import Control.Concurrent.STM.Classy.TVar
import Control.Concurrent.STM.Classy.TMVar
import Control.Concurrent.STM.Classy.TChan
import Control.Concurrent.STM.Classy.TQueue

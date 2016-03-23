-- | Classy software transactional memory.
module Control.Concurrent.STM.Classy
  ( module Control.Monad.STM.Class
  , module Control.Concurrent.STM.Classy.TVar
  , module Control.Concurrent.STM.Classy.TMVar
  ) where

import Control.Monad.STM.Class
import Control.Concurrent.STM.Classy.TVar
import Control.Concurrent.STM.Classy.TMVar

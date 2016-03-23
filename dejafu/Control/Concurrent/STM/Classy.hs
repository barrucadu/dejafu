-- | Classy software transactional memory.
module Control.Concurrent.STM.Classy
  ( module Control.Monad.STM.Class
  , module Control.Concurrent.STM.Classy.CTVar
  , module Control.Concurrent.STM.Classy.CTMVar
  ) where

import Control.Monad.STM.Class
import Control.Concurrent.STM.Classy.CTVar
import Control.Concurrent.STM.Classy.CTMVar

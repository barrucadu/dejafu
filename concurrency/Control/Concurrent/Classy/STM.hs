-- |
-- Module      : Control.Concurrent.Classy.STM
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : non-portable
--
-- Classy software transactional memory.
module Control.Concurrent.Classy.STM
  ( module Control.Monad.STM.Class
  , module Control.Concurrent.Classy.STM.TVar
  , module Control.Concurrent.Classy.STM.TMVar
  , module Control.Concurrent.Classy.STM.TChan
  , module Control.Concurrent.Classy.STM.TQueue
  , module Control.Concurrent.Classy.STM.TBQueue
  , module Control.Concurrent.Classy.STM.TArray
  ) where

import Control.Monad.STM.Class
import Control.Concurrent.Classy.STM.TVar
import Control.Concurrent.Classy.STM.TMVar
import Control.Concurrent.Classy.STM.TChan
import Control.Concurrent.Classy.STM.TQueue
import Control.Concurrent.Classy.STM.TBQueue
import Control.Concurrent.Classy.STM.TArray

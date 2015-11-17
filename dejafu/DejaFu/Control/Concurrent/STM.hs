{-# language NoMonomorphismRestriction #-}

module DejaFu.Control.Concurrent.STM where

import Control.Monad.STM.Class
import Control.Monad.Conc.Class

newTVar = Control.Monad.STM.Class.newCTVar
readTVar = Control.Monad.STM.Class.readCTVar
writeTVar = Control.Monad.STM.Class.writeCTVar
atomically = Control.Monad.Conc.Class.atomically

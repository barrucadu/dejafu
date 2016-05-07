{-# language NoMonomorphismRestriction #-}

module DejaFu.Control.Concurrent where

import Control.Monad.Conc.Class

forkIO = Control.Monad.Conc.Class.fork

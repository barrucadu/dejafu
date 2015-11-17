{-# language NoMonomorphismRestriction #-}

module DejaFu.Control.Concurrent.MVar where

import Control.Concurrent.CVar 

newEmptyMVar = newEmptyCVar
newMVar = newCVar
takeMVar = takeCVar
putMVar = putCVar


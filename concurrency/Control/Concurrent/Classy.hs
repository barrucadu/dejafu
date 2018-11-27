{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Control.Concurrent.Classy
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : non-portable
--
-- Classy concurrency.
--
-- Concurrency is \"lightweight\", which means that both thread
-- creation and context switching overheads are extremely
-- low. Scheduling of Haskell threads is done internally in the
-- Haskell runtime system, and doesn't make use of any operating
-- system-supplied thread packages.
--
-- Haskell threads can communicate via @MVar@s, a kind of synchronised
-- mutable variable (see "Control.Concurrent.Classy.MVar"). Several
-- common concurrency abstractions can be built from @MVar@s, and
-- these are provided by the "Control.Concurrent.Classy"
-- library. Threads may also communicate via exceptions.
module Control.Concurrent.Classy
  ( module Control.Monad.Conc.Class
  , module Control.Concurrent.Classy.Chan
  , module Control.Concurrent.Classy.BoundedChan
  , module Control.Concurrent.Classy.CRef
  , module Control.Concurrent.Classy.IORef
  , module Control.Concurrent.Classy.MVar
  , module Control.Concurrent.Classy.STM
  , module Control.Concurrent.Classy.QSem
  , module Control.Concurrent.Classy.QSemN
  , module Control.Concurrent.Classy.Lock
  , module Control.Concurrent.Classy.RWLock
  ) where

import           Control.Concurrent.Classy.BoundedChan
import           Control.Concurrent.Classy.Chan
import           Control.Concurrent.Classy.CRef
import           Control.Concurrent.Classy.IORef
import           Control.Concurrent.Classy.Lock
import           Control.Concurrent.Classy.MVar
import           Control.Concurrent.Classy.QSem
import           Control.Concurrent.Classy.QSemN
import           Control.Concurrent.Classy.RWLock
import           Control.Concurrent.Classy.STM
import           Control.Monad.Conc.Class

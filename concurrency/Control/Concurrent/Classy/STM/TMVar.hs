-- |
-- Module      : Control.Concurrent.Classy.STM.TMVar
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : stable
-- Portability : portable
--
-- Transactional @MVar@s, for use with 'MonadSTM'.
--
-- __Deviations:__ @TMVar@ as defined here does not have an @Eq@
-- instance, this is because the @MonadSTM@ @TVar@ type does not have
-- an @Eq@ constraint. Furthermore, the @newTMVarIO@,
-- @newEmptyTMVarIO@, and @mkWeakTMVar@ functions are not provided.
module Control.Concurrent.Classy.STM.TMVar
  ( -- * @TMVar@s
    TMVar
  , newTMVar
  , newTMVarN
  , newEmptyTMVar
  , newEmptyTMVarN
  , takeTMVar
  , putTMVar
  , readTMVar
  , tryTakeTMVar
  , tryPutTMVar
  , tryReadTMVar
  , isEmptyTMVar
  , swapTMVar
  ) where

import           Control.Monad           (liftM, unless, when)
import           Control.Monad.STM.Class
import           Data.Maybe              (isJust, isNothing)

-- | A @TMVar@ is like an @MVar@ or a @mVar@, but using transactional
-- memory. As transactions are atomic, this makes dealing with
-- multiple @TMVar@s easier than wrangling multiple @mVar@s.
--
-- @since 1.0.0.0
newtype TMVar stm a = TMVar (TVar stm (Maybe a))

-- | Create a 'TMVar' containing the given value.
--
-- @since 1.0.0.0
newTMVar :: MonadSTM stm => a -> stm (TMVar stm a)
newTMVar = newTMVarN ""

-- | Create a 'TMVar' containing the given value, with the given
-- name.
--
-- Name conflicts are handled as usual for 'TVar's. The name is
-- prefixed with \"ctmvar-\".
--
-- @since 1.0.0.0
newTMVarN :: MonadSTM stm => String -> a -> stm (TMVar stm a)
newTMVarN n a = do
  let n' = if null n then "ctmvar" else "ctmvar-" ++ n
  ctvar <- newTVarN n' $ Just a
  pure (TMVar ctvar)

-- | Create a new empty 'TMVar'.
--
-- @since 1.0.0.0
newEmptyTMVar :: MonadSTM stm => stm (TMVar stm a)
newEmptyTMVar = newEmptyTMVarN ""

-- | Create a new empty 'TMVar' with the given name.
--
-- Name conflicts are handled as usual for 'TVar's. The name is
-- prefixed with \"ctmvar-\".
--
-- @since 1.0.0.0
newEmptyTMVarN :: MonadSTM stm => String -> stm (TMVar stm a)
newEmptyTMVarN n = do
  let n' = if null n then "ctmvar" else "ctmvar-" ++ n
  ctvar <- newTVarN n' Nothing
  pure (TMVar ctvar)

-- | Take the contents of a 'TMVar', or 'retry' if it is empty.
--
-- @since 1.0.0.0
takeTMVar :: MonadSTM stm => TMVar stm a -> stm a
takeTMVar ctmvar = do
  taken <- tryTakeTMVar ctmvar
  maybe retry pure taken

-- | Write to a 'TMVar', or 'retry' if it is full.
--
-- @since 1.0.0.0
putTMVar :: MonadSTM stm => TMVar stm a -> a -> stm ()
putTMVar ctmvar a = do
  putted <- tryPutTMVar ctmvar a
  unless putted retry

-- | Read from a 'TMVar' without emptying, or 'retry' if it is empty.
--
-- @since 1.0.0.0
readTMVar :: MonadSTM stm => TMVar stm a -> stm a
readTMVar ctmvar = do
  readed <- tryReadTMVar ctmvar
  maybe retry pure readed

-- | Try to take the contents of a 'TMVar', returning 'Nothing' if it
-- is empty.
--
-- @since 1.0.0.0
tryTakeTMVar :: MonadSTM stm => TMVar stm a -> stm (Maybe a)
tryTakeTMVar (TMVar ctvar) = do
  val <- readTVar ctvar
  when (isJust val) $ writeTVar ctvar Nothing
  pure val

-- | Try to write to a 'TMVar', returning 'False' if it is full.
--
-- @since 1.0.0.0
tryPutTMVar :: MonadSTM stm => TMVar stm a -> a -> stm Bool
tryPutTMVar (TMVar ctvar) a = do
  val <- readTVar ctvar
  when (isNothing val) $ writeTVar ctvar (Just a)
  pure (isNothing val)

-- | Try to read from a 'TMVar' without emptying, returning 'Nothing'
-- if it is empty.
--
-- @since 1.0.0.0
tryReadTMVar :: MonadSTM stm => TMVar stm a -> stm (Maybe a)
tryReadTMVar (TMVar ctvar) = readTVar ctvar

-- | Check if a 'TMVar' is empty or not.
--
-- @since 1.0.0.0
isEmptyTMVar :: MonadSTM stm => TMVar stm a -> stm Bool
isEmptyTMVar ctmvar = isNothing `liftM` tryReadTMVar ctmvar

-- | Swap the contents of a 'TMVar' returning the old contents, or
-- 'retry' if it is empty.
--
-- @since 1.0.0.0
swapTMVar :: MonadSTM stm => TMVar stm a -> a -> stm a
swapTMVar ctmvar a = do
  val <- takeTMVar ctmvar
  putTMVar ctmvar a
  pure val

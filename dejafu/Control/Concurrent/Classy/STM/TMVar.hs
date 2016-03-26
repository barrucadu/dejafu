-- | Transactional @MVar@s, for use with 'MonadSTM'.
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

import Control.Monad (liftM, when, unless)
import Control.Monad.STM.Class
import Data.Maybe (isJust, isNothing)

-- | A @TMVar@ is like an @MVar@ or a @mVar@, but using transactional
-- memory. As transactions are atomic, this makes dealing with
-- multiple @TMVar@s easier than wrangling multiple @mVar@s.
newtype TMVar stm a = TMVar (TVar stm (Maybe a))

-- | Create a 'TMVar' containing the given value.
newTMVar :: MonadSTM stm => a -> stm (TMVar stm a)
newTMVar = newTMVarN ""

-- | Create a 'TMVar' containing the given value, with the given
-- name.
--
-- Name conflicts are handled as usual for 'TVar's. The name is
-- prefixed with \"ctmvar-\".
newTMVarN :: MonadSTM stm => String -> a -> stm (TMVar stm a)
newTMVarN n a = do
  let n' = if null n then "ctmvar" else "ctmvar-" ++ n
  ctvar <- newTVarN n' $ Just a
  return $ TMVar ctvar

-- | Create a new empty 'TMVar'.
newEmptyTMVar :: MonadSTM stm => stm (TMVar stm a)
newEmptyTMVar = newEmptyTMVarN ""

-- | Create a new empty 'TMVar' with the given name.
--
-- Name conflicts are handled as usual for 'TVar's. The name is
-- prefixed with \"ctmvar-\".
newEmptyTMVarN :: MonadSTM stm => String -> stm (TMVar stm a)
newEmptyTMVarN n = do
  let n' = if null n then "ctmvar" else "ctmvar-" ++ n
  ctvar <- newTVarN n' Nothing
  return $ TMVar ctvar

-- | Take the contents of a 'TMVar', or 'retry' if it is empty.
takeTMVar :: MonadSTM stm => TMVar stm a -> stm a
takeTMVar ctmvar = do
  taken <- tryTakeTMVar ctmvar
  maybe retry return taken

-- | Write to a 'TMVar', or 'retry' if it is full.
putTMVar :: MonadSTM stm => TMVar stm a -> a -> stm ()
putTMVar ctmvar a = do
  putted <- tryPutTMVar ctmvar a
  unless putted retry

-- | Read from a 'TMVar' without emptying, or 'retry' if it is empty.
readTMVar :: MonadSTM stm => TMVar stm a -> stm a
readTMVar ctmvar = do
  readed <- tryReadTMVar ctmvar
  maybe retry return readed

-- | Try to take the contents of a 'TMVar', returning 'Nothing' if it
-- is empty.
tryTakeTMVar :: MonadSTM stm => TMVar stm a -> stm (Maybe a)
tryTakeTMVar (TMVar ctvar) = do
  val <- readTVar ctvar
  when (isJust val) $ writeTVar ctvar Nothing
  return val

-- | Try to write to a 'TMVar', returning 'False' if it is full.
tryPutTMVar :: MonadSTM stm => TMVar stm a -> a -> stm Bool
tryPutTMVar (TMVar ctvar) a = do
  val <- readTVar ctvar
  when (isNothing val) $ writeTVar ctvar (Just a)
  return $ isNothing val

-- | Try to read from a 'TMVar' without emptying, returning 'Nothing'
-- if it is empty.
tryReadTMVar :: MonadSTM stm => TMVar stm a -> stm (Maybe a)
tryReadTMVar (TMVar ctvar) = readTVar ctvar

-- | Check if a 'TMVar' is empty or not.
isEmptyTMVar :: MonadSTM stm => TMVar stm a -> stm Bool
isEmptyTMVar ctmvar = isNothing `liftM` tryReadTMVar ctmvar

-- | Swap the contents of a 'TMVar' returning the old contents, or
-- 'retry' if it is empty.
swapTMVar :: MonadSTM stm => TMVar stm a -> a -> stm a
swapTMVar ctmvar a = do
  val <- takeTMVar ctmvar
  putTMVar ctmvar a
  return val

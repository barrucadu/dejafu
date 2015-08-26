-- | Operations over @CVar@s
module Test.DejaFu.Deterministic.Internal.CVar where

import Control.Monad (when)
import Test.DejaFu.Internal
import Test.DejaFu.Deterministic.Internal.Common
import Test.DejaFu.Deterministic.Internal.Threading

--------------------------------------------------------------------------------
-- * Manipulating @CVar@s

-- | Put a value into a @CVar@, in either a blocking or nonblocking
-- way.
putIntoCVar :: Monad n
            => Bool -> V r a -> a -> (Bool -> Action n r s)
            -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
putIntoCVar blocking (cvid, ref) a c fixed threadid threads = do
  val <- readRef fixed ref

  case val of
    Just _
      | blocking ->
        let threads' = block (OnCVarEmpty cvid) threadid threads
        in return (False, threads', [])

      | otherwise ->
        return (False, goto (c False) threadid threads, [])

    Nothing -> do
      writeRef fixed ref $ Just a
      let (threads', woken) = wake (OnCVarFull cvid) threads
      return (True, goto (c True) threadid threads', woken)

-- | Take a value from a @CVar@, in either a blocking or nonblocking
-- way.
readFromCVar :: Monad n
             => Bool -> Bool -> V r a -> (Maybe a -> Action n r s)
             -> Fixed n r s -> ThreadId -> Threads n r s -> n (Bool, Threads n r s, [ThreadId])
readFromCVar emptying blocking (cvid, ref) c fixed threadid threads = do
  val <- readRef fixed ref

  case val of
    Just _ -> do
      when emptying $ writeRef fixed ref Nothing
      let (threads', woken) = wake (OnCVarEmpty cvid) threads
      return (True, goto (c val) threadid threads', woken)

    Nothing
      | blocking ->
        let threads' = block (OnCVarFull cvid) threadid threads
        in return (False, threads', [])

      | otherwise ->
        return (False, goto (c Nothing) threadid threads, [])

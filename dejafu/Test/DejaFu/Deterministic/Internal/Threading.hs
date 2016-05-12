{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

-- |
-- Module      : Test.DejaFu.Deterministic.Internal.Threading
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : ExistentialQuantification, RankNTypes
--
-- Operations and types for threads. This module is NOT considered to
-- form part of the public interface of this library.
module Test.DejaFu.Deterministic.Internal.Threading where

import Control.Exception (Exception, MaskingState(..), SomeException, fromException)
import Data.List (intersect, nub)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust, isNothing)

import Test.DejaFu.Common
import Test.DejaFu.Deterministic.Internal.Common

import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
-- * Threads

-- | Threads are stored in a map index by 'ThreadId'.
type Threads n r s = Map ThreadId (Thread n r s)

-- | All the state of a thread.
data Thread n r s = Thread
  { _continuation :: Action n r s
  -- ^ The next action to execute.
  , _blocking     :: Maybe BlockedOn
  -- ^ The state of any blocks.
  , _handlers     :: [Handler n r s]
  -- ^ Stack of exception handlers
  , _masking      :: MaskingState
  -- ^ The exception masking state.
  , _known        :: [Either MVarId TVarId]
  -- ^ Shared variables the thread knows about.
  , _fullknown    :: Bool
  -- ^ Whether the referenced variables of the thread are completely
  -- known. If every thread has _fullknown == True, then turn on
  -- detection of nonglobal deadlock.
  }

-- | Construct a thread with just one action
mkthread :: Action n r s -> Thread n r s
mkthread c = Thread c Nothing [] Unmasked [] False

--------------------------------------------------------------------------------
-- * Blocking

-- | A @BlockedOn@ is used to determine what sort of variable a thread
-- is blocked on.
data BlockedOn = OnMVarFull MVarId | OnMVarEmpty MVarId | OnTVar [TVarId] | OnMask ThreadId deriving Eq

-- | Determine if a thread is blocked in a certain way.
(~=) :: Thread n r s -> BlockedOn -> Bool
thread ~= theblock = case (_blocking thread, theblock) of
  (Just (OnMVarFull  _), OnMVarFull  _) -> True
  (Just (OnMVarEmpty _), OnMVarEmpty _) -> True
  (Just (OnTVar      _), OnTVar      _) -> True
  (Just (OnMask      _), OnMask      _) -> True
  _ -> False

-- | Determine if a thread is deadlocked. If at least one thread is
-- not in a fully-known state, this will only check for global
-- deadlock.
isLocked :: ThreadId -> Threads n r a -> Bool
isLocked tid ts
  | allKnown = case M.lookup tid ts of
    Just thread -> noRefs $ _blocking thread
    Nothing -> False
  | otherwise = M.null $ M.filter (isNothing . _blocking) ts

  where
    -- | Check if all threads are in a fully-known state.
    allKnown = all _fullknown $ M.elems ts

    -- | Check if no other runnable thread has a reference to anything
    -- the block references.
    noRefs (Just (OnMVarFull  cvarid)) = null $ findMVar  cvarid
    noRefs (Just (OnMVarEmpty cvarid)) = null $ findMVar  cvarid
    noRefs (Just (OnTVar      tvids))  = null $ findTVars tvids
    noRefs _ = True

    -- | Get IDs of all threads (other than the one under
    -- consideration) which reference a 'MVar'.
    findMVar cvarid = M.keys $ M.filterWithKey (check [Left cvarid]) ts

    -- | Get IDs of all runnable threads (other than the one under
    -- consideration) which reference some 'TVar's.
    findTVars tvids = M.keys $ M.filterWithKey (check (map Right tvids)) ts

    -- | Check if a thread references a variable, and if it's not the
    -- thread under consideration.
    check lookingfor thetid thethread
      | thetid == tid = False
      | otherwise     = (not . null $ lookingfor `intersect` _known thethread) && isNothing (_blocking thethread)

--------------------------------------------------------------------------------
-- * Exceptions

-- | An exception handler.
data Handler n r s = forall e. Exception e => Handler (e -> Action n r s)

-- | Propagate an exception upwards, finding the closest handler
-- which can deal with it.
propagate :: SomeException -> ThreadId -> Threads n r s -> Maybe (Threads n r s)
propagate e tid threads = case M.lookup tid threads >>= go . _handlers of
  Just (act, hs) -> Just $ except act hs tid threads
  Nothing -> Nothing

  where
    go [] = Nothing
    go (Handler h:hs) = maybe (go hs) (\act -> Just (act, hs)) $ h <$> fromException e

-- | Check if a thread can be interrupted by an exception.
interruptible :: Thread n r s -> Bool
interruptible thread = _masking thread == Unmasked || (_masking thread == MaskedInterruptible && isJust (_blocking thread))

-- | Register a new exception handler.
catching :: Exception e => (e -> Action n r s) -> ThreadId -> Threads n r s -> Threads n r s
catching h = M.alter $ \(Just thread) -> Just $ thread { _handlers = Handler h : _handlers thread }

-- | Remove the most recent exception handler.
uncatching :: ThreadId -> Threads n r s -> Threads n r s
uncatching = M.alter $ \(Just thread) -> Just $ thread { _handlers = tail $ _handlers thread }

-- | Raise an exception in a thread.
except :: Action n r s -> [Handler n r s] -> ThreadId -> Threads n r s -> Threads n r s
except act hs = M.alter $ \(Just thread) -> Just $ thread { _continuation = act, _handlers = hs, _blocking = Nothing }

-- | Set the masking state of a thread.
mask :: MaskingState -> ThreadId -> Threads n r s -> Threads n r s
mask ms = M.alter $ \(Just thread) -> Just $ thread { _masking = ms }

--------------------------------------------------------------------------------
-- * Manipulating threads

-- | Replace the @Action@ of a thread.
goto :: Action n r s -> ThreadId -> Threads n r s -> Threads n r s
goto a = M.alter $ \(Just thread) -> Just (thread { _continuation = a })

-- | Start a thread with the given ID, inheriting the masking state
-- from the parent thread. This ID must not already be in use!
launch :: ThreadId -> ThreadId -> ((forall b. M n r s b -> M n r s b) -> Action n r s) -> Threads n r s -> Threads n r s
launch parent tid a threads = launch' ms tid a threads where
  ms = fromMaybe Unmasked $ _masking <$> M.lookup parent threads

-- | Start a thread with the given ID and masking state. This must not already be in use!
launch' :: MaskingState -> ThreadId -> ((forall b. M n r s b -> M n r s b) -> Action n r s) -> Threads n r s -> Threads n r s
launch' ms tid a = M.insert tid thread where
  thread = Thread { _continuation = a umask, _blocking = Nothing, _handlers = [], _masking = ms, _known = [], _fullknown = False }

  umask mb = resetMask True Unmasked >> mb >>= \b -> resetMask False ms >> return b
  resetMask typ m = cont $ \k -> AResetMask typ True m $ k ()

-- | Kill a thread.
kill :: ThreadId -> Threads n r s -> Threads n r s
kill = M.delete

-- | Block a thread.
block :: BlockedOn -> ThreadId -> Threads n r s -> Threads n r s
block blockedOn = M.alter doBlock where
  doBlock (Just thread) = Just $ thread { _blocking = Just blockedOn }
  doBlock _ = error "Invariant failure in 'block': thread does NOT exist!"

-- | Unblock all threads waiting on the appropriate block. For 'TVar'
-- blocks, this will wake all threads waiting on at least one of the
-- given 'TVar's.
wake :: BlockedOn -> Threads n r s -> (Threads n r s, [ThreadId])
wake blockedOn threads = (unblock <$> threads, M.keys $ M.filter isBlocked threads) where
  unblock thread
    | isBlocked thread = thread { _blocking = Nothing }
    | otherwise = thread

  isBlocked thread = case (_blocking thread, blockedOn) of
    (Just (OnTVar tvids), OnTVar blockedOn') -> tvids `intersect` blockedOn' /= []
    (theblock, _) -> theblock == Just blockedOn

-- | Record that a thread knows about a shared variable.
knows :: [Either MVarId TVarId] -> ThreadId -> Threads n r s -> Threads n r s
knows theids = M.alter go where
  go (Just thread) = Just $ thread { _known = nub $ theids ++ _known thread }
  go _ = error "Invariant failure in 'knows': thread does NOT exist!"

-- | Forget about a shared variable.
forgets :: [Either MVarId TVarId] -> ThreadId -> Threads n r s -> Threads n r s
forgets theids = M.alter go where
  go (Just thread) = Just $ thread { _known = filter (`notElem` theids) $ _known thread }
  go _ = error "Invariant failure in 'forgets': thread does NOT exist!"

-- | Record that a thread's shared variable state is fully known.
fullknown :: ThreadId -> Threads n r s -> Threads n r s
fullknown = M.alter go where
  go (Just thread) = Just $ thread { _fullknown = True }
  go _ = error "Invariant failure in 'fullknown': thread does NOT exist!"

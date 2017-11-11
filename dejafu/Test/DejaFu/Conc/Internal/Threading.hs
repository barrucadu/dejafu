{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Test.DejaFu.Conc.Internal.Threading
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : ExistentialQuantification, RankNTypes
--
-- Operations and types for threads. This module is NOT considered to
-- form part of the public interface of this library.
module Test.DejaFu.Conc.Internal.Threading where

import qualified Control.Concurrent.Classy        as C
import           Control.Exception                (Exception, MaskingState(..),
                                                   SomeException, fromException)
import           Data.List                        (intersect)
import           Data.Map.Strict                  (Map)
import           Data.Maybe                       (isJust)

import           Test.DejaFu.Common
import           Test.DejaFu.Conc.Internal.Common

import qualified Data.Map.Strict                  as M

--------------------------------------------------------------------------------
-- * Threads

-- | Threads are stored in a map index by 'ThreadId'.
type Threads n r = Map ThreadId (Thread n r)

-- | All the state of a thread.
data Thread n r = Thread
  { _continuation :: Action n r
  -- ^ The next action to execute.
  , _blocking     :: Maybe BlockedOn
  -- ^ The state of any blocks.
  , _handlers     :: [Handler n r]
  -- ^ Stack of exception handlers
  , _masking      :: MaskingState
  -- ^ The exception masking state.
  , _bound        :: Maybe (BoundThread n r)
  -- ^ State for the associated bound thread, if it exists.
  }

-- | The state of a bound thread.
data BoundThread n r = BoundThread
  { _runboundIO :: C.MVar n (n (Action n r))
  -- ^ Run an @IO@ action in the bound thread by writing to this.
  , _getboundIO :: C.MVar n (Action n r)
  -- ^ Get the result of the above by reading from this.
  , _boundTId   :: C.ThreadId n
  -- ^ Thread ID
  }

-- | Construct a thread with just one action
mkthread :: Action n r -> Thread n r
mkthread c = Thread c Nothing [] Unmasked Nothing

--------------------------------------------------------------------------------
-- * Blocking

-- | A @BlockedOn@ is used to determine what sort of variable a thread
-- is blocked on.
data BlockedOn = OnMVarFull MVarId | OnMVarEmpty MVarId | OnTVar [TVarId] | OnMask ThreadId deriving Eq

-- | Determine if a thread is blocked in a certain way.
(~=) :: Thread n r -> BlockedOn -> Bool
thread ~= theblock = case (_blocking thread, theblock) of
  (Just (OnMVarFull  _), OnMVarFull  _) -> True
  (Just (OnMVarEmpty _), OnMVarEmpty _) -> True
  (Just (OnTVar      _), OnTVar      _) -> True
  (Just (OnMask      _), OnMask      _) -> True
  _ -> False

--------------------------------------------------------------------------------
-- * Exceptions

-- | An exception handler.
data Handler n r = forall e. Exception e => Handler (e -> MaskingState -> Action n r)

-- | Propagate an exception upwards, finding the closest handler
-- which can deal with it.
propagate :: SomeException -> ThreadId -> Threads n r -> Maybe (Threads n r)
propagate e tid threads = case M.lookup tid threads >>= go . _handlers of
  Just (act, hs) -> Just $ except act hs tid threads
  Nothing -> Nothing

  where
    go [] = Nothing
    go (Handler h:hs) = maybe (go hs) (\act -> Just (act, hs)) $ h <$> fromException e

-- | Check if a thread can be interrupted by an exception.
interruptible :: Thread n r -> Bool
interruptible thread = _masking thread == Unmasked || (_masking thread == MaskedInterruptible && isJust (_blocking thread))

-- | Register a new exception handler.
catching :: Exception e => (e -> Action n r) -> ThreadId -> Threads n r -> Threads n r
catching h = M.adjust $ \thread ->
  let ms0 = _masking thread
      h'  = Handler $ \e ms -> (if ms /= ms0 then AResetMask False False ms0 else id) (h e)
  in thread { _handlers = h' : _handlers thread }

-- | Remove the most recent exception handler.
uncatching :: ThreadId -> Threads n r -> Threads n r
uncatching = M.adjust $ \thread -> thread { _handlers = etail "uncatching" (_handlers thread) }

-- | Raise an exception in a thread.
except :: (MaskingState -> Action n r) -> [Handler n r] -> ThreadId -> Threads n r -> Threads n r
except actf hs = M.adjust $ \thread -> thread
  { _continuation = actf (_masking thread)
  , _handlers = hs
  , _blocking = Nothing
  }

-- | Set the masking state of a thread.
mask :: MaskingState -> ThreadId -> Threads n r -> Threads n r
mask ms = M.adjust $ \thread -> thread { _masking = ms }

--------------------------------------------------------------------------------
-- * Manipulating threads

-- | Replace the @Action@ of a thread.
goto :: Action n r -> ThreadId -> Threads n r -> Threads n r
goto a = M.adjust $ \thread -> thread { _continuation = a }

-- | Start a thread with the given ID, inheriting the masking state
-- from the parent thread. This ID must not already be in use!
launch :: ThreadId -> ThreadId -> ((forall b. M n r b -> M n r b) -> Action n r) -> Threads n r -> Threads n r
launch parent tid a threads = launch' ms tid a threads where
  ms = maybe Unmasked _masking (M.lookup parent threads)

-- | Start a thread with the given ID and masking state. This must not already be in use!
launch' :: MaskingState -> ThreadId -> ((forall b. M n r b -> M n r b) -> Action n r) -> Threads n r -> Threads n r
launch' ms tid a = M.insert tid thread where
  thread = Thread (a umask) Nothing [] ms Nothing

  umask mb = resetMask True Unmasked >> mb >>= \b -> resetMask False ms >> pure b
  resetMask typ m = cont $ \k -> AResetMask typ True m $ k ()

-- | Block a thread.
block :: BlockedOn -> ThreadId -> Threads n r -> Threads n r
block blockedOn = M.adjust $ \thread -> thread { _blocking = Just blockedOn }

-- | Unblock all threads waiting on the appropriate block. For 'TVar'
-- blocks, this will wake all threads waiting on at least one of the
-- given 'TVar's.
wake :: BlockedOn -> Threads n r -> (Threads n r, [ThreadId])
wake blockedOn threads = (unblock <$> threads, M.keys $ M.filter isBlocked threads) where
  unblock thread
    | isBlocked thread = thread { _blocking = Nothing }
    | otherwise = thread

  isBlocked thread = case (_blocking thread, blockedOn) of
    (Just (OnTVar tvids), OnTVar blockedOn') -> tvids `intersect` blockedOn' /= []
    (theblock, _) -> theblock == Just blockedOn

-------------------------------------------------------------------------------
-- ** Bound threads

-- | Turn a thread into a bound thread.
makeBound :: C.MonadConc n => ThreadId -> Threads n r -> n (Threads n r)
makeBound tid threads = do
    runboundIO <- C.newEmptyMVar
    getboundIO <- C.newEmptyMVar
    btid <- C.forkOSN ("bound worker for '" ++ show tid ++ "'") (go runboundIO getboundIO)
    let bt = BoundThread runboundIO getboundIO btid
    pure (M.adjust (\t -> t { _bound = Just bt }) tid threads)
  where
    go runboundIO getboundIO =
      let loop = do
            na <- C.takeMVar runboundIO
            C.putMVar getboundIO =<< na
            loop
      in loop

-- | Kill a thread and remove it from the thread map.
--
-- If the thread is bound, the worker thread is cleaned up.
kill :: C.MonadConc n => ThreadId -> Threads n r -> n (Threads n r)
kill tid threads = case M.lookup tid threads of
  Just thread -> case _bound thread of
    Just bt -> do
      C.killThread (_boundTId bt)
      pure (M.delete tid threads)
    Nothing -> pure (M.delete tid threads)
  Nothing -> pure threads

-- | Run an action.
--
-- If the thread is bound, the action is run in the worker thread.
runLiftedAct :: C.MonadConc n => ThreadId -> Threads n r -> n (Action n r) -> n (Action n r)
runLiftedAct tid threads ma = case _bound =<< M.lookup tid threads of
  Just bt -> do
    C.putMVar (_runboundIO bt) ma
    C.takeMVar (_getboundIO bt)
  Nothing -> ma

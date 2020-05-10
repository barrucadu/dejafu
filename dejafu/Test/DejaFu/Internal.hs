{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Test.DejaFu.Internal
-- Copyright   : (c) 2017--2020 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : DeriveAnyClass, DeriveGeneric, FlexibleContexts, GADTs, LambdaCase
--
-- Internal types and functions used throughout DejaFu.  This module
-- is NOT considered to form part of the public interface of this
-- library.
module Test.DejaFu.Internal where

import           Control.DeepSeq    (NFData(..))
import           Control.Exception  (MaskingState(..))
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as S
import           GHC.Generics       (Generic)
import           GHC.Stack          (HasCallStack, withFrozenCallStack)
import           System.Random      (RandomGen)

import           Test.DejaFu.Types

-------------------------------------------------------------------------------
-- * SCT settings

-- | SCT configuration record.
--
-- @since 1.2.0.0
data Settings n a = Settings
  { _way :: Way
  , _lengthBound :: Maybe LengthBound
  , _memtype :: MemType
  , _discard :: Maybe (Either Condition a -> Maybe Discard)
  , _debugShow :: Maybe (a -> String)
  , _debugPrint :: Maybe (String -> n ())
  , _debugFatal :: Bool
  , _earlyExit :: Maybe (Either Condition a -> Bool)
  , _equality :: Maybe (a -> a -> Bool)
  , _simplify  :: Bool
  , _safeIO :: Bool
  , _showAborts :: Bool
  }

-- | How to explore the possible executions of a concurrent program.
--
-- @since 0.7.0.0
data Way where
  Systematic :: Bounds -> Way
  Randomly   :: RandomGen g => (g -> (Int, g)) -> g -> Int -> Way

instance Show Way where
  show (Systematic bs) = "Systematic (" ++ show bs ++ ")"
  show (Randomly _ _ n) = "Randomly <f> <gen> " ++ show n

-------------------------------------------------------------------------------
-- * Identifiers

-- | The number of ID parameters was getting a bit unwieldy, so this
-- hides them all away.
data IdSource = IdSource
  { _iorids :: (Int, [String])
  , _mvids  :: (Int, [String])
  , _tvids  :: (Int, [String])
  , _tids   :: (Int, [String])
  } deriving (Eq, Ord, Show, Generic, NFData)

-- | Get the next free 'IORefId'.
nextIORId :: String -> IdSource -> (IdSource, IORefId)
nextIORId name idsource =
  let (iorid, iorids') = nextId name (_iorids idsource)
  in (idsource { _iorids = iorids' }, IORefId iorid)

-- | Get the next free 'MVarId'.
nextMVId :: String -> IdSource -> (IdSource, MVarId)
nextMVId name idsource =
  let (mvid, mvids') = nextId name (_mvids idsource)
  in (idsource { _mvids = mvids' }, MVarId mvid)

-- | Get the next free 'TVarId'.
nextTVId :: String -> IdSource -> (IdSource, TVarId)
nextTVId name idsource =
  let (tvid, tvids') = nextId name (_tvids idsource)
  in (idsource { _tvids = tvids' }, TVarId tvid)

-- | Get the next free 'ThreadId'.
nextTId :: String -> IdSource -> (IdSource, ThreadId)
nextTId name idsource =
  let (tid, tids') = nextId name (_tids idsource)
  in (idsource { _tids = tids' }, ThreadId tid)

-- | Helper for @next*@
nextId :: String -> (Int, [String]) -> (Id, (Int, [String]))
nextId name (num, used) = (Id newName (num+1), (num+1, newUsed)) where
  newName
    | null name = Nothing
    | occurrences > 0 = Just (name ++ "-" ++ show occurrences)
    | otherwise = Just name
  newUsed
    | null name = used
    | otherwise = name : used
  occurrences = length (filter (==name) used)

-- | The initial ID source.
initialIdSource :: IdSource
initialIdSource = IdSource (0, []) (0, []) (0, []) (0, [])

-------------------------------------------------------------------------------
-- * Actions

-- | Check if a @ThreadAction@ immediately blocks.
isBlock :: ThreadAction -> Bool
isBlock (BlockedThrowTo  _) = True
isBlock (BlockedTakeMVar _) = True
isBlock (BlockedReadMVar _) = True
isBlock (BlockedPutMVar  _) = True
isBlock (BlockedSTM _) = True
isBlock _ = False

-- | Get the @TVar@s affected by a @ThreadAction@.
tvarsOf :: ThreadAction -> Set TVarId
tvarsOf act = tvarsRead act `S.union` tvarsWritten act

-- | Get the @TVar@s a transaction wrote to (or would have, if it
-- didn't @retry@).
tvarsWritten :: ThreadAction -> Set TVarId
tvarsWritten act = S.fromList $ case act of
  STM trc _ -> concatMap tvarsOf' trc
  BlockedSTM trc -> concatMap tvarsOf' trc
  _ -> []

  where
    tvarsOf' (TNew tv) = [tv]
    tvarsOf' (TWrite tv) = [tv]
    tvarsOf' (TOrElse ta tb) = concatMap tvarsOf' (ta ++ fromMaybe [] tb)
    tvarsOf' (TCatch  ta tb) = concatMap tvarsOf' (ta ++ fromMaybe [] tb)
    tvarsOf' _ = []

-- | Get the @TVar@s a transaction read from.
tvarsRead :: ThreadAction -> Set TVarId
tvarsRead act = S.fromList $ case act of
  STM trc _ -> concatMap tvarsOf' trc
  BlockedSTM trc -> concatMap tvarsOf' trc
  _ -> []

  where
    tvarsOf' (TRead tv) = [tv]
    tvarsOf' (TOrElse ta tb) = concatMap tvarsOf' (ta ++ fromMaybe [] tb)
    tvarsOf' (TCatch  ta tb) = concatMap tvarsOf' (ta ++ fromMaybe [] tb)
    tvarsOf' _ = []

-- | Convert a 'ThreadAction' into a 'Lookahead': \"rewind\" what has
-- happened.
rewind :: ThreadAction -> Lookahead
rewind (Fork _) = WillFork
rewind (ForkOS _) = WillForkOS
rewind (SupportsBoundThreads _) = WillSupportsBoundThreads
rewind (IsCurrentThreadBound _) = WillIsCurrentThreadBound
rewind MyThreadId = WillMyThreadId
rewind (GetNumCapabilities _) = WillGetNumCapabilities
rewind (SetNumCapabilities i) = WillSetNumCapabilities i
rewind Yield = WillYield
rewind (ThreadDelay n) = WillThreadDelay n
rewind (NewMVar _) = WillNewMVar
rewind (PutMVar c _) = WillPutMVar c
rewind (BlockedPutMVar c) = WillPutMVar c
rewind (TryPutMVar c _ _) = WillTryPutMVar c
rewind (ReadMVar c) = WillReadMVar c
rewind (BlockedReadMVar c) = WillReadMVar c
rewind (TryReadMVar c _) = WillTryReadMVar c
rewind (TakeMVar c _) = WillTakeMVar c
rewind (BlockedTakeMVar c) = WillTakeMVar c
rewind (TryTakeMVar c _ _) = WillTryTakeMVar c
rewind (NewIORef _) = WillNewIORef
rewind (ReadIORef c) = WillReadIORef c
rewind (ReadIORefCas c) = WillReadIORefCas c
rewind (ModIORef c) = WillModIORef c
rewind (ModIORefCas c) = WillModIORefCas c
rewind (WriteIORef c) = WillWriteIORef c
rewind (CasIORef c _) = WillCasIORef c
rewind (CommitIORef t c) = WillCommitIORef t c
rewind (STM _ _) = WillSTM
rewind (BlockedSTM _) = WillSTM
rewind Catching = WillCatching
rewind PopCatching = WillPopCatching
rewind (Throw _) = WillThrow
rewind (ThrowTo t _) = WillThrowTo t
rewind (BlockedThrowTo t) = WillThrowTo t
rewind (SetMasking b m) = WillSetMasking b m
rewind (ResetMasking b m) = WillResetMasking b m
rewind (GetMaskingState _) = WillGetMaskingState
rewind LiftIO = WillLiftIO
rewind Return = WillReturn
rewind Stop = WillStop
rewind RegisterInvariant = WillRegisterInvariant

-- | Check if an operation could enable another thread.
willRelease :: Lookahead -> Bool
willRelease WillFork = True
willRelease WillForkOS = True
willRelease WillYield = True
willRelease (WillThreadDelay _) = True
willRelease (WillPutMVar _) = True
willRelease (WillTryPutMVar _) = True
willRelease (WillReadMVar _) = True
willRelease (WillTakeMVar _) = True
willRelease (WillTryTakeMVar _) = True
willRelease WillSTM = True
willRelease WillThrow = True
willRelease (WillSetMasking _ _) = True
willRelease (WillResetMasking _ _) = True
willRelease WillStop = True
willRelease _ = False

-------------------------------------------------------------------------------
-- * Simplified actions

-- | A simplified view of the possible actions a thread can perform.
data ActionType =
    UnsynchronisedRead  IORefId
  -- ^ A 'readIORef' or a 'readForCAS'.
  | UnsynchronisedWrite IORefId
  -- ^ A 'writeIORef'.
  | UnsynchronisedOther
  -- ^ Some other action which doesn't require cross-thread
  -- communication.
  | PartiallySynchronisedCommit IORefId
  -- ^ A commit.
  | PartiallySynchronisedWrite  IORefId
  -- ^ A 'casIORef'
  | PartiallySynchronisedModify IORefId
  -- ^ A 'modifyIORefCAS'
  | SynchronisedModify  IORefId
  -- ^ An 'atomicModifyIORef'.
  | SynchronisedRead    MVarId
  -- ^ A 'readMVar' or 'takeMVar' (or @try@/@blocked@ variants).
  | SynchronisedWrite   MVarId
  -- ^ A 'putMVar' (or @try@/@blocked@ variant).
  | SynchronisedOther
  -- ^ Some other action which does require cross-thread
  -- communication.
  deriving (Eq, Show, Generic, NFData)

-- | Check if an action imposes a write barrier.
isBarrier :: ActionType -> Bool
isBarrier (SynchronisedModify _) = True
isBarrier (SynchronisedRead   _) = True
isBarrier (SynchronisedWrite  _) = True
isBarrier SynchronisedOther = True
isBarrier _ = False

-- | Check if an action commits a given 'IORef'.
isCommit :: ActionType -> IORefId -> Bool
isCommit (PartiallySynchronisedCommit c) r = c == r
isCommit (PartiallySynchronisedWrite  c) r = c == r
isCommit (PartiallySynchronisedModify c) r = c == r
isCommit _ _ = False

-- | Check if an action synchronises a given 'IORef'.
synchronises :: ActionType -> IORefId -> Bool
synchronises a r = isCommit a r || isBarrier a

-- | Get the 'IORef' affected.
iorefOf :: ActionType -> Maybe IORefId
iorefOf (UnsynchronisedRead  r) = Just r
iorefOf (UnsynchronisedWrite r) = Just r
iorefOf (SynchronisedModify  r) = Just r
iorefOf (PartiallySynchronisedCommit r) = Just r
iorefOf (PartiallySynchronisedWrite  r) = Just r
iorefOf (PartiallySynchronisedModify r) = Just r
iorefOf _ = Nothing

-- | Get the 'MVar' affected.
mvarOf :: ActionType -> Maybe MVarId
mvarOf (SynchronisedRead  c) = Just c
mvarOf (SynchronisedWrite c) = Just c
mvarOf _ = Nothing

-- | Get the @ThreadId@s involved in a @ThreadAction@.
tidsOf :: ThreadAction -> Set ThreadId
tidsOf (Fork tid) = S.singleton tid
tidsOf (ForkOS tid) = S.singleton tid
tidsOf (PutMVar _ tids) = S.fromList tids
tidsOf (TryPutMVar _ _ tids) = S.fromList tids
tidsOf (TakeMVar _ tids) = S.fromList tids
tidsOf (TryTakeMVar _ _ tids) = S.fromList tids
tidsOf (CommitIORef tid _) = S.singleton tid
tidsOf (STM _ tids) = S.fromList tids
tidsOf (ThrowTo tid _) = S.singleton tid
tidsOf (BlockedThrowTo tid) = S.singleton tid
tidsOf _ = S.empty

-- | Throw away information from a 'ThreadAction' and give a
-- simplified view of what is happening.
--
-- This is used in the SCT code to help determine interesting
-- alternative scheduling decisions.
simplifyAction :: ThreadAction -> ActionType
simplifyAction = simplifyLookahead . rewind

-- | Variant of 'simplifyAction' that takes a 'Lookahead'.
simplifyLookahead :: Lookahead -> ActionType
simplifyLookahead (WillPutMVar c)     = SynchronisedWrite c
simplifyLookahead (WillTryPutMVar c)  = SynchronisedWrite c
simplifyLookahead (WillReadMVar c)    = SynchronisedRead c
simplifyLookahead (WillTryReadMVar c) = SynchronisedRead c
simplifyLookahead (WillTakeMVar c)    = SynchronisedRead c
simplifyLookahead (WillTryTakeMVar c)  = SynchronisedRead c
simplifyLookahead (WillReadIORef r)     = UnsynchronisedRead r
simplifyLookahead (WillReadIORefCas r)  = UnsynchronisedRead r
simplifyLookahead (WillModIORef r)      = SynchronisedModify r
simplifyLookahead (WillModIORefCas r)   = PartiallySynchronisedModify r
simplifyLookahead (WillWriteIORef r)    = UnsynchronisedWrite r
simplifyLookahead (WillCasIORef r)      = PartiallySynchronisedWrite r
simplifyLookahead (WillCommitIORef _ r) = PartiallySynchronisedCommit r
simplifyLookahead WillSTM         = SynchronisedOther
simplifyLookahead (WillThrowTo _) = SynchronisedOther
simplifyLookahead _ = UnsynchronisedOther

-------------------------------------------------------------------------------
-- * Concurrency state

-- | Initial concurrency state.
initialCState :: ConcurrencyState
initialCState = ConcurrencyState M.empty S.empty M.empty

-- | Update the concurrency state with the action that has just
-- happened.
updateCState :: MemType -> ConcurrencyState -> ThreadId -> ThreadAction -> ConcurrencyState
updateCState memtype cstate tid act = ConcurrencyState
  { concIOState   = updateIOState memtype act $ concIOState   cstate
  , concMVState   = updateMVState         act $ concMVState   cstate
  , concMaskState = updateMaskState tid   act $ concMaskState cstate
  }

-- | Update the @IORef@ buffer state with the action that has just
-- happened.
updateIOState :: MemType -> ThreadAction -> Map IORefId Int -> Map IORefId Int
updateIOState SequentialConsistency _ = const M.empty
updateIOState _ (CommitIORef _ r) = (`M.alter` r) $ \case
  Just 1  -> Nothing
  Just n  -> Just (n-1)
  Nothing -> Nothing
updateIOState _ (WriteIORef    r) = M.insertWith (+) r 1
updateIOState _ ta
  | isBarrier $ simplifyAction ta = const M.empty
  | otherwise = id

-- | Update the @MVar@ full/empty state with the action that has just
-- happened.
updateMVState :: ThreadAction -> Set MVarId -> Set MVarId
updateMVState (PutMVar mvid _) = S.insert mvid
updateMVState (TryPutMVar mvid True _) = S.insert mvid
updateMVState (TakeMVar mvid _) = S.delete mvid
updateMVState (TryTakeMVar mvid True _) = S.delete mvid
updateMVState _ = id

-- | Update the thread masking state with the action that has just
-- happened.
updateMaskState :: ThreadId -> ThreadAction -> Map ThreadId MaskingState -> Map ThreadId MaskingState
updateMaskState tid (Fork tid2) = \masks -> case M.lookup tid masks of
  -- A thread inherits the masking state of its parent.
  Just ms -> M.insert tid2 ms masks
  Nothing -> masks
updateMaskState tid (SetMasking   _ ms) = M.insert tid ms
updateMaskState tid (ResetMasking _ ms) = M.insert tid ms
updateMaskState tid (Throw True) = M.delete tid
updateMaskState _ (ThrowTo tid True) = M.delete tid
updateMaskState tid Stop = M.delete tid
updateMaskState _ _ = id

-------------------------------------------------------------------------------
-- * Error reporting

-- | 'tail' but with a better error message if it fails.  Use this
-- only where it shouldn't fail!
etail :: HasCallStack => [a] -> [a]
etail (_:xs) = xs
etail _ = withFrozenCallStack $ fatal "tail: empty list"

-- | '(!!)' but with a better error message if it fails.  Use this
-- only where it shouldn't fail!
eidx :: HasCallStack => [a] -> Int -> a
eidx xs i
  | i < length xs = xs !! i
  | otherwise = withFrozenCallStack $ fatal "(!!): index too large"

-- | 'fromJust' but with a better error message if it fails.  Use this
-- only where it shouldn't fail!
efromJust :: HasCallStack => Maybe a -> a
efromJust (Just x) = x
efromJust _ = withFrozenCallStack $ fatal "fromJust: Nothing"

-- | 'fromList' but with a better error message if it fails.  Use this
-- only where it shouldn't fail!
efromList :: HasCallStack => [a] -> NonEmpty a
efromList (x:xs) = x:|xs
efromList _ = withFrozenCallStack $ fatal "fromList: empty list"

-- | 'fromRight' but with a better error message if it fails.  Use
-- this only where it shouldn't fail!
efromRight :: HasCallStack => Either a b -> b
efromRight (Right b) = b
efromRight _ = withFrozenCallStack $ fatal "fromRight: Left"

-- | 'fromLeft' but with a better error message if it fails.  Use
-- this only where it shouldn't fail!
efromLeft :: HasCallStack => Either a b -> a
efromLeft (Left a) = a
efromLeft _ = withFrozenCallStack $ fatal "fromLeft: Right"

-- | 'M.adjust' but which errors if the key is not present.  Use this
-- only where it shouldn't fail!
eadjust :: (Ord k, Show k, HasCallStack) => (v -> v) -> k -> M.Map k v -> M.Map k v
eadjust f k m = case M.lookup k m of
  Just v -> M.insert k (f v) m
  Nothing -> withFrozenCallStack $ fatal ("adjust: key '" ++ show k ++ "' not found")

-- | 'M.insert' but which errors if the key is already present.  Use
-- this only where it shouldn't fail!
einsert :: (Ord k, Show k, HasCallStack) => k -> v -> M.Map k v -> M.Map k v
einsert k v m
  | M.member k m = withFrozenCallStack $ fatal ("insert: key '" ++ show k ++ "' already present")
  | otherwise = M.insert k v m

-- | 'M.lookup' but which errors if the key is not present.  Use this
-- only where it shouldn't fail!
elookup :: (Ord k, Show k, HasCallStack) => k -> M.Map k v -> v
elookup k =
  fromMaybe (withFrozenCallStack $ fatal ("lookup: key '" ++ show k ++ "' not found")) .
  M.lookup k

-- | 'error' but saying where it came from
fatal :: HasCallStack => String -> a
fatal msg = withFrozenCallStack $ error ("(dejafu) " ++ msg)

-------------------------------------------------------------------------------
-- * Miscellaneous

-- | Run with a continuation that writes its value into a reference,
-- returning the computation and the reference.  Using the reference
-- is non-blocking, it is up to you to ensure you wait sufficiently.
runRefCont :: MonadDejaFu n
  => (n () -> x)
  -> (a -> Maybe b)
  -> ((a -> x) -> x)
  -> n (x, Ref n (Maybe b))
runRefCont act f k = do
  ref <- newRef Nothing
  let c = k (act . writeRef ref . f)
  pure (c, ref)

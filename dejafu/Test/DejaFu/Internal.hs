{-# LANGUAGE GADTs #-}

-- |
-- Module      : Test.DejaFu.Internal
-- Copyright   : (c) 2017--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : GADTs
--
-- Internal types and functions used throughout DejaFu.  This module
-- is NOT considered to form part of the public interface of this
-- library.
module Test.DejaFu.Internal where

import           Control.DeepSeq    (NFData(..))
import           Control.Monad.Ref  (MonadRef(..))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as S
import           System.Random      (RandomGen)

import           Test.DejaFu.Types

-------------------------------------------------------------------------------
-- * SCT settings

-- | SCT configuration record.
--
-- @since 1.2.0.0
data Settings n a = Settings
  { _way :: Way
  , _memtype :: MemType
  , _discard :: Maybe (Either Failure a -> Maybe Discard)
  , _debugShow :: Maybe (a -> String)
  , _debugPrint :: Maybe (String -> n ())
  , _earlyExit :: Maybe (Either Failure a -> Bool)
  }

-- | How to explore the possible executions of a concurrent program.
--
-- @since 0.7.0.0
data Way where
  Systematic :: Bounds -> Way
  Weighted   :: RandomGen g => g -> Int -> Int -> Way
  Uniform    :: RandomGen g => g -> Int -> Way

instance Show Way where
  show (Systematic bs)  = "Systematic (" ++ show bs ++ ")"
  show (Weighted _ n t) = "Weighted <gen> " ++ show (n, t)
  show (Uniform  _ n)   = "Uniform <gen> " ++ show n

-------------------------------------------------------------------------------
-- * Identifiers

-- | The number of ID parameters was getting a bit unwieldy, so this
-- hides them all away.
data IdSource = IdSource
  { _crids :: (Int, [String])
  , _mvids :: (Int, [String])
  , _tvids :: (Int, [String])
  , _tids  :: (Int, [String])
  } deriving (Eq, Ord, Show)

instance NFData IdSource where
  rnf idsource = rnf ( _crids idsource
                     , _mvids idsource
                     , _tvids idsource
                     , _tids  idsource
                     )

-- | Get the next free 'CRefId'.
nextCRId :: String -> IdSource -> (IdSource, CRefId)
nextCRId name idsource =
  let (crid, crids') = nextId name (_crids idsource)
  in (idsource { _crids = crids' }, CRefId crid)

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
-- happened. 'Killed' has no 'Lookahead' counterpart.
rewind :: ThreadAction -> Maybe Lookahead
rewind (Fork _) = Just WillFork
rewind (ForkOS _) = Just WillForkOS
rewind (IsCurrentThreadBound _) = Just WillIsCurrentThreadBound
rewind MyThreadId = Just WillMyThreadId
rewind (GetNumCapabilities _) = Just WillGetNumCapabilities
rewind (SetNumCapabilities i) = Just (WillSetNumCapabilities i)
rewind Yield = Just WillYield
rewind (ThreadDelay n) = Just (WillThreadDelay n)
rewind (NewMVar _) = Just WillNewMVar
rewind (PutMVar c _) = Just (WillPutMVar c)
rewind (BlockedPutMVar c) = Just (WillPutMVar c)
rewind (TryPutMVar c _ _) = Just (WillTryPutMVar c)
rewind (ReadMVar c) = Just (WillReadMVar c)
rewind (BlockedReadMVar c) = Just (WillReadMVar c)
rewind (TryReadMVar c _) = Just (WillTryReadMVar c)
rewind (TakeMVar c _) = Just (WillTakeMVar c)
rewind (BlockedTakeMVar c) = Just (WillTakeMVar c)
rewind (TryTakeMVar c _ _) = Just (WillTryTakeMVar c)
rewind (NewCRef _) = Just WillNewCRef
rewind (ReadCRef c) = Just (WillReadCRef c)
rewind (ReadCRefCas c) = Just (WillReadCRefCas c)
rewind (ModCRef c) = Just (WillModCRef c)
rewind (ModCRefCas c) = Just (WillModCRefCas c)
rewind (WriteCRef c) = Just (WillWriteCRef c)
rewind (CasCRef c _) = Just (WillCasCRef c)
rewind (CommitCRef t c) = Just (WillCommitCRef t c)
rewind (STM _ _) = Just WillSTM
rewind (BlockedSTM _) = Just WillSTM
rewind Catching = Just WillCatching
rewind PopCatching = Just WillPopCatching
rewind Throw = Just WillThrow
rewind (ThrowTo t) = Just (WillThrowTo t)
rewind (BlockedThrowTo t) = Just (WillThrowTo t)
rewind Killed = Nothing
rewind (SetMasking b m) = Just (WillSetMasking b m)
rewind (ResetMasking b m) = Just (WillResetMasking b m)
rewind LiftIO = Just WillLiftIO
rewind Return = Just WillReturn
rewind Stop = Just WillStop
rewind Subconcurrency = Just WillSubconcurrency
rewind StopSubconcurrency = Just WillStopSubconcurrency
rewind (DontCheck _) = Just WillDontCheck

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
willRelease WillDontCheck = True
willRelease _ = False

-------------------------------------------------------------------------------
-- * Simplified actions

-- | A simplified view of the possible actions a thread can perform.
data ActionType =
    UnsynchronisedRead  CRefId
  -- ^ A 'readCRef' or a 'readForCAS'.
  | UnsynchronisedWrite CRefId
  -- ^ A 'writeCRef'.
  | UnsynchronisedOther
  -- ^ Some other action which doesn't require cross-thread
  -- communication.
  | PartiallySynchronisedCommit CRefId
  -- ^ A commit.
  | PartiallySynchronisedWrite  CRefId
  -- ^ A 'casCRef'
  | PartiallySynchronisedModify CRefId
  -- ^ A 'modifyCRefCAS'
  | SynchronisedModify  CRefId
  -- ^ An 'atomicModifyCRef'.
  | SynchronisedRead    MVarId
  -- ^ A 'readMVar' or 'takeMVar' (or @try@/@blocked@ variants).
  | SynchronisedWrite   MVarId
  -- ^ A 'putMVar' (or @try@/@blocked@ variant).
  | SynchronisedOther
  -- ^ Some other action which does require cross-thread
  -- communication.
  deriving (Eq, Show)

instance NFData ActionType where
  rnf (UnsynchronisedRead c) = rnf c
  rnf (UnsynchronisedWrite c) = rnf c
  rnf (PartiallySynchronisedCommit c) = rnf c
  rnf (PartiallySynchronisedWrite c) = rnf c
  rnf (PartiallySynchronisedModify c) = rnf c
  rnf (SynchronisedModify c) = rnf c
  rnf (SynchronisedRead m) = rnf m
  rnf (SynchronisedWrite m) = rnf m
  rnf a = a `seq` ()

-- | Check if an action imposes a write barrier.
isBarrier :: ActionType -> Bool
isBarrier (SynchronisedModify _) = True
isBarrier (SynchronisedRead   _) = True
isBarrier (SynchronisedWrite  _) = True
isBarrier SynchronisedOther = True
isBarrier _ = False

-- | Check if an action commits a given 'CRef'.
isCommit :: ActionType -> CRefId -> Bool
isCommit (PartiallySynchronisedCommit c) r = c == r
isCommit (PartiallySynchronisedWrite  c) r = c == r
isCommit (PartiallySynchronisedModify c) r = c == r
isCommit _ _ = False

-- | Check if an action synchronises a given 'CRef'.
synchronises :: ActionType -> CRefId -> Bool
synchronises a r = isCommit a r || isBarrier a

-- | Get the 'CRef' affected.
crefOf :: ActionType -> Maybe CRefId
crefOf (UnsynchronisedRead  r) = Just r
crefOf (UnsynchronisedWrite r) = Just r
crefOf (SynchronisedModify  r) = Just r
crefOf (PartiallySynchronisedCommit r) = Just r
crefOf (PartiallySynchronisedWrite  r) = Just r
crefOf (PartiallySynchronisedModify r) = Just r
crefOf _ = Nothing

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
tidsOf (CommitCRef tid _) = S.singleton tid
tidsOf (STM _ tids) = S.fromList tids
tidsOf (ThrowTo tid) = S.singleton tid
tidsOf (BlockedThrowTo tid) = S.singleton tid
tidsOf _ = S.empty

-- | Throw away information from a 'ThreadAction' and give a
-- simplified view of what is happening.
--
-- This is used in the SCT code to help determine interesting
-- alternative scheduling decisions.
simplifyAction :: ThreadAction -> ActionType
simplifyAction = maybe UnsynchronisedOther simplifyLookahead . rewind

-- | Variant of 'simplifyAction' that takes a 'Lookahead'.
simplifyLookahead :: Lookahead -> ActionType
simplifyLookahead (WillPutMVar c)     = SynchronisedWrite c
simplifyLookahead (WillTryPutMVar c)  = SynchronisedWrite c
simplifyLookahead (WillReadMVar c)    = SynchronisedRead c
simplifyLookahead (WillTryReadMVar c) = SynchronisedRead c
simplifyLookahead (WillTakeMVar c)    = SynchronisedRead c
simplifyLookahead (WillTryTakeMVar c)  = SynchronisedRead c
simplifyLookahead (WillReadCRef r)     = UnsynchronisedRead r
simplifyLookahead (WillReadCRefCas r)  = UnsynchronisedRead r
simplifyLookahead (WillModCRef r)      = SynchronisedModify r
simplifyLookahead (WillModCRefCas r)   = PartiallySynchronisedModify r
simplifyLookahead (WillWriteCRef r)    = UnsynchronisedWrite r
simplifyLookahead (WillCasCRef r)      = PartiallySynchronisedWrite r
simplifyLookahead (WillCommitCRef _ r) = PartiallySynchronisedCommit r
simplifyLookahead WillSTM         = SynchronisedOther
simplifyLookahead (WillThrowTo _) = SynchronisedOther
simplifyLookahead _ = UnsynchronisedOther

-------------------------------------------------------------------------------
-- * Error reporting

-- | 'tail' but with a better error message if it fails.  Use this
-- only where it shouldn't fail!
etail :: String -> [a] -> [a]
etail _ (_:xs) = xs
etail src _ = fatal src "tail: empty list"

-- | '(!!)' but with a better error message if it fails.  Use this
-- only where it shouldn't fail!
eidx :: String -> [a] -> Int -> a
eidx src xs i
  | i < length xs = xs !! i
  | otherwise = fatal src "(!!): index too large"

-- | 'fromJust' but with a better error message if it fails.  Use this
-- only where it shouldn't fail!
efromJust :: String -> Maybe a -> a
efromJust _ (Just x) = x
efromJust src _ = fatal src "fromJust: Nothing"

-- | 'fromList' but with a better error message if it fails.  Use this
-- only where it shouldn't fail!
efromList :: String -> [a] -> NonEmpty a
efromList _ (x:xs) = x:|xs
efromList src _ = fatal src "fromList: empty list"

-- | 'M.adjust' but which errors if the key is not present.  Use this
-- only where it shouldn't fail!
eadjust :: (Ord k, Show k) => String -> (v -> v) -> k -> M.Map k v -> M.Map k v
eadjust src f k m = case M.lookup k m of
  Just v -> M.insert k (f v) m
  Nothing -> fatal src ("adjust: key '" ++ show k ++ "' not found")

-- | 'M.insert' but which errors if the key is already present.  Use
-- this only where it shouldn't fail!
einsert :: (Ord k, Show k) => String -> k -> v -> M.Map k v -> M.Map k v
einsert src k v m
  | M.member k m = fatal src ("insert: key '" ++ show k ++ "' already present")
  | otherwise = M.insert k v m

-- | 'M.lookup' but which errors if the key is not present.  Use this
-- only where it shouldn't fail!
elookup :: (Ord k, Show k) => String -> k -> M.Map k v -> v
elookup src k =
  fromMaybe (fatal src ("lookup: key '" ++ show k ++ "' not found")) .
  M.lookup k

-- | 'error' but saying where it came from
fatal :: String -> String -> a
fatal src msg = error ("(dejafu: " ++ src ++ ") " ++ msg)

-------------------------------------------------------------------------------
-- * Miscellaneous

-- | Run with a continuation that writes its value into a reference,
-- returning the computation and the reference.  Using the reference
-- is non-blocking, it is up to you to ensure you wait sufficiently.
runRefCont :: MonadRef r n => (n () -> x) -> (a -> Maybe b) -> ((a -> x) -> x) -> n (x, r (Maybe b))
runRefCont act f k = do
  ref <- newRef Nothing
  let c = k (act . writeRef ref . f)
  pure (c, ref)

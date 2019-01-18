{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Test.DejaFu.SCT.Internal
-- Copyright   : (c) 2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : BangPatterns, FlexibleContexts, LambdaCase, RankNTypes
--
-- Internal types and functions for SCT.  This module is NOT
-- considered to form part of the public interface of this library.
module Test.DejaFu.SCT.Internal where

import           Control.Monad.Conc.Class         (MonadConc)
import           Data.Coerce                      (Coercible, coerce)
import qualified Data.IntMap.Strict               as I
import           Data.List                        (find, mapAccumL)
import           Data.Maybe                       (fromMaybe)
import           GHC.Stack                        (HasCallStack)

import           Test.DejaFu.Conc
import           Test.DejaFu.Conc.Internal        (Context(..), DCSnapshot(..))
import           Test.DejaFu.Conc.Internal.Memory (commitThreadId)
import           Test.DejaFu.Internal
import           Test.DejaFu.Schedule             (Scheduler(..))
import           Test.DejaFu.SCT.Internal.DPOR
import           Test.DejaFu.Types
import           Test.DejaFu.Utils

-------------------------------------------------------------------------------
-- * Exploration

-- | General-purpose SCT function.
sct :: (MonadConc n, HasCallStack)
  => Settings n a
  -- ^ The SCT settings ('Way' is ignored)
  -> ([ThreadId] -> s)
  -- ^ Initial state
  -> (s -> Maybe t)
  -- ^ State predicate
  -> ((Scheduler g -> g -> n (Either Condition a, g, Trace)) -> s -> t -> n (s, Maybe (Either Condition a, Trace)))
  -- ^ Run the computation and update the state
  -> ConcT n a
  -> n [(Either Condition a, Trace)]
sct settings s0 sfun srun conc
    | canDCSnapshot conc = runForDCSnapshot conc >>= \case
        Just (Right snap, _) -> sct'Snap snap
        Just (Left f, trace) -> pure [(Left f, trace)]
        _ -> do
          debugFatal "Failed to construct snapshot, continuing without."
          sct'Full
    | otherwise = sct'Full
  where
    sct'Full = sct'
      settings
      (s0 [initialThread])
      sfun
      (srun runFull)
      runFull
      (toId 1)
      (toId 1)

    sct'Snap snap = let idsrc = cIdSource (dcsContext snap) in sct'
      settings
      (s0 (fst (threadsFromDCSnapshot snap)))
      sfun
      (srun (runSnap snap))
      (runSnap snap)
      (toId $ 1 + fst (_tids idsrc))
      (toId $ 1 + fst (_iorids idsrc))

    runFull sched s = runConcurrent sched (_memtype settings) s conc
    runSnap snap sched s = runWithDCSnapshot sched (_memtype settings) s snap

    debugFatal = if _debugFatal settings then fatal else debugPrint
    debugPrint = fromMaybe (const (pure ())) (_debugPrint settings)

-- | Like 'sct' but given a function to run the computation.
sct' :: (MonadConc n, HasCallStack)
  => Settings n a
  -- ^ The SCT settings ('Way' is ignored)
  -> s
  -- ^ Initial state
  -> (s -> Maybe t)
  -- ^ State predicate
  -> (s -> t -> n (s, Maybe (Either Condition a, Trace)))
  -- ^ Run the computation and update the state
  -> (forall x. Scheduler x -> x -> n (Either Condition a, x, Trace))
  -- ^ Just run the computation
  -> ThreadId
  -- ^ The first available @ThreadId@
  -> IORefId
  -- ^ The first available @IORefId@
  -> n [(Either Condition a, Trace)]
sct' settings s0 sfun srun run nTId nCRId = go Nothing [] s0 where
  go (Just res) _ _ | earlyExit res = pure []
  go _ seen !s = case sfun s of
    Just t -> srun s t >>= \case
      (s', Just (res, trace)) -> case discard res of
        Just DiscardResultAndTrace -> go (Just res) seen s'
        Just DiscardTrace -> result res [] seen s'
        Nothing -> result res trace seen s'
      (s', Nothing) -> go Nothing seen s'
    Nothing -> pure []

  -- Sadly, we have to use a list to store the set of unique results,
  -- as we don't have an @Ord a@ dict hanging around.  I suspect that
  -- most test cases will have a relatively small number of unique
  -- results, compared to the number of executions, however.
  -- Pathological cases (like IORef ones in dejafu-tests which produce
  -- a different result on every execution) are probably uncommon.
  result = case _equality settings of
    Just f -> \res trace seen s ->
      let eq cmp (Right a1) (Right a2) = cmp a1 a2
          eq _   (Left  e1) (Left  e2) = e1 == e2
          eq _ _ _ = False
      in if any (eq f res) seen
         then go (Just res) seen s
         else dosimplify res trace (res:seen) s
    Nothing -> dosimplify

  dosimplify res [] seen s = ((res, []) :) <$> go (Just res) seen s
  dosimplify res trace seen s
    | not (_simplify settings) = ((res, trace) :) <$> go (Just res) seen s
    | otherwise = do
        shrunk <- simplifyExecution settings run nTId nCRId res trace
        (shrunk :) <$> go (Just res) seen s

  earlyExit = fromMaybe (const False) (_earlyExit settings)
  discard = fromMaybe (const Nothing) (_discard settings)

-- | Given a result and a trace, produce a more minimal trace.
--
-- In principle, simplification is semantics preserving and can be
-- done without needing to execute the computation again.  However,
-- there are two good reasons to do so:
--
--  * It's a sanity check that there are no bugs.
--  * It's easier to generate a reduced sequence of scheduling
--    decisions and let dejafu generate the full trace, than to
--    generate a reduced trace directly
--
-- Unlike shrinking in randomised property-testing tools like
-- QuickCheck or Hedgehog, we only run the test case /once/, at the
-- end, rather than after every simplification step.
simplifyExecution :: (MonadConc n, HasCallStack)
  => Settings n a
  -- ^ The SCT settings ('Way' is ignored)
  -> (forall x. Scheduler x -> x -> n (Either Condition a, x, Trace))
  -- ^ Just run the computation
  -> ThreadId
  -- ^ The first available @ThreadId@
  -> IORefId
  -- ^ The first available @IORefId@
  -> Either Condition a
  -- ^ The expected result
  -> Trace
  -> n (Either Condition a, Trace)
simplifyExecution settings run nTId nCRId res trace
    | tidTrace == simplifiedTrace = do
        debugPrint ("Simplifying new result '" ++ p res ++ "': no simplification possible!")
        pure (res, trace)
    | otherwise = do
        debugPrint ("Simplifying new result '" ++ p res ++ "': OK!")
        (res', _, trace') <- replay run (fixup simplifiedTrace)
        case (_equality settings, res, res') of
          (Just f,  Right a1, Right a2) | f a1 a2  -> pure (res', trace')
          (_,       Left  e1, Left  e2) | e1 == e2 -> pure (res', trace')
          (Nothing, Right _,  Right _) -> pure (res', trace') -- this is a risky case!
          _ -> do
            debugFatal ("Got a different result after simplifying: '" ++ p res ++ "' /= '" ++ p res' ++ "'")
            pure (res, trace)
  where
    tidTrace = toTIdTrace trace
    simplifiedTrace = simplify (_safeIO settings) (_memtype settings) tidTrace
    fixup = renumber (_memtype settings) (fromId nTId) (fromId nCRId)

    debugFatal = if _debugFatal settings then fatal else debugPrint
    debugPrint = fromMaybe (const (pure ())) (_debugPrint settings)
    debugShow = fromMaybe (const "_") (_debugShow settings)
    p = either show debugShow

-- | Replay an execution.
replay :: MonadConc n
  => (forall x. Scheduler x -> x -> n (Either Condition a, x, Trace))
  -- ^ Run the computation
  -> [(ThreadId, ThreadAction)]
  -- ^ The reduced sequence of scheduling decisions
  -> n (Either Condition a, [(ThreadId, ThreadAction)], Trace)
replay run = run (Scheduler (const sched)) where
    sched runnable ((t, Stop):ts) = case findThread t runnable of
      Just t' -> (Just t', ts)
      Nothing -> sched runnable ts
    sched runnable ((t, _):ts) = (findThread t runnable, ts)
    sched _ _ = (Nothing, [])

    -- find a thread ignoring names
    findThread tid0 =
      fmap fst . find (\(tid,_) -> fromId tid == fromId tid0)

-------------------------------------------------------------------------------
-- * Schedule simplification

-- | Simplify a trace by permuting adjacent independent actions to
-- reduce context switching.
simplify :: Bool -> MemType -> [(ThreadId, ThreadAction)] -> [(ThreadId, ThreadAction)]
simplify safeIO memtype trc0 = loop (length trc0) (prepare trc0) where
  prepare = dropCommits safeIO memtype . lexicoNormalForm safeIO memtype
  step = pushForward safeIO memtype . pullBack safeIO memtype

  loop 0 trc = trc
  loop n trc =
    let trc' = step trc
    in if trc' /= trc then loop (n-1) trc' else trc

-- | Put a trace into lexicographic (by thread ID) normal form.
lexicoNormalForm :: Bool -> MemType -> [(ThreadId, ThreadAction)] -> [(ThreadId, ThreadAction)]
lexicoNormalForm safeIO memtype = go where
  go trc =
    let trc' = permuteBy safeIO memtype (repeat (>)) trc
    in if trc == trc' then trc else go trc'

-- | Swap adjacent independent actions in the trace if a predicate
-- holds.
permuteBy
  :: Bool
  -> MemType
  -> [ThreadId -> ThreadId -> Bool]
  -> [(ThreadId, ThreadAction)]
  -> [(ThreadId, ThreadAction)]
permuteBy safeIO memtype = go initialDepState where
  go ds (p:ps) (t1@(tid1, ta1):t2@(tid2, ta2):trc)
    | independent safeIO ds tid1 ta1 tid2 ta2 && p tid1 tid2 = go' ds ps t2 (t1 : trc)
    | otherwise = go' ds ps t1 (t2 : trc)
  go _ _ trc = trc

  go' ds ps t@(tid, ta) trc = t : go (updateDepState memtype ds tid ta) ps trc

-- | Throw away commit actions which are followed by a memory barrier.
dropCommits :: Bool -> MemType -> [(ThreadId, ThreadAction)] -> [(ThreadId, ThreadAction)]
dropCommits _ SequentialConsistency = id
dropCommits safeIO memtype = go initialDepState where
  go ds (t1@(tid1, ta1@(CommitIORef _ iorefid)):t2@(tid2, ta2):trc)
    | isBarrier (simplifyAction ta2) && numBuffered ds iorefid == 1 = go ds (t2:trc)
    | independent safeIO ds tid1 ta1 tid2 ta2 = t2 : go (updateDepState memtype ds tid2 ta2) (t1:trc)
  go ds (t@(tid,ta):trc) = t : go (updateDepState memtype ds tid ta) trc
  go _ [] = []

-- | Attempt to reduce context switches by \"pulling\" thread actions
-- back to a prior execution of that thread.
--
-- Simple example, say we have @[(tidA, act1), (tidB, act2), (tidA,
-- act3)]@, where @act2@ and @act3@ are independent.  In this case
-- 'pullBack' will swap them, giving the sequence @[(tidA, act1),
-- (tidA, act3), (tidB, act2)]@.  It works for arbitrary separations.
pullBack :: Bool -> MemType -> [(ThreadId, ThreadAction)] -> [(ThreadId, ThreadAction)]
pullBack safeIO memtype = go initialDepState where
  go ds (t1@(tid1, ta1):trc@((tid2, _):_)) =
    let ds' = updateDepState memtype ds tid1 ta1
        trc' = if tid1 /= tid2
               then maybe trc (uncurry (:)) (findAction tid1 ds' trc)
               else trc
    in t1 : go ds' trc'
  go _ trc = trc

  findAction tid0 = fgo where
    fgo ds (t@(tid, ta):trc)
      | tid == tid0 = Just (t, trc)
      | otherwise = case fgo (updateDepState memtype ds tid ta) trc of
          Just (ft@(ftid, fa), trc')
            | independent safeIO ds tid ta ftid fa -> Just (ft, t:trc')
          _ -> Nothing
    fgo _ _ = Nothing

-- | Attempt to reduce context switches by \"pushing\" thread actions
-- forward to a future execution of that thread.
--
-- This is kind of the opposite of 'pullBack', but there are cases
-- where one applies but not the other.
--
-- Simple example, say we have @[(tidA, act1), (tidB, act2), (tidA,
-- act3)]@, where @act1@ and @act2@ are independent.  In this case
-- 'pushForward' will swap them, giving the sequence @[(tidB, act2),
-- (tidA, act1), (tidA, act3)]@.  It works for arbitrary separations.
pushForward :: Bool -> MemType -> [(ThreadId, ThreadAction)] -> [(ThreadId, ThreadAction)]
pushForward safeIO memtype = go initialDepState where
  go ds (t1@(tid1, ta1):trc@((tid2, _):_)) =
    let ds' = updateDepState memtype ds tid1 ta1
    in if tid1 /= tid2
       then maybe (t1 : go ds' trc) (go ds) (findAction tid1 ta1 ds trc)
       else t1 : go ds' trc
  go _ trc = trc

  findAction tid0 ta0 = fgo where
    fgo ds (t@(tid, ta):trc)
      | tid == tid0 = Just ((tid0, ta0) : t : trc)
      | independent safeIO ds tid0 ta0 tid ta = (t:) <$> fgo (updateDepState memtype ds tid ta) trc
      | otherwise = Nothing
    fgo _ _ = Nothing

-- | Re-number threads and IORefs.
--
-- Permuting forks or newIORefs makes the existing numbering invalid,
-- which then causes problems for scheduling.  Just re-numbering
-- threads isn't enough, as IORef IDs are used to determine commit
-- thread IDs.
--
-- Renumbered things will not fix their names, so don't rely on those
-- at all.
renumber
  :: MemType
  -- ^ The memory model determines how commit threads are numbered.
  -> Int
  -- ^ First free thread ID.
  -> Int
  -- ^ First free @IORef@ ID.
  -> [(ThreadId, ThreadAction)]
  -> [(ThreadId, ThreadAction)]
renumber memtype tid0 crid0 = snd . mapAccumL go (I.empty, tid0, I.empty, crid0) where
  go s@(tidmap, _, cridmap, _) (_, CommitIORef tid crid) =
    let tid'  = renumbered tidmap  tid
        crid' = renumbered cridmap crid
        act' = CommitIORef tid' crid'
    in case memtype of
         PartialStoreOrder -> (s, (commitThreadId tid' (Just crid'), act'))
         _ -> (s, (commitThreadId tid' Nothing, act'))
  go s@(tidmap, _, _, _) (tid, act) =
    let (s', act') = updateAction s act
    in (s', (renumbered tidmap tid, act'))

  -- I can't help but feel there should be some generic programming
  -- solution to this sort of thing (and to the many other functions
  -- operating over @ThreadAction@s / @Lookahead@s)
  updateAction (tidmap, nTId, cridmap, nCRId) (Fork old) =
    let tidmap' = I.insert (fromId old) nTId tidmap
        nTId' = nTId + 1
    in ((tidmap', nTId', cridmap, nCRId), Fork (toId nTId))
  updateAction (tidmap, nTId, cridmap, nCRId) (ForkOS old) =
    let tidmap' = I.insert (fromId old) nTId tidmap
        nTId' = nTId + 1
    in ((tidmap', nTId', cridmap, nCRId), ForkOS (toId nTId))
  updateAction s@(tidmap, _, _, _) (PutMVar mvid olds) =
    (s, PutMVar mvid (map (renumbered tidmap) olds))
  updateAction s@(tidmap, _, _, _) (TryPutMVar mvid b olds) =
    (s, TryPutMVar mvid b (map (renumbered tidmap) olds))
  updateAction s@(tidmap, _, _, _) (TakeMVar mvid olds) =
    (s, TakeMVar mvid (map (renumbered tidmap) olds))
  updateAction s@(tidmap, _, _, _) (TryTakeMVar mvid b olds) =
    (s, TryTakeMVar mvid b (map (renumbered tidmap) olds))
  updateAction (tidmap, nTId, cridmap, nCRId) (NewIORef old) =
    let cridmap' = I.insert (fromId old) nCRId cridmap
        nCRId' = nCRId + 1
    in ((tidmap, nTId, cridmap', nCRId'), NewIORef (toId nCRId))
  updateAction s@(_, _, cridmap, _) (ReadIORef old) =
    (s, ReadIORef (renumbered cridmap old))
  updateAction s@(_, _, cridmap, _) (ReadIORefCas old) =
    (s, ReadIORefCas (renumbered cridmap old))
  updateAction s@(_, _, cridmap, _) (ModIORef old) =
    (s, ModIORef (renumbered cridmap old))
  updateAction s@(_, _, cridmap, _) (ModIORefCas old) =
    (s, ModIORefCas (renumbered cridmap old))
  updateAction s@(_, _, cridmap, _) (WriteIORef old) =
    (s, WriteIORef (renumbered cridmap old))
  updateAction s@(_, _, cridmap, _) (CasIORef old b) =
    (s, CasIORef (renumbered cridmap old) b)
  updateAction s@(tidmap, _, _, _) (STM tas olds) =
    (s, STM tas (map (renumbered tidmap) olds))
  updateAction s@(tidmap, _, _, _) (ThrowTo old b) =
    (s, ThrowTo (renumbered tidmap old) b)
  updateAction s@(tidmap, _, _, _) (BlockedThrowTo old) =
    (s, BlockedThrowTo (renumbered tidmap old))
  updateAction s act = (s, act)

  renumbered :: (Coercible a Id, Coercible Id a) => I.IntMap Int -> a -> a
  renumbered idmap id_ = toId $ I.findWithDefault (fromId id_) (fromId id_) idmap

-------------------------------------------------------------------------------
-- * Utilities

-- | Helper function for constructing IDs of any sort.
toId :: Coercible Id a => Int -> a
toId = coerce . Id Nothing

-- | Helper function for deconstructing IDs of any sort.
fromId :: Coercible a Id => a -> Int
fromId a = let (Id _ id_) = coerce a in id_

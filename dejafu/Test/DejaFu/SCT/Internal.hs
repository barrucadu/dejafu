{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Test.DejaFu.SCT.Internal
-- Copyright   : (c) 2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : BangPatterns, LambdaCase, RankNTypes
--
-- Internal types and functions for SCT.  This module is NOT
-- considered to form part of the public interface of this library.
module Test.DejaFu.SCT.Internal where

import           Control.Monad.Conc.Class      (MonadConc)
import           Control.Monad.Ref             (MonadRef)
import           Data.Maybe                    (fromMaybe)

import           Test.DejaFu.Conc
import           Test.DejaFu.Internal
import           Test.DejaFu.Schedule          (Scheduler(..))
import           Test.DejaFu.SCT.Internal.DPOR
import           Test.DejaFu.Types
import           Test.DejaFu.Utils

-------------------------------------------------------------------------------
-- * Exploration

-- | General-purpose SCT function.
sct :: (MonadConc n, MonadRef r n)
  => Settings n a
  -- ^ The SCT settings ('Way' is ignored)
  -> ([ThreadId] -> s)
  -- ^ Initial state
  -> (s -> Maybe t)
  -- ^ State predicate
  -> ((Scheduler g -> g -> n (Either Failure a, g, Trace)) -> s -> t -> n (s, Maybe (Either Failure a, Trace)))
  -- ^ Run the computation and update the state
  -> ConcT r n a
  -> n [(Either Failure a, Trace)]
sct settings s0 sfun srun conc
    | canDCSnapshot conc = runForDCSnapshot conc >>= \case
        Just (Right snap, _) -> sct' settings (s0 (fst (threadsFromDCSnapshot snap))) sfun (srun (runSnap snap)) (runSnap snap)
        Just (Left f, trace) -> pure [(Left f, trace)]
        _ -> do
          debugPrint "Failed to construct snapshot, continuing without."
          sct' settings (s0 [initialThread]) sfun (srun runFull) runFull
    | otherwise = sct' settings (s0 [initialThread]) sfun (srun runFull) runFull
  where
    runFull sched s = runConcurrent sched (_memtype settings) s conc
    runSnap snap sched s = runWithDCSnapshot sched (_memtype settings) s snap

    debugPrint = fromMaybe (const (pure ())) (_debugPrint settings)

-- | Like 'sct' but given a function to run the computation.
sct' :: (MonadConc n, MonadRef r n)
  => Settings n a
  -- ^ The SCT settings ('Way' is ignored)
  -> s
  -- ^ Initial state
  -> (s -> Maybe t)
  -- ^ State predicate
  -> (s -> t -> n (s, Maybe (Either Failure a, Trace)))
  -- ^ Run the computation and update the state
  -> (forall x. Scheduler x -> x -> n (Either Failure a, x, Trace))
  -- ^ Just run the computation
  -> n [(Either Failure a, Trace)]
sct' settings s0 sfun srun run = go Nothing [] s0 where
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
         else doshrink res trace (res:seen) s
    Nothing -> doshrink

  doshrink res [] seen s = ((res, []) :) <$> go (Just res) seen s
  doshrink res trace seen s
    | not (_shrink settings) = ((res, trace) :) <$> go (Just res) seen s
    | otherwise = do
        shrunk <- shrink settings run res trace
        (shrunk :) <$> go (Just res) seen s

  earlyExit = fromMaybe (const False) (_earlyExit settings)
  discard = fromMaybe (const Nothing) (_discard settings)

-- | Given a result and a trace, produce a more minimal trace.
--
-- In principle, shrinking is semantics preserving and can be done
-- without needing to execute the computation again.  However, there
-- are two good reasons to do so:
--
--  * It's a sanity check that there are no bugs.
--  * It's easier to generate a reduced sequence of scheduling
--    decisions and let dejafu generate the full trace, than to
--    generate a reduced trace directly
--
-- Unlike shrinking in randomised property-testing tools like
-- QuickCheck or Hedgehog, we only run the test case /once/, at the
-- end, rather than after every shrinking step.
shrink :: (MonadConc n, MonadRef r n)
  => Settings n a
  -- ^ The SCT settings ('Way' is ignored)
  -> (forall x. Scheduler x -> x -> n (Either Failure a, x, Trace))
  -- ^ Just run the computation
  -> Either Failure a
  -- ^ The result to shrink to
  -> Trace
  -> n (Either Failure a, Trace)
shrink settings run res trace
    | tidTrace == normalisedTrace = do
        debugPrint ("Shrinking new result '" ++ p res ++ "': no shrink possible!")
        pure (res, trace)
    | otherwise = do
        debugPrint ("Shrinking new result '" ++ p res ++ "': OK!")
        (res', _, trace') <- replay run normalisedTrace
        case (_equality settings, res, res') of
          (Just f,  Right a1, Right a2) | f a1 a2  -> pure (res', trace')
          (_,       Left  e1, Left  e2) | e1 == e2 -> pure (res', trace')
          (Nothing, Right _,  Right _) -> pure (res', trace') -- this is a risky case!
          _ -> do
            debugPrint ("Got a different result after shrinking: '" ++ p res ++ "' /= '" ++ p res' ++ "'")
            pure (res, trace)
  where
    tidTrace = toTIdTrace trace
    normalisedTrace = normalise (_memtype settings) tidTrace

    debugPrint = fromMaybe (const (pure ())) (_debugPrint settings)
    debugShow = fromMaybe (const "_") (_debugShow settings)
    p = either show debugShow

-- | Replay an execution.
replay :: (MonadConc n, MonadRef r n)
  => (forall x. Scheduler x -> x -> n (Either Failure a, x, Trace))
  -- ^ Run the computation
  -> [(ThreadId, ThreadAction)]
  -- ^ The reduced sequence of scheduling decisions
  -> n (Either Failure a, [(ThreadId, ThreadAction)], Trace)
replay run = run (Scheduler sched) where
    sched prior runnable ((t, Stop):ts)
      | any ((==t) . fst) runnable = (Just t, ts)
      | otherwise = sched prior runnable ts
    sched _ _ ((t, _):ts) = (Just t, ts)
    sched _ _ _ = (Nothing, [])

-------------------------------------------------------------------------------
-- * Schedule normalisation

-- | Normalise a trace by permuting adjacent independent actions to
-- reduce context switching.
normalise :: MemType -> [(ThreadId, ThreadAction)] -> [(ThreadId, ThreadAction)]
normalise memtype trc0 = go (length trc0) trc0 where
  go 0 trc = trc
  go n trc =
    let trc' = dropCommits memtype . pushForward memtype . pullBack memtype . preferLowest $ trc
    in if trc' /= trc then go (n-1) trc' else trc

  preferLowest = permuteBy memtype . repeat $ \tid1 tid2 -> tid1 > tid2

-- | Swap adjacent independent actions in the trace if a predicate
-- holds.
permuteBy
  :: MemType
  -> [ThreadId -> ThreadId -> Bool]
  -> [(ThreadId, ThreadAction)]
  -> [(ThreadId, ThreadAction)]
permuteBy memtype = go initialDepState where
  go ds (p:ps) (t1@(tid1, ta1):t2@(tid2, ta2):trc)
    | independent ds tid1 ta1 tid2 ta2 && p tid1 tid2 = go' ds ps t2 (t1 : trc)
    | otherwise = go' ds ps t1 (t2 : trc)
  go _ _ trc = trc

  go' ds ps t@(tid, ta) trc = t : go (updateDepState memtype ds tid ta) ps trc

-- | Attempt to reduce context switches by \"pulling\" thread actions
-- back to a prior execution of that thread.
--
-- Simple example, say we have @[(tidA, act1), (tidB, act2), (tidA,
-- act3)]@, where @act2@ and @act3@ are independent.  In this case
-- 'pullBack' will swap them, giving the sequence @[(tidA, act1),
-- (tidA, act3), (tidB, act2)]@.  It works for arbitrary separations.
pullBack :: MemType -> [(ThreadId, ThreadAction)] -> [(ThreadId, ThreadAction)]
pullBack memtype = go initialDepState where
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
            | independent ds tid ta ftid fa -> Just (ft, t:trc')
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
pushForward :: MemType -> [(ThreadId, ThreadAction)] -> [(ThreadId, ThreadAction)]
pushForward memtype = go initialDepState where
  go ds (t1@(tid1, ta1):trc@((tid2, _):_)) =
    let ds' = updateDepState memtype ds tid1 ta1
    in if tid1 /= tid2
       then maybe (t1 : go ds' trc) (go ds) (findAction tid1 ta1 ds trc)
       else t1 : go ds' trc
  go _ trc = trc

  findAction tid0 ta0 = fgo where
    fgo ds (t@(tid, ta):trc)
      | tid == tid0 = Just ((tid0, ta0) : t : trc)
      | independent ds tid0 ta0 tid ta = (t:) <$> fgo (updateDepState memtype ds tid ta) trc
      | otherwise = Nothing
    fgo _ _ = Nothing

-- | Throw away commit actions which are followed by a memory barrier.
dropCommits :: MemType -> [(ThreadId, ThreadAction)] -> [(ThreadId, ThreadAction)]
dropCommits SequentialConsistency = id
dropCommits memtype = go initialDepState where
  go ds (t1@(tid1, ta1@(CommitCRef _ _)):t2@(tid2, ta2):trc)
    | isBarrier (simplifyAction ta2) = go ds (t2:trc)
    | independent ds tid1 ta1 tid2 ta2 = t2 : go (updateDepState memtype ds tid2 ta2) (t1:trc)
  go ds (t@(tid,ta):trc) = t : go (updateDepState memtype ds tid ta) trc
  go _ [] = []

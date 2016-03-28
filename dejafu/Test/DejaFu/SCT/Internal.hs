-- | Internal utilities and types for BPOR.
module Test.DejaFu.SCT.Internal where

import Data.Map.Strict (Map)
import Data.Maybe (isJust, fromJust)
import Test.DejaFu.Deterministic.Internal hiding (Decision(..))
import Test.DejaFu.DPOR (BacktrackStep(..), Decision(..), DPOR(..), decisionOf, tidOf)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- * BPOR state

type BPOR = DPOR ThreadId ThreadAction

-- | Add new backtracking points, if they have not already been
-- visited, fit into the bound, and aren't in the sleep set.
todo :: ([(Decision ThreadId, ThreadAction)] -> (Decision ThreadId, Lookahead) -> Bool) -> [BacktrackStep ThreadId ThreadAction Lookahead state] -> BPOR -> BPOR
todo bv = go Nothing [] where
  go priorTid pref (b:bs) bpor =
    let bpor' = doBacktrack priorTid pref b bpor
        tid   = bcktThreadid b
        pref' = pref ++ [bcktDecision b]
        child = go (Just tid) pref' bs . fromJust $ M.lookup tid (dporDone bpor)
    in bpor' { dporDone = M.insert tid child $ dporDone bpor' }

  go _ _ [] bpor = bpor

  doBacktrack priorTid pref b bpor =
    let todo' = [ x
                | x@(t,c) <- M.toList $ bcktBacktracks b
                , let decision  = decisionOf priorTid (dporRunnable bpor) t
                , let lahead = fromJust . M.lookup t $ bcktRunnable b
                , bv pref (decision, lahead)
                , t `notElem` M.keys (dporDone bpor)
                , c || M.notMember t (dporSleep bpor)
                ]
    in bpor { dporTodo = dporTodo bpor `M.union` M.fromList todo' }

-- | Remove commits from the todo sets where every other action will
-- result in a write barrier (and so a commit) occurring.
--
-- To get the benefit from this, do not execute commit actions from
-- the todo set until there are no other choises.
pruneCommits :: BPOR -> BPOR
pruneCommits bpor
  | not onlycommits || not alldonesync = go bpor
  | otherwise = go bpor { dporTodo = M.empty }

  where
    go b = b { dporDone = pruneCommits <$> dporDone bpor }

    onlycommits = all (<initialThread) . M.keys $ dporTodo bpor
    alldonesync = all barrier . M.elems $ dporDone bpor

    barrier = isBarrier . simplify . fromJust . dporAction

-- * Utilities

-- | Check if an action is dependent on another.
dependent :: MemType -> CRState -> (ThreadId, ThreadAction) -> (ThreadId, ThreadAction) -> Bool
dependent _ _ (_, Lift) (_, Lift) = True
dependent _ _ (_, ThrowTo t) (t2, Stop) | t == t2 = False
dependent _ _ (t2, Stop) (_, ThrowTo t) | t == t2 = False
dependent _ _ (_, ThrowTo t) (t2, _) = t == t2
dependent _ _ (t2, _) (_, ThrowTo t) = t == t2
dependent _ _ (_, STM _ _) (_, STM _ _) = True
dependent _ _ (_, GetNumCapabilities a) (_, SetNumCapabilities b) = a /= b
dependent _ _ (_, SetNumCapabilities a) (_, GetNumCapabilities b) = a /= b
dependent _ _ (_, SetNumCapabilities a) (_, SetNumCapabilities b) = a /= b
dependent memtype buf (_, d1) (_, d2) = dependentActions memtype buf (simplify d1) (simplify d2)

-- | Variant of 'dependent' to handle 'ThreadAction''s
dependent' :: MemType -> CRState -> (ThreadId, ThreadAction) -> (ThreadId, Lookahead) -> Bool
dependent' _ _ (_, Lift) (_, WillLift) = True
dependent' _ _ (_, ThrowTo t) (t2, WillStop) | t == t2 = False
dependent' _ _ (t2, Stop) (_, WillThrowTo t) | t == t2 = False
dependent' _ _ (_, ThrowTo t) (t2, _)     = t == t2
dependent' _ _ (t2, _) (_, WillThrowTo t) = t == t2
dependent' _ _ (_, STM _ _) (_, WillSTM) = True
dependent' _ _ (_, GetNumCapabilities a) (_, WillSetNumCapabilities b) = a /= b
dependent' _ _ (_, SetNumCapabilities _) (_, WillGetNumCapabilities)   = True
dependent' _ _ (_, SetNumCapabilities a) (_, WillSetNumCapabilities b) = a /= b
-- This is safe because, if the thread blocks anyway, a context switch
-- will occur anyway so there's no point pre-empting the action.
--
-- UNLESS the pre-emption would possibly allow for a different relaxed
-- memory stage.
dependent' _ _ (_, a1) (_, a2) | isBlock a1 && isBarrier (simplify' a2) = False
dependent' memtype buf (_, d1) (_, d2) = dependentActions memtype buf (simplify d1) (simplify' d2)

-- | Check if two 'ActionType's are dependent. Note that this is not
-- sufficient to know if two 'ThreadAction's are dependent, without
-- being so great an over-approximation as to be useless!
dependentActions :: MemType -> CRState -> ActionType -> ActionType -> Bool
dependentActions memtype buf a1 a2 = case (a1, a2) of
  -- Unsynchronised reads and writes are always dependent, even under
  -- a relaxed memory model, as an unsynchronised write gives rise to
  -- a commit, which synchronises.
  (UnsynchronisedRead  r1, UnsynchronisedWrite r2) -> r1 == r2
  (UnsynchronisedWrite r1, UnsynchronisedRead  r2) -> r1 == r2
  (UnsynchronisedWrite r1, UnsynchronisedWrite r2) -> r1 == r2

  -- Unsynchronised writes and synchronisation where the buffer is not
  -- empty.
  --
  -- See [RMMVerification], lemma 5.25.
  (UnsynchronisedWrite r1, _) | same crefOf && isCommit a2 r1 && isBuffered buf r1 -> False
  (_, UnsynchronisedWrite r2) | same crefOf && isCommit a1 r2 && isBuffered buf r2 -> False

  -- Unsynchronised reads where a memory barrier would flush a
  -- buffered write
  (UnsynchronisedRead r1, _) | isBarrier a2 -> isBuffered buf r1 && memtype /= SequentialConsistency
  (_, UnsynchronisedRead r2) | isBarrier a1 -> isBuffered buf r2 && memtype /= SequentialConsistency

  (_, _)
    -- Two actions on the same CRef where at least one is synchronised
    | same crefOf && (synchronises a1 (fromJust $ crefOf a1) || synchronises a2 (fromJust $ crefOf a2)) -> True
    -- Two actions on the same MVar
    | same cvarOf -> True

  _ -> False

  where
    same f = isJust (f a1) && f a1 == f a2

-- * Keeping track of 'CRef' buffer state

data CRState = Known (Map CRefId Bool) | Unknown
  deriving (Eq, Show)

-- | Initial global 'CRef buffer state.
initialCRState :: CRState
initialCRState = Known M.empty

-- | 'CRef' buffer state with nothing known.
unknownCRState :: CRState
unknownCRState = Unknown

-- | Update the 'CRef' buffer state with the action that has just
-- happened.
updateCRState :: CRState -> ThreadAction -> CRState
updateCRState Unknown _ = Unknown
updateCRState (Known crstate) (CommitRef _ r) = Known $ M.delete r crstate
updateCRState (Known crstate) (WriteRef r) = Known $ M.insert r True crstate
updateCRState crstate ta
  | isBarrier $ simplify ta = initialCRState
  | otherwise = crstate

-- | Check if a 'CRef' has a buffered write pending.
--
-- If the state is @Unknown@, this assumes @True@.
isBuffered :: CRState -> CRefId -> Bool
isBuffered Unknown _ = True
isBuffered (Known crstate) r = M.findWithDefault False r crstate

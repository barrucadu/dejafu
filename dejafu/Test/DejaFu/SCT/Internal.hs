{-# LANGUAGE CPP #-}

-- | Internal utilities and types for BPOR.
module Test.DejaFu.SCT.Internal where

import Control.DeepSeq (NFData(..))
import Data.List (foldl', partition, sortBy)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe, isJust, fromJust)
import Data.Ord (Down(..), comparing)
import Data.Sequence (Seq, ViewL(..))
import Data.Set (Set)
import Test.DejaFu.Deterministic.Internal
import Test.DejaFu.Deterministic.Schedule

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Sq
import qualified Data.Set as S

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

-- * BPOR state

-- | One step of the execution, including information for backtracking
-- purposes. This backtracking information is used to generate new
-- schedules.
data BacktrackStep = BacktrackStep
  { _threadid  :: ThreadId
  -- ^ The thread running at this step
  , _decision  :: (Decision, ThreadAction)
  -- ^ What happened at this step.
  , _runnable  :: Map ThreadId Lookahead
  -- ^ The threads runnable at this step
  , _backtrack :: Map ThreadId Bool
  -- ^ The list of alternative threads to run, and whether those
  -- alternatives were added conservatively due to the bound.
  } deriving (Eq, Show)

instance NFData BacktrackStep where
  rnf b = rnf (_threadid b, _decision b, _runnable b, _backtrack b)

-- | BPOR execution is represented as a tree of states, characterised
-- by the decisions that lead to that state.
data BPOR = BPOR
  { _brunnable :: Set ThreadId
  -- ^ What threads are runnable at this step.
  , _btodo     :: Map ThreadId Bool
  -- ^ Follow-on decisions still to make, and whether that decision
  -- was added conservatively due to the bound.
  , _bignore   :: Set ThreadId
  -- ^ Follow-on decisions never to make, because they will result in
  -- the chosen thread immediately blocking without achieving
  -- anything, which can't have any effect on the result of the
  -- program.
  , _bdone     :: Map ThreadId BPOR
  -- ^ Follow-on decisions that have been made.
  , _bsleep    :: Map ThreadId ThreadAction
  -- ^ Transitions to ignore (in this node and children) until a
  -- dependent transition happens.
  , _btaken    :: Map ThreadId ThreadAction
  -- ^ Transitions which have been taken, excluding
  -- conservatively-added ones. This is used in implementing sleep
  -- sets.
  , _baction    :: Maybe ThreadAction
  -- ^ What happened at this step. This will be 'Nothing' at the root,
  -- 'Just' everywhere else.
  }

-- | Initial BPOR state.
initialState :: BPOR
initialState = BPOR
  { _brunnable = S.singleton (ThreadId 0)
  , _btodo     = M.singleton (ThreadId 0) False
  , _bignore   = S.empty
  , _bdone     = M.empty
  , _bsleep    = M.empty
  , _btaken    = M.empty
  , _baction   = Nothing
  }

-- | Produce a new schedule from a BPOR tree. If there are no new
-- schedules remaining, return 'Nothing'. Also returns whether the
-- decision made was added conservatively.
--
-- This returns the longest prefix, on the assumption that this will
-- lead to lots of backtracking points being identified before
-- higher-up decisions are reconsidered, so enlarging the sleep sets.
next :: BPOR -> Maybe ([ThreadId], Bool)
next = go 0 where
  go tid bpor =
        -- All the possible prefix traces from this point, with
        -- updated BPOR subtrees if taken from the done list.
    let prefixes = mapMaybe go' (M.toList $ _bdone bpor) ++ [([t], c) | (t, c) <- M.toList $ _btodo bpor]
        -- Sort by number of preemptions, in descending order.
        cmp = preEmps tid bpor . fst

    in if null prefixes
       then Nothing
       else case partition (\(t:_,_) -> t < 0) $ sortBy (comparing $ Down . cmp) prefixes of
              (_, (ts,c):_) -> Just (ts, c)
              ((ts,c):_, _) -> Just (ts, c)
              ([], []) -> error "Invariant failure in 'next': empty prefix list!"

  go' (tid, bpor) = (\(ts,c) -> (tid:ts,c)) <$> go tid bpor

  preEmps tid bpor (t:ts) =
    let rest = preEmps t (fromJust . M.lookup t $ _bdone bpor) ts
    in  if t > 0 && tid /= t && tid `S.member` _brunnable bpor then 1 + rest else rest
  preEmps _ _ [] = 0::Int

-- | Produce a list of new backtracking points from an execution
-- trace.
findBacktrack :: MemType
  -> ([BacktrackStep] -> Int -> ThreadId -> [BacktrackStep])
  -> Seq (NonEmpty (ThreadId, Lookahead), [ThreadId])
  -> Trace'
  -> [BacktrackStep]
findBacktrack memtype backtrack = go initialCRState S.empty 0 [] . Sq.viewl where
  go crstate allThreads tid bs ((e,i):<is) ((d,_,a):ts) =
    let tid' = tidOf tid d
        crstate' = updateCRState crstate a
        this = BacktrackStep
          { _threadid  = tid'
          , _decision  = (d, a)
          , _runnable  = M.fromList . toList $ e
          , _backtrack = M.fromList $ map (\i' -> (i', False)) i
          }
        allThreads' = allThreads `S.union` S.fromList (M.keys $ _runnable this)
        bs' = doBacktrack (null ts) crstate' allThreads' (toList e) (bs++[this])
    in go crstate' allThreads' tid' bs' (Sq.viewl is) ts
  go _ _ _ bs _ _ = bs

  doBacktrack isEnd crstate allThreads enabledThreads bs =
    let tagged = reverse $ zip [0..] bs
        idxs   = [ (head is, u)
                 | (u, n) <- enabledThreads
                 , v <- S.toList allThreads
                 , u /= v
                 , let is = [ i
                            | (i, b) <- tagged
                            , _threadid b == v

                            -- There is a dependency between the actions
                            , let isDependent = dependent' memtype crstate (_threadid b, snd $ _decision b) (u, n)
                            -- This is the final action and threads will be killed
                            , let killsEarly  = isEnd && any (/=0) (M.keys $ _runnable b)

                            , killsEarly || isDependent
                            ]
                 , not $ null is] :: [(Int, ThreadId)]
    in foldl' (\b (i, u) -> backtrack b i u) bs idxs

-- | Add a new trace to the tree, creating a new subtree.
grow :: MemType -> Bool -> Trace' -> BPOR -> BPOR
grow memtype conservative = grow' initialCVState initialCRState 0 where
  grow' cvstate crstate tid trc@((d, _, a):rest) bpor =
    let tid'     = tidOf tid d
        cvstate' = updateCVState cvstate a
        crstate' = updateCRState crstate a
    in  case M.lookup tid' $ _bdone bpor of
          Just bpor' -> bpor { _bdone  = M.insert tid' (grow' cvstate' crstate' tid' rest bpor') $ _bdone bpor }
          Nothing    -> bpor { _btaken = if conservative then _btaken bpor else M.insert tid' a $ _btaken bpor
                            , _btodo  = M.delete tid' $ _btodo bpor
                            , _bdone  = M.insert tid' (subtree cvstate' crstate' tid' (_bsleep bpor `M.union` _btaken bpor) trc) $ _bdone bpor }
  grow' _ _ _ [] bpor = bpor

  subtree cvstate crstate tid sleep ((d, ts, a):rest) =
    let cvstate' = updateCVState cvstate a
        crstate' = updateCRState crstate a
        sleep'   = M.filterWithKey (\t a' -> not $ dependent memtype crstate' (tid, a) (t,a')) sleep
    in BPOR
        { _brunnable = S.fromList $ tids tid d a ts
        , _btodo     = M.empty
        , _bignore   = S.fromList [tidOf tid d' | (d',as) <- ts, willBlockSafely cvstate' $ toList as]
        , _bdone     = M.fromList $ case rest of
          ((d', _, _):_) ->
            let tid' = tidOf tid d'
            in  [(tid', subtree cvstate' crstate' tid' sleep' rest)]
          [] -> []
        , _bsleep = sleep'
        , _btaken = case rest of
          ((d', _, a'):_) -> M.singleton (tidOf tid d') a'
          [] -> M.empty
        , _baction = Just a
        }
  subtree _ _ _ _ [] = error "Invariant failure in 'subtree': suffix empty!"

  tids tid d (Fork t)           ts = tidOf tid d : t : map (tidOf tid . fst) ts
  tids tid _ (BlockedPutVar _)  ts = map (tidOf tid . fst) ts
  tids tid _ (BlockedReadVar _) ts = map (tidOf tid . fst) ts
  tids tid _ (BlockedTakeVar _) ts = map (tidOf tid . fst) ts
  tids tid _ BlockedSTM         ts = map (tidOf tid . fst) ts
  tids tid _ (BlockedThrowTo _) ts = map (tidOf tid . fst) ts
  tids tid _ Stop               ts = map (tidOf tid . fst) ts
  tids tid d _ ts = tidOf tid d : map (tidOf tid . fst) ts

-- | Add new backtracking points, if they have not already been
-- visited, fit into the bound, and aren't in the sleep set.
todo :: ([(Decision, ThreadAction)] -> (Decision, Lookahead) -> Bool) -> [BacktrackStep] -> BPOR -> BPOR
todo bv = step where
  step bs bpor =
    let (bpor', bs') = go 0 [] Nothing bs bpor
    in  if all (M.null . _backtrack) bs'
        then bpor'
        else step bs' bpor'

  go tid pref lastb (b:bs) bpor =
    let (bpor', blocked) = backtrack pref b bpor
        tid'   = tidOf tid . fst $ _decision b
        pref'  = pref ++ [_decision b]
        (child, blocked')  = go tid' pref' (Just b) bs . fromJust $ M.lookup tid' (_bdone bpor)
        bpor'' = bpor' { _bdone = M.insert tid' child $ _bdone bpor' }
    in  case lastb of
         Just b' -> (bpor'', b' { _backtrack = blocked } : blocked')
         Nothing -> (bpor'', blocked')

  go _ _ (Just b') _ bpor = (bpor, [b' { _backtrack = M.empty }])
  go _ _ Nothing   _ bpor = (bpor, [])

  backtrack pref b bpor =
    let todo' = [ x
                | x@(t,c) <- M.toList $ _backtrack b
                , let decision  = decisionOf (Just . activeTid $ map fst pref) (_brunnable bpor) t
                , let lookahead = fromJust . M.lookup t $ _runnable b
                , bv pref (decision, lookahead)
                , t `notElem` M.keys (_bdone bpor)
                , c || M.notMember t (_bsleep bpor)
                ]
        (blocked, nxt) = partition (\(t,_) -> t `S.member` _bignore bpor) todo'
    in  (bpor { _btodo = _btodo bpor `M.union` M.fromList nxt }, M.fromList blocked)

-- | Remove commits from the todo sets where every other action will
-- result in a write barrier (and so a commit) occurring.
--
-- To get the benefit from this, do not execute commit actions from
-- the todo set until there are no other choises.
pruneCommits :: BPOR -> BPOR
pruneCommits bpor
  | not onlycommits || not alldonesync = go bpor
  | otherwise = go bpor { _btodo = M.empty, _bdone = pruneCommits <$> _bdone bpor }

  where
    go b = b { _bdone = pruneCommits <$> _bdone bpor }

    onlycommits = all (<0) . M.keys $ _btodo bpor
    alldonesync = all barrier . M.elems $ _bdone bpor

    barrier = isBarrier . simplify . fromJust . _baction

-- * Utilities

-- | Get the resultant 'ThreadId' of a 'Decision', with a default case
-- for 'Continue'.
tidOf :: ThreadId -> Decision -> ThreadId
tidOf _ (Start t)    = t
tidOf _ (SwitchTo t) = t
tidOf tid _          = tid

-- | Get the 'Decision' that would have resulted in this 'ThreadId',
-- given a prior 'ThreadId' (if any) and list of runnable threads.
decisionOf :: Maybe ThreadId -> Set ThreadId -> ThreadId -> Decision
decisionOf prior runnable chosen
  | prior == Just chosen = Continue
  | prior `S.member` S.map Just runnable = SwitchTo chosen
  | otherwise = Start chosen

-- | Get the tid of the currently active thread after executing a
-- series of decisions. The list MUST begin with a 'Start'.
activeTid :: [Decision] -> ThreadId
activeTid = foldl' tidOf 0

-- | Check if an action is dependent on another.
dependent :: MemType -> CRState -> (ThreadId, ThreadAction) -> (ThreadId, ThreadAction) -> Bool
dependent _ _ (_, Lift) (_, Lift) = True
dependent _ _ (_, ThrowTo t) (t2, _) = t == t2
dependent _ _ (t2, _) (_, ThrowTo t) = t == t2
dependent _ _ (_, STM _) (_, STM _) = True
dependent memtype buf (_, d1) (_, d2) = dependentActions memtype buf (simplify d1) (simplify d2)

-- | Variant of 'dependent' to handle 'ThreadAction''s
dependent' :: MemType -> CRState -> (ThreadId, ThreadAction) -> (ThreadId, Lookahead) -> Bool
dependent' _ _ (_, Lift) (_, WillLift) = True
dependent' _ _ (_, ThrowTo t) (t2, _)     = t == t2
dependent' _ _ (t2, _) (_, WillThrowTo t) = t == t2
dependent' _ _ (_, STM _) (_, WillSTM) = True
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
  (UnsynchronisedWrite r1, UnsynchronisedWrite r2) -> r1 == r2

  -- Unsynchronised reads where a memory barrier would flush a
  -- buffered write
  (UnsynchronisedRead r1, _) | isBarrier a2 -> isBuffered buf r1 && memtype /= SequentialConsistency

  (_, _)
    -- Two actions on the same CRef where at least one is synchronised
    | same crefOf && (synchronises a1 (fromJust $ crefOf a1) || synchronises a2 (fromJust $ crefOf a2)) -> True
    -- Two actions on the same CVar
    | same cvarOf -> True

  _ -> False

  where
    same f = isJust (f a1) && f a1 == f a2

-- * Keeping track of 'CVar' full/empty states

type CVState = Map CVarId Bool

-- | Initial global 'CVar' state
initialCVState :: CVState
initialCVState = M.empty

-- | Update the 'CVar' state with the action that has just happened.
updateCVState :: CVState -> ThreadAction -> CVState
updateCVState cvstate (PutVar  c _) = M.insert c True  cvstate
updateCVState cvstate (TakeVar c _) = M.insert c False cvstate
updateCVState cvstate (TryPutVar  c True _) = M.insert c True  cvstate
updateCVState cvstate (TryTakeVar c True _) = M.insert c False cvstate
updateCVState cvstate _ = cvstate

-- | Check if an action will block.
willBlock :: CVState -> Lookahead -> Bool
willBlock cvstate (WillPutVar  c) = M.lookup c cvstate == Just True
willBlock cvstate (WillTakeVar c) = M.lookup c cvstate == Just False
willBlock cvstate (WillReadVar c) = M.lookup c cvstate == Just False
willBlock _ _ = False

-- | Check if a list of actions will block safely (without modifying
-- any global state). This allows further lookahead at, say, the
-- 'spawn' of a thread (which always starts with 'KnowsAbout').
willBlockSafely :: CVState -> [Lookahead] -> Bool
willBlockSafely cvstate (WillMyThreadId:as) = willBlockSafely cvstate as
willBlockSafely cvstate (WillNewVar:as)     = willBlockSafely cvstate as
willBlockSafely cvstate (WillNewRef:as)     = willBlockSafely cvstate as
willBlockSafely cvstate (WillReturn:as)     = willBlockSafely cvstate as
willBlockSafely cvstate (WillKnowsAbout:as) = willBlockSafely cvstate as
willBlockSafely cvstate (WillForgets:as)    = willBlockSafely cvstate as
willBlockSafely cvstate (WillAllKnown:as)   = willBlockSafely cvstate as
willBlockSafely cvstate (WillPutVar  c:_) = willBlock cvstate (WillPutVar  c)
willBlockSafely cvstate (WillTakeVar c:_) = willBlock cvstate (WillTakeVar c)
willBlockSafely _ _ = False

-- * Keeping track of 'CRef' buffer state

type CRState = Map CRefId Bool

-- | Initial global 'CRef buffer state.
initialCRState :: CRState
initialCRState = M.empty

-- | Update the 'CRef' buffer state with the action that has just
-- happened.
updateCRState :: CRState -> ThreadAction -> CRState
updateCRState crstate (CommitRef _ r) = M.delete r crstate
updateCRState crstate (WriteRef r) = M.insert r True crstate
updateCRState crstate ta
  | isBarrier $ simplify ta = initialCRState
  | otherwise = crstate

-- | Check if a 'CRef' has a buffered write pending.
isBuffered :: CRState -> CRefId -> Bool
isBuffered crefid r = M.findWithDefault False r crefid

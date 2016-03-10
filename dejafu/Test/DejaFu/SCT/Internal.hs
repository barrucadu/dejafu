-- | Internal utilities and types for BPOR.
module Test.DejaFu.SCT.Internal where

import Control.DeepSeq (NFData(..))
import Data.List (foldl', partition, sortBy, intercalate)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe, isJust, fromJust, listToMaybe)
import Data.Ord (Down(..), comparing)
import Data.Sequence (Seq, ViewL(..))
import Data.Set (Set)
import Test.DejaFu.Deterministic.Internal
import Test.DejaFu.Deterministic.Schedule

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Sq
import qualified Data.Set as S

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
  , _crstate :: CRState
  -- ^ The relaxed memory state of the @CRef@s at this point.
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

-- | Render a 'BPOR' value as a graph in GraphViz \"dot\" format.
toDot :: BPOR -> String
toDot = toDotFilter (\_ _ -> True)

-- | Variant of 'toDot' which doesn't include aborted subtrees.
toDotSmall :: BPOR -> String
toDotSmall = toDotFilter (curry check) where
  -- Check that a subtree has at least one non-aborted branch.
  check (i, b) = (i == initialThread && _baction b == Just Stop) || any check (M.toList $ _bdone b)

-- | Render a 'BPOR' value as a graph in GraphViz \"dot\" format, with
-- a function to determine if a subtree should be included or not.
toDotFilter :: (ThreadId -> BPOR -> Bool) -> BPOR -> String
toDotFilter check bpor = "digraph {\n" ++ go "L" bpor ++ "\n}" where
  go l b = unlines $ node l b : [edge l l' i ++ go l' b' | (i, b') <- M.toList (_bdone b), check i b', let l' = l ++ show' i]

  -- Display a labelled node.
  node n b = n ++ " [label=\"" ++ label b ++ "\"]"

  -- A node label, summary of the BPOR state at that node.
  label b = intercalate ","
    [ show $ _baction b
    , "Run:" ++ show (S.toList $ _brunnable b)
    , "Tod:" ++ show (M.keys   $ _btodo     b)
    , "Slp:" ++ show (M.toList $ _bsleep    b)
    ]

  -- Display a labelled edge
  --
  -- TODO: Incorporate the thread name.
  edge n1 n2 l = n1 ++ "-> " ++ n2 ++ " [label=\"" ++ show l ++ "\"]\n"

  -- Show a 'ThreadId', replacing a minus sign for \"N\".
  show' (ThreadId _ i) = map (\c -> if c == '-' then 'N' else c) $ show i

-- | Initial BPOR state.
initialState :: BPOR
initialState = BPOR
  { _brunnable = S.singleton initialThread
  , _btodo     = M.singleton initialThread False
  , _bdone     = M.empty
  , _bsleep    = M.empty
  , _btaken    = M.empty
  , _baction   = Nothing
  }

-- | Produce a new schedule from a BPOR tree. If there are no new
-- schedules remaining, return 'Nothing'. Also returns whether the
-- decision was added conservatively, and the sleep set at the point
-- where divergence happens.
--
-- This returns the longest prefix, on the assumption that this will
-- lead to lots of backtracking points being identified before
-- higher-up decisions are reconsidered, so enlarging the sleep sets.
next :: BPOR -> Maybe ([ThreadId], Bool, Map ThreadId ThreadAction)
next = go initialThread where
  go tid bpor =
        -- All the possible prefix traces from this point, with
        -- updated BPOR subtrees if taken from the done list.
    let prefixes = mapMaybe go' (M.toList $ _bdone bpor) ++ [([t], c, sleeps bpor) | (t, c) <- M.toList $ _btodo bpor]
        -- Sort by number of preemptions, in descending order.
        cmp = preEmps tid bpor . (\(a,_,_) -> a)

    in if null prefixes
       then Nothing
       else case partition (\(t:_,_,_) -> t < initialThread) $ sortBy (comparing $ Down . cmp) prefixes of
              (commits, others)
                | not $ null others  -> listToMaybe others
                | not $ null commits -> listToMaybe commits
                | otherwise -> error "Invariant failure in 'next': empty prefix list!"

  go' (tid, bpor) = (\(ts,c,slp) -> (tid:ts,c,slp)) <$> go tid bpor

  sleeps bpor = _bsleep bpor `M.union` _btaken bpor

  preEmps tid bpor (t:ts) =
    let rest = preEmps t (fromJust . M.lookup t $ _bdone bpor) ts
    in  if t > initialThread && tid /= t && tid `S.member` _brunnable bpor then 1 + rest else rest
  preEmps _ _ [] = 0::Int

-- | Produce a list of new backtracking points from an execution
-- trace.
findBacktrack :: MemType
  -> ([BacktrackStep] -> Int -> ThreadId -> [BacktrackStep])
  -> Seq (NonEmpty (ThreadId, Lookahead), [ThreadId])
  -> Trace'
  -> [BacktrackStep]
findBacktrack memtype backtrack = go initialCRState S.empty initialThread [] . Sq.viewl where
  go crstate allThreads tid bs ((e,i):<is) ((d,_,a):ts) =
    let tid' = tidOf tid d
        crstate' = updateCRState crstate a
        this = BacktrackStep
          { _threadid  = tid'
          , _decision  = (d, a)
          , _runnable  = M.fromList . toList $ e
          , _backtrack = M.fromList $ map (\i' -> (i', False)) i
          , _crstate   = crstate'
          }
        bs' = doBacktrack killsEarly allThreads' (toList e) (bs++[this])
        allThreads' = allThreads `S.union` S.fromList (M.keys $ _runnable this)
        killsEarly = null ts && any (/=initialThread) (M.keys $ _runnable this)
    in go crstate' allThreads' tid' bs' (Sq.viewl is) ts
  go _ _ _ bs _ _ = bs

  doBacktrack killsEarly allThreads enabledThreads bs =
    let tagged = reverse $ zip [0..] bs
        idxs   = [ (head is, u)
                 | (u, n) <- enabledThreads
                 , v <- S.toList allThreads
                 , u /= v
                 , let is = idxs' u n v tagged
                 , not $ null is]

        idxs' u n v = mapMaybe go' where
          go' (i, b)
            | _threadid b == v && (killsEarly || isDependent b) = Just i
            | otherwise = Nothing

          isDependent b = dependent' memtype (_crstate b) (_threadid b, snd $ _decision b) (u, n)
    in foldl' (\b (i, u) -> backtrack b i u) bs idxs

-- | Add a new trace to the tree, creating a new subtree.
grow :: MemType -> Bool -> Trace' -> BPOR -> BPOR
grow memtype conservative = grow' initialCRState initialThread where
  grow' crstate tid trc@((d, _, a):rest) bpor =
    let tid'     = tidOf tid d
        crstate' = updateCRState crstate a
    in  case M.lookup tid' $ _bdone bpor of
          Just bpor' -> bpor { _bdone  = M.insert tid' (grow' crstate' tid' rest bpor') $ _bdone bpor }
          Nothing    -> bpor { _btaken = if conservative then _btaken bpor else M.insert tid' a $ _btaken bpor
                            , _btodo  = M.delete tid' $ _btodo bpor
                            , _bdone  = M.insert tid' (subtree crstate' tid' (_bsleep bpor `M.union` _btaken bpor) trc) $ _bdone bpor }
  grow' _ _ [] bpor = bpor

  subtree crstate tid sleep ((d, ts, a):rest) =
    let crstate' = updateCRState crstate a
        sleep'   = M.filterWithKey (\t a' -> not $ dependent memtype crstate' (tid, a) (t,a')) sleep
    in BPOR
        { _brunnable = S.fromList $ tids tid d a ts
        , _btodo     = M.empty
        , _bdone     = M.fromList $ case rest of
          ((d', _, _):_) ->
            let tid' = tidOf tid d'
            in  [(tid', subtree crstate' tid' sleep' rest)]
          [] -> []
        , _bsleep = sleep'
        , _btaken = case rest of
          ((d', _, a'):_) -> M.singleton (tidOf tid d') a'
          [] -> M.empty
        , _baction = Just a
        }
  subtree _ _ _ [] = error "Invariant failure in 'subtree': suffix empty!"

  tids tid d (Fork t)           ts = tidOf tid d : t : map (tidOf tid . fst) ts
  tids tid _ (BlockedPutVar _)  ts = map (tidOf tid . fst) ts
  tids tid _ (BlockedReadVar _) ts = map (tidOf tid . fst) ts
  tids tid _ (BlockedTakeVar _) ts = map (tidOf tid . fst) ts
  tids tid _ (BlockedSTM _)     ts = map (tidOf tid . fst) ts
  tids tid _ (BlockedThrowTo _) ts = map (tidOf tid . fst) ts
  tids tid _ Stop               ts = map (tidOf tid . fst) ts
  tids tid d _ ts = tidOf tid d : map (tidOf tid . fst) ts

-- | Add new backtracking points, if they have not already been
-- visited, fit into the bound, and aren't in the sleep set.
todo :: ([(Decision, ThreadAction)] -> (Decision, Lookahead) -> Bool) -> [BacktrackStep] -> BPOR -> BPOR
todo bv = go initialThread [] where
  go tid pref (b:bs) bpor =
    let bpor' = backtrack pref b bpor
        tid'  = tidOf tid . fst $ _decision b
        pref' = pref ++ [_decision b]
        child = go tid' pref' bs . fromJust $ M.lookup tid' (_bdone bpor)
    in bpor' { _bdone = M.insert tid' child $ _bdone bpor' }

  go _ _ [] bpor = bpor

  backtrack pref b bpor =
    let todo' = [ x
                | x@(t,c) <- M.toList $ _backtrack b
                , let decision  = decisionOf (Just . activeTid $ map fst pref) (_brunnable bpor) t
                , let lahead = fromJust . M.lookup t $ _runnable b
                , bv pref (decision, lahead)
                , t `notElem` M.keys (_bdone bpor)
                , c || M.notMember t (_bsleep bpor)
                ]
    in bpor { _btodo = _btodo bpor `M.union` M.fromList todo' }

-- | Remove commits from the todo sets where every other action will
-- result in a write barrier (and so a commit) occurring.
--
-- To get the benefit from this, do not execute commit actions from
-- the todo set until there are no other choises.
pruneCommits :: BPOR -> BPOR
pruneCommits bpor
  | not onlycommits || not alldonesync = go bpor
  | otherwise = go bpor { _btodo = M.empty }

  where
    go b = b { _bdone = pruneCommits <$> _bdone bpor }

    onlycommits = all (<initialThread) . M.keys $ _btodo bpor
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
activeTid = foldl' tidOf initialThread

-- | Check if an action is dependent on another.
dependent :: MemType -> CRState -> (ThreadId, ThreadAction) -> (ThreadId, ThreadAction) -> Bool
dependent _ _ (_, Lift) (_, Lift) = True
dependent _ _ (_, ThrowTo t) (t2, a) = t == t2 && a /= Stop
dependent _ _ (t2, a) (_, ThrowTo t) = t == t2 && a /= Stop
dependent _ _ (_, STM _ _) (_, STM _ _) = True
dependent _ _ (_, GetNumCapabilities a) (_, SetNumCapabilities b) = a /= b
dependent _ _ (_, SetNumCapabilities a) (_, GetNumCapabilities b) = a /= b
dependent _ _ (_, SetNumCapabilities a) (_, SetNumCapabilities b) = a /= b
dependent memtype buf (_, d1) (_, d2) = dependentActions memtype buf (simplify d1) (simplify d2)

-- | Variant of 'dependent' to handle 'ThreadAction''s
dependent' :: MemType -> CRState -> (ThreadId, ThreadAction) -> (ThreadId, Lookahead) -> Bool
dependent' _ _ (_, Lift) (_, WillLift) = True
dependent' _ _ (_, ThrowTo t) (t2, a)     = t == t2 && a /= WillStop
dependent' _ _ (t2, a) (_, WillThrowTo t) = t == t2 && a /= Stop
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

  -- Unsynchronised reads where a memory barrier would flush a
  -- buffered write
  (UnsynchronisedRead r1, _) | isBarrier a2 -> isBuffered buf r1 && memtype /= SequentialConsistency
  (_, UnsynchronisedRead r2) | isBarrier a1 -> isBuffered buf r2 && memtype /= SequentialConsistency

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

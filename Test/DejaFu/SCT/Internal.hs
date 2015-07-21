-- | Internal utilities and types for BPOR.
module Test.DejaFu.SCT.Internal where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData(..))
import Data.List (foldl', partition, sortBy)
import Data.Ord (Down(..), comparing)
import Data.Ord.Extra
import Data.Map (Map)
import Data.Maybe (mapMaybe, fromJust)
import Data.Set (Set)
import Test.DejaFu.Deterministic

import qualified Data.Map as M
import qualified Data.Set as S

-- * BPOR state

-- | One step of the execution, including information for backtracking
-- purposes. This backtracking information is used to generate new
-- schedules.
data BacktrackStep = BacktrackStep
  { _decision  :: (Decision, ThreadAction)
  -- ^ What happened at this step.
  , _runnable  :: Set ThreadId
  -- ^ The threads runnable at this step
  , _backtrack :: Set (First ThreadId Bool)
  -- ^ The list of alternative threads to run, and whether those
  -- alternatives were added conservatively due to the bound.
  } deriving (Eq, Show)

instance NFData BacktrackStep where
  rnf b = rnf (_decision b, _runnable b, _backtrack b)

-- | BPOR execution is represented as a tree of states, characterised
-- by the decisions that lead to that state.
data BPOR = BPOR
  { _brunnable :: Set ThreadId
  -- ^ What threads are runnable at this step.
  , _btodo     :: Set (First ThreadId Bool)
  -- ^ Follow-on decisions still to make, and whether that decision
  -- was added conservatively due to the bound.
  , _bignore :: Set ThreadId
  -- ^ Follow-on decisions never to make, because they will result in
  -- the chosen thread immediately blocking without achieving
  -- anything, which can't have any effect on the result of the
  -- program.
  , _bdone     :: Map ThreadId BPOR
  -- ^ Follow-on decisions that have been made.
  , _bsleep    :: Set (First ThreadId ThreadAction)
  -- ^ Transitions to ignore (in this node and children) until a
  -- dependent transition happens.
  , _btaken    :: Set (First ThreadId ThreadAction)
  -- ^ Transitions which have been taken, excluding
  -- conservatively-added ones, in the (reverse) order that they were
  -- taken, as the 'Map' doesn't preserve insertion order. This is
  -- used in implementing sleep sets.
  }

-- | Initial BPOR state.
initialState :: BPOR
initialState = BPOR
  { _brunnable = S.singleton 0
  , _btodo     = S.singleton $ First (0, False)
  , _bignore   = S.empty
  , _bdone     = M.empty
  , _bsleep    = S.empty
  , _btaken    = S.empty
  }

-- | Produce a new schedule from a BPOR tree. If there are no new
-- schedules remaining, return 'Nothing'. Also returns whether the
-- decision made was added conservatively.
--
-- This returns the longest prefix, on the assumption that this will
-- lead to lots of backtracking points being identified before
-- higher-up decisions are reconsidered, so enlarging the sleep sets.
next :: BPOR -> Maybe ([ThreadId], Bool, BPOR)
next = go 0 where
  go tid bpor =
        -- All the possible prefix traces from this point, with
        -- updated BPOR subtrees if taken from the done list.
    let prefixes = [Left t | t <- S.toList $ _btodo bpor] ++ mapMaybe go' (M.toList $ _bdone bpor)
        -- Sort by number of preemptions, in descending order.
        sorted   = sortBy (comparing $ Down . preEmps tid bpor . either (\f -> [fst $ unFirst f]) (\(a,_,_) -> a)) prefixes

    in case sorted of
         -- If the prefix with the most preemptions is from the done list, update that.
         (Right (ts@(t:_), c, b):_) -> Just (ts, c, bpor { _bdone = M.insert t b $ _bdone bpor })
         -- If from the todo list, remove it.
         (Left f:_) -> Just ([fst $ unFirst f], snd $ unFirst f, bpor { _btodo = S.filter (/=f) $ _btodo bpor })

         _ -> Nothing

  go' (tid, bpor) = (\(ts,c,b) -> Right (tid:ts, c, b)) <$> go tid bpor

  preEmps tid bpor (t:ts) =
    let rest = preEmps t (fromJust . M.lookup t $ _bdone bpor) ts
    in  if tid /= t && tid `S.member` _brunnable bpor then 1 + rest else rest
  preEmps _ _ [] = 0::Int

-- | Produce a list of new backtracking points from an execution
-- trace.
findBacktrack :: ([BacktrackStep] -> Int -> ThreadId -> [BacktrackStep])
              -> [(NonEmpty (ThreadId, ThreadAction'), [ThreadId])]
              -> Trace'
              -> [BacktrackStep]
findBacktrack backtrack = go S.empty [] where
  go allThreads bs ((e,i):is) ((d,_,a):ts) =
    let this        = BacktrackStep { _decision = (d, a)
                                    , _runnable = S.fromList . map fst . toList $ e
                                    , _backtrack = S.fromList $ map (\i' -> First (i', False)) i
                                    }
        bs'         = doBacktrack allThreads (toList e) bs
        allThreads' = allThreads `S.union` _runnable this
    in go allThreads' (bs' ++ [this]) is ts
  go _ bs _ _ = bs

  doBacktrack allThreads enabledThreads bs =
    let tagged = reverse . zip [0..] $ tidTag (fst . _decision) 0 bs
        idxs   = [ (head is, u)
                 | (u, n) <- enabledThreads
                 , v <- S.toList allThreads
                 , u /= v
                 , let is = [ i
                            | (i, (t, b)) <- tagged
                            , t == v
                            , dependent' (snd $ _decision b) (u, n)
                            ]
                 , not $ null is] :: [(Int, ThreadId)]
    in foldl' (\bs (i, u) -> backtrack bs i u) bs idxs

-- | Add a new trace to the tree, creating a new subtree.
grow :: Bool -> Trace' -> BPOR -> BPOR
grow conservative = grow' initialCVState 0 where
  grow' cvstate tid trc@((d, _, a):rest) bpor =
    let tid'     = tidOf tid d
        cvstate' = updateCVState cvstate a
    in  case M.lookup tid' $ _bdone bpor of
          Just bpor' -> bpor { _bdone  = M.insert tid' (grow' cvstate' tid' rest bpor') $ _bdone bpor }
          Nothing    -> bpor { _btaken = if conservative then _btaken bpor else First (tid', a) `S.insert` _btaken bpor
                            , _bdone  = M.insert tid' (subtree cvstate' tid' (_bsleep bpor `S.union` _btaken bpor) trc) $ _bdone bpor }
  grow' _ _ [] bpor = bpor

  subtree cvstate tid sleep ((d, ts, a):rest) =
    let cvstate' = updateCVState cvstate a
        sleep'   = S.filter (not . dependent a . unFirst) sleep
    in BPOR
        { _brunnable = S.fromList $ tids tid d a ts
        , _btodo     = S.empty
        , _bignore   = S.fromList [tidOf tid d | (d,as) <- ts, willBlockSafely cvstate' $ toList as]
        , _bdone     = M.fromList $ case rest of
          ((d', _, _):_) ->
            let tid' = tidOf tid d'
            in  [(tid', subtree cvstate' tid' sleep' rest)]
          [] -> []
        , _bsleep = sleep'
        , _btaken = case rest of
          ((d', _, a'):_) -> S.singleton $ First (tidOf tid d', a')
          [] -> S.empty
        }

  tids tid d (Fork t)           ts = tidOf tid d : t : map (tidOf tid . fst) ts
  tids tid _ (BlockedPut _)     ts = map (tidOf tid . fst) ts
  tids tid _ (BlockedRead _)    ts = map (tidOf tid . fst) ts
  tids tid _ (BlockedTake _)    ts = map (tidOf tid . fst) ts
  tids tid _ BlockedSTM         ts = map (tidOf tid . fst) ts
  tids tid _ (BlockedThrowTo _) ts = map (tidOf tid . fst) ts
  tids tid _ Stop               ts = map (tidOf tid . fst) ts
  tids tid d _ ts = tidOf tid d : map (tidOf tid . fst) ts

-- | Add new backtracking points, if they have not already been
-- visited, fit into the bound, and aren't in the sleep set.
todo :: ([Decision] -> Bool) -> [BacktrackStep] -> BPOR -> BPOR
todo bv = step where
  step bs bpor =
    let (bpor', bs') = go 0 [] Nothing bs bpor
    in  if all (S.null . _backtrack) bs'
        then bpor'
        else step bs' bpor'

  go tid pref lastb (b:bs) bpor =
    let (bpor', blocked) = backtrack pref b bpor
        tid'   = tidOf tid . fst $ _decision b
        (child, blocked')  = go tid' (pref++[fst $ _decision b]) (Just b) bs . fromJust $ M.lookup tid' (_bdone bpor)
        bpor'' = bpor' { _bdone = M.insert tid' child $ _bdone bpor' }
    in  case lastb of
         Just b' -> (bpor'', b' { _backtrack = blocked } : blocked')
         Nothing -> (bpor'', blocked')

  go _ _ (Just b') _ bpor = (bpor, [b' { _backtrack = S.empty }])
  go _ _ Nothing   _ bpor = (bpor, [])

  backtrack pref b bpor =
    let todo' = [ x
                | x@(First (t,c)) <- S.toList $ _backtrack b
                , bv $ pref ++ [decisionOf (Just $ activeTid pref) (_brunnable bpor) t]
                , t `notElem` M.keys (_bdone bpor)
                , c || t `S.notMember` S.map (fst . unFirst) (_bsleep bpor)
                ]
        (blocked, next) = partition (\(First (t,_)) -> t `S.member` _bignore bpor) todo'
    in  (bpor { _btodo = _btodo bpor `S.union` S.fromList next }, S.fromList blocked)

-- * Utilities

-- | Get the resultant 'ThreadId' of a 'Decision', with a default case
-- for 'Continue'.
tidOf :: ThreadId -> Decision -> ThreadId
tidOf _ (Start t)    = t
tidOf _ (SwitchTo t) = t
tidOf tid Continue   = tid

-- | Tag a list of items encapsulating 'Decision's with 'ThreadId's,
-- with an initial default case for 'Continue'.
tidTag :: (a -> Decision) -> ThreadId -> [a] -> [(ThreadId, a)]
tidTag df = go where
  go t (a:as) =
    let t' = tidOf t $ df a
    in (t', a) : go t' as
  go _ [] = []

-- | Get the 'Decision' that would have resulted in this 'ThreadId',
-- given a prior 'ThreadId' (if any) and list of runnable threds.
decisionOf :: Maybe ThreadId -> Set ThreadId -> ThreadId -> Decision
decisionOf prior runnable chosen
  | prior == Just chosen = Continue
  | prior `S.member` S.map Just runnable = SwitchTo chosen
  | otherwise = Start chosen

-- | Get the tid of the currently active thread after executing a
-- series of decisions. The list MUST begin with a 'Start'.
activeTid :: [Decision] -> ThreadId
activeTid = foldl' go 0 where
  go _ (Start t)    = t
  go _ (SwitchTo t) = t
  go t Continue     = t

-- | Count the number of preemptions in a schedule
preEmpCount :: [Decision] -> Int
preEmpCount (SwitchTo _:ds) = 1 + preEmpCount ds
preEmpCount (_:ds) = preEmpCount ds
preEmpCount [] = 0

-- | Check if an action is dependent on another, assumes the actions
-- are from different threads (two actions in the same thread are
-- always dependent).
dependent :: ThreadAction -> (ThreadId, ThreadAction) -> Bool
dependent Lift (_, Lift) = True
dependent (ThrowTo t) (t2, _) = t == t2
dependent d1 (_, d2) = cref || cvar || ctvar where
  cref = Just True == ((\(r1, w1) (r2, w2) -> r1 == r2 && (w1 || w2)) <$> cref' d1 <*> cref' d2)
  cref'  (ReadRef  r) = Just (r, False)
  cref'  (ModRef   r) = Just (r, True)
  cref'  _ = Nothing

  cvar = Just True == ((==) <$> cvar' d1 <*> cvar' d2)
  cvar'  (TryPut  c _ _) = Just c
  cvar'  (TryTake c _ _) = Just c
  cvar'  (Put  c _) = Just c
  cvar'  (Read c)   = Just c
  cvar'  (Take c _) = Just c
  cvar'  _ = Nothing

  ctvar = ctvar' d1 && ctvar' d2
  ctvar' (STM _) = True
  ctvar' _ = False

-- | Variant of 'dependent' to handle 'ThreadAction''s
dependent' :: ThreadAction -> (ThreadId, ThreadAction') -> Bool
dependent' Lift (_, Lift') = True
dependent' (ThrowTo t) (t2, _) = t == t2
dependent' d1 (_, d2) = cref || cvar || ctvar where
  cref = Just True == ((\(r1, w1) (r2, w2) -> r1 == r2 && (w1 || w2)) <$> cref' d1 <*> cref'' d2)
  cref'  (ReadRef  r) = Just (r, False)
  cref'  (ModRef   r) = Just (r, True)
  cref'  _ = Nothing
  cref'' (ReadRef' r) = Just (r, False)
  cref'' (ModRef'  r) = Just (r, True)
  cref'' _ = Nothing

  cvar = Just True == ((==) <$> cvar' d1 <*> cvar'' d2)
  cvar'  (TryPut  c _ _) = Just c
  cvar'  (TryTake c _ _) = Just c
  cvar'  (Put  c _) = Just c
  cvar'  (Read c)   = Just c
  cvar'  (Take c _) = Just c
  cvar'  _ = Nothing
  cvar'' (TryPut'  c) = Just c
  cvar'' (TryTake' c) = Just c
  cvar'' (Put'  c) = Just c
  cvar'' (Read' c) = Just c
  cvar'' (Take' c) = Just c
  cvar'' _ = Nothing

  ctvar = ctvar' d1 && ctvar'' d2
  ctvar' (STM _) = True
  ctvar' _ = False
  ctvar'' STM' = True
  ctvar'' _ = False

-- * Keeping track of 'CVar' full/empty states

-- | Initial global 'CVar' state
initialCVState :: [(CVarId, Bool)]
initialCVState = []

-- | Update the 'CVar' state with the action that has just happened.
updateCVState :: [(CVarId, Bool)] -> ThreadAction -> [(CVarId, Bool)]
updateCVState cvstate (Put  c _) = (c,True)  : filter (/=(c,False)) cvstate
updateCVState cvstate (Take c _) = (c,False) : filter (/=(c,True))  cvstate
updateCVState cvstate (TryPut  c True _) = (c,True)  : filter (/=(c,False)) cvstate
updateCVState cvstate (TryTake c True _) = (c,False) : filter (/=(c,True))  cvstate
updateCVState cvstate _ = cvstate

-- | Check if an action will block.
willBlock :: [(CVarId, Bool)] -> ThreadAction' -> Bool
willBlock cvstate (Put'  c) = (c, True)  `elem` cvstate
willBlock cvstate (Take' c) = (c, False) `elem` cvstate
willBlock _ _ = False

-- | Check if a list of actions will block safely (without modifying
-- any global state). This allows further lookahead at, say, the
-- 'spawn' of a thread (which always starts with 'KnowsAbout'.
willBlockSafely :: [(CVarId, Bool)] -> [ThreadAction'] -> Bool
willBlockSafely cvstate (KnowsAbout':as) = willBlockSafely cvstate as
willBlockSafely cvstate (Forgets':as)    = willBlockSafely cvstate as
willBlockSafely cvstate (AllKnown':as)   = willBlockSafely cvstate as
willBlockSafely cvstate (Put'  c:_) = willBlock cvstate (Put'  c)
willBlockSafely cvstate (Take' c:_) = willBlock cvstate (Take' c)
willBlockSafely _ _ = False

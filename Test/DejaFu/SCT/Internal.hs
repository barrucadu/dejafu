-- | Internal utilities and types for BPOR.
module Test.DejaFu.SCT.Internal where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Control.DeepSeq (NFData(..))
import Data.List (foldl', nub, sortBy)
import Data.Ord (Down(..), comparing)
import Data.Map (Map)
import Data.Maybe (mapMaybe, fromJust)
import Test.DejaFu.Deterministic

import qualified Data.Map as M

-- * BPOR state

-- | One step of the execution, including information for backtracking
-- purposes. This backtracking information is used to generate new
-- schedules.
data BacktrackStep = BacktrackStep
  { _decision  :: (Decision, ThreadAction)
  -- ^ What happened at this step.
  , _runnable  :: [ThreadId]
  -- ^ The threads runnable at this step
  , _backtrack :: [ThreadId]
  -- ^ The list of alternative threads to run.
  } deriving (Eq, Show)

instance NFData BacktrackStep where
  rnf b = rnf (_decision b, _runnable b, _backtrack b)

-- | BPOR execution is represented as a tree of states, characterised
-- by the decisions that lead to that state.
data BPOR = BPOR
  { _brunnable :: [ThreadId]
  -- ^ What threads are runnable at this step.
  , _btodo     :: [ThreadId]
  -- ^ Follow-on decisions still to make.
  , _bdone     :: Map ThreadId BPOR
  }

-- | Initial BPOR state.
initialState :: BPOR
initialState = BPOR
  { _brunnable = [0]
  , _btodo     = [0]
  , _bdone     = M.empty
  }

-- | Produce a new schedule from a BPOR tree. If there are no new
-- schedules remaining, return 'Nothing'.
--
-- This returns the prefix with the most preemptions in, on the
-- assumption that preemptions are likely to exhibit bugs, and so lead
-- to earlier test failures.
next :: BPOR -> Maybe ([ThreadId], BPOR)
next = go 0 where
  go tid bpor =
        -- All the possible prefix traces from this point, with
        -- updated BPOR subtrees if taken from the done list.
    let prefixes = [Left t | t <- _btodo bpor] ++ mapMaybe go' (M.toList $ _bdone bpor)
        -- Sort by number of preemptions, in descending order.
        sorted   = sortBy (comparing $ Down . preEmps tid bpor . either (:[]) fst) prefixes

    in case sorted of
         -- If the schedule with the most preemptions is from the done list, update that.
         (Right (ts@(t:_), b):_) -> Just (ts, bpor { _bdone = M.insert t b $ _bdone bpor })
         -- If from the todo list, remove it.
         (Left t:_) -> Just ([t],  bpor { _btodo = filter (/=t) $ _btodo bpor })

         _ -> Nothing

  go' (tid, bpor) = Right . first (tid:) <$> go tid bpor

  preEmps tid bpor (t:ts) =
    let rest = preEmps t (fromJust . M.lookup t $ _bdone bpor) ts
    in  if tid /= t && tid `elem` _brunnable bpor then 1 + rest else rest
  preEmps _ _ [] = 0::Int

-- | Produce a list of new backtracking points from an execution
-- trace.
findBacktrack :: Bool
              -> ([BacktrackStep] -> Int -> ThreadId -> [BacktrackStep])
              -> [(NonEmpty (ThreadId, ThreadAction'), [ThreadId])]
              -> Trace
              -> [BacktrackStep]
findBacktrack deplifts backtrack = go [] where
  go bs ((e,i):is) ((d,_,a):ts) =
    let this = BacktrackStep { _decision  = (d, a), _runnable = map fst . toList $ e, _backtrack = i }
        bs'  = doBacktrack (toList e) bs
    in go (bs' ++ [this]) is ts
  go bs _ _ = bs

  doBacktrack enabledThreads bs =
    let idxs = [ (maximum is, u)
               | (u, n) <- enabledThreads
               , v <- allThreads bs
               , u /= v
               , let is = [ i
                          | (i, (t, b)) <- zip [0..] $ tidTag (fst . _decision) 0 bs
                          , t == v
                          , dependent deplifts (snd $ _decision b) (u, n)
                          ]
               , not $ null is] :: [(Int, ThreadId)]
    in foldl' (\bs (i, u) -> backtrack bs i u) bs idxs

  allThreads = nub . concatMap _runnable

-- | Add a new trace to the tree, creating a new subtree.
grow :: Trace -> BPOR -> BPOR
grow = grow' 0 where
  grow' tid trc@((d, _, _):rest) bpor =
    let tid' = tidOf tid d
    in  case M.lookup tid' $ _bdone bpor of
          Just bpor' -> bpor { _bdone = M.insert tid' (grow' tid' rest bpor') $ _bdone bpor }
          Nothing    -> bpor { _bdone = M.insert tid' (subtree tid' trc)      $ _bdone bpor }
  grow' _ [] bpor = bpor

  subtree tid ((d, ts, a):rest) = BPOR
    { _brunnable = tids tid d a ts
    , _btodo     = []
    , _bdone     = M.fromList $ case rest of
      ((d', _, _):_) ->
        let tid' = tidOf tid d'
        in  [(tid', subtree tid' rest)]
      [] -> []
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
-- visited and fit into the bound.
todo :: ([Decision] -> Bool) -> [BacktrackStep] -> BPOR -> BPOR
todo bv = go 0 [] where
  go tid pref (b:bs) bpor =
    let bpor' = backtrack pref b bpor
        tid' = tidOf tid . fst $ _decision b
    in  bpor' { _bdone = M.adjust (go tid' (pref++[fst $ _decision b]) bs) tid' $ _bdone bpor' }
  go _ _ _ bpor = bpor

  backtrack pref b bpor =
    let todo' = nub $ _btodo bpor ++ [ t
                                     | t <- _backtrack b
                                     , bv $ pref ++ [decisionOf (Just $ activeTid pref) (_brunnable bpor) t]
                                     , t `notElem` M.keys (_bdone bpor)
                                     ]
    in  bpor { _btodo = todo' }

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
decisionOf :: Maybe ThreadId -> [ThreadId] -> ThreadId -> Decision
decisionOf prior runnable chosen
  | prior == Just chosen = Continue
  | prior `elem` map Just runnable = SwitchTo chosen
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
dependent :: Bool -> ThreadAction -> (ThreadId, ThreadAction') -> Bool
dependent deplifts Lift (_, Lift') = deplifts
dependent _ (ThrowTo t) (t2, _) = t == t2
dependent _ d1 (_, d2) = cref || cvar || ctvar where
  cref = Just True == ((\(r1, w1) (r2, w2) -> r1 == r2 && (w1 || w2)) <$> cref' d1 <*> cref'' d2)
  cref'  (ReadRef  r) = Just (r, False)
  cref'  (ModRef   r) = Just (r, True)
  cref'  _ = Nothing
  cref'' (ReadRef' r) = Just (r, False)
  cref'' (ModRef'  r) = Just (r, True)
  cref'' _ = Nothing

  cvar = Just True == ((==) <$> cvar' d1 <*> cvar'' d2)
  cvar'  (BlockedPut  _) = Nothing
  cvar'  (BlockedRead _) = Nothing
  cvar'  (BlockedTake _) = Nothing
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
  ctvar' (STM _)    = True
  ctvar' BlockedSTM = False
  ctvar' _ = False
  ctvar'' STM' = True
  ctvar'' _ = False

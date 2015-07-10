{-# LANGUAGE Rank2Types #-}

-- | Systematic testing for concurrent computations.
module Test.DejaFu.SCT
 ( -- * Schedule Bounding
 -- | Schedule bounding is a means of cutting down the search space of
 -- schedules, by taking advantage of some intrinsic properties of
 -- schedules: such as the number of pre-emptions (pre-emption
 -- bounding), or the number of deviations from a deterministic
 -- scheduler (delay bounding); and then exploring all schedules
 -- within the bound.
   sctBounded
 , sctPreBound
 , sctDelayBound
 , sctBoundedIO
 , sctPreBoundIO
 , sctDelayBoundIO

 -- * Result Trees
 , SCTTree(..)
 , SCTTreeIO(..)
 , sequenceIOTree

 -- * Utilities
 , preEmpCount
 ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.DeepSeq (NFData(..), force)
import Data.Foldable (Foldable(foldMap))
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Traversable (Traversable(traverse), fmapDefault, foldMapDefault)
import Test.DejaFu.Deterministic
import Test.DejaFu.Deterministic.IO (ConcIO, runConcIO)

-- * Result trees

-- | Results are presented in a lazy tree, where each node contains a
-- trace and a result. The children of a node represent those
-- schedules obtainable from it on the next bounding level.
data SCTTree a = SCTTree (Either Failure a) Trace [SCTTree a]
  deriving (Eq, Show)

instance Functor SCTTree where
  fmap = fmapDefault

instance Foldable SCTTree where
  foldMap = foldMapDefault

instance Traversable SCTTree where
  traverse f (SCTTree (Left  x) t trees) = SCTTree (Left x) <$> pure t <*> traverse (traverse f) trees
  traverse f (SCTTree (Right x) t trees) = SCTTree . Right  <$> f x <*> pure t <*> traverse (traverse f) trees

instance NFData a => NFData (SCTTree a) where
  rnf (SCTTree a t trees) = rnf (a, t, trees)

-- | Results which need IO to compute. Laziness is preserved by
-- wrapping child nodes in an 'IO' list.
data SCTTreeIO a = SCTTreeIO (Either Failure a) Trace (IO [SCTTreeIO a])

instance Functor SCTTreeIO where
  fmap f (SCTTreeIO a t iotrees) = SCTTreeIO (fmap f a) t $ fmap (map $ fmap f) iotrees

-- | Perform all of the 'IO' in an 'SCTTreeIO' from left to right. As
-- '>>=' for IO is strict, this will evaluate the entire tree, which
-- may be a lot of work. To counter this, a depth limit can optionally
-- be provided, where children below level @0@ will not be present in
-- the output.
sequenceIOTree :: Maybe Int -> SCTTreeIO a -> IO (SCTTree a)
sequenceIOTree (Just n) (SCTTreeIO a t iotrees)
  | n <= 0 = return $ SCTTree a t []
  | otherwise = do
    trees <- iotrees
    SCTTree a t <$> mapM (sequenceIOTree . Just $ n-1) trees
sequenceIOTree Nothing (SCTTreeIO a t iotrees) = do
  trees <- iotrees
  SCTTree a t <$> mapM (sequenceIOTree Nothing) trees

-- * Pre-emption bounding

-- | An SCT runner using a pre-emption bounding scheduler.
sctPreBound :: (forall t. Conc t a) -> [SCTTree a]
sctPreBound = sctBounded pbSiblings (pbOffspring False)

-- | Variant of 'sctPreBound' for computations which do 'IO'.
sctPreBoundIO :: (forall t. ConcIO t a) -> IO [SCTTreeIO a]
sctPreBoundIO = sctBoundedIO pbSiblings (pbOffspring True)

-- | Return all modifications to this schedule which do not introduce
-- extra pre-emptions.
pbSiblings :: [(CVarId, Bool)] -> Trace -> [[Decision]]
pbSiblings cvstate = siblings [] where
  siblings pref (t@(Start i, alts, _):ds) =
    let alters  = [[a] | (a@(Start _), act) <- alts, doesntBlock pref act]
        balters = [[a] | (a@(Start _), act) <- alts, not $ doesntBlock pref act]
    in (if null alters then balters else alters) ++
       [Start i : o | o <- siblings (pref++[t]) ds, not $ null o]

  siblings pref (t@(SwitchTo i, alts, _):ds) =
    let alters  = [[a] | (a@(SwitchTo _), act) <- alts, doesntBlock pref act]
        balters = [[a] | (a@(SwitchTo _), act) <- alts, not $ doesntBlock pref act]
    in (if null alters then balters else alters) ++
       [SwitchTo i : o | o <- siblings (pref++[t]) ds, not $ null o]

  siblings pref (t@(d, _, _):ds) = [d : o | o <- siblings (pref++[t]) ds, not $ null o]
  siblings _ [] = []

  -- TODO: Include also blocking on throwing exceptions to masked
  -- threads.
  doesntBlock pref (Put' c)  = (c, False) `elem` updateCVState cvstate pref
  doesntBlock pref (Read' c) = (c, True)  `elem` updateCVState cvstate pref
  doesntBlock pref (Take' c) = (c, True)  `elem` updateCVState cvstate pref
  doesntBlock _ _ = True

-- | Return all modifications to this schedule which do introduce an
-- extra pre-emption. Only introduce pre-emptions around CVar actions
-- and lifts.
pbOffspring :: Bool -> [(CVarId, Bool)] -> Trace -> [[Decision]]
pbOffspring lifts cvstate = offspring [] where
  offspring pref (t@(Continue, alts, ta):ds)
    | interesting lifts ta =
      let alters  = [[n] | (n@(SwitchTo _), act) <- alts, doesntBlock pref act]
          balters = [[n] | (n@(SwitchTo _), act) <- alts, not $ doesntBlock pref act]
      in (if null alters then balters else alters) ++
         [Continue : n | n <- offspring (pref++[t]) ds, not $ null n]
    | otherwise = [Continue : n | n <- offspring (pref++[t]) ds, not $ null n]

  offspring pref (t@(d, _, _):ds) = [d : n | n <- offspring (pref++[t]) ds, not $ null n]
  offspring _ [] = []

  -- TODO: Include also blocking on throwing exceptions to masked
  -- threads.
  doesntBlock pref (Put' c)  = (c, False) `elem` updateCVState cvstate pref
  doesntBlock pref (Read' c) = (c, True)  `elem` updateCVState cvstate pref
  doesntBlock pref (Take' c) = (c, True)  `elem` updateCVState cvstate pref
  doesntBlock _ _ = True

-- | Check the pre-emption count of some scheduling decisions.
preEmpCount :: [Decision] -> Int
preEmpCount (SwitchTo _:ss) = 1 + preEmpCount ss
preEmpCount (_:ss) = preEmpCount ss
preEmpCount [] = 0

-- * Delay bounding

-- | An SCT runner using a delay-bounding scheduler.
sctDelayBound :: (forall t. Conc t a) -> [SCTTree a]
sctDelayBound = sctBounded (\_ _ -> []) (dbOffspring False)

-- | Variant of 'sctDelayBound' for computations which do 'IO'.
sctDelayBoundIO :: (forall t. ConcIO t a) -> IO [SCTTreeIO a]
sctDelayBoundIO = sctBoundedIO (\_ _ -> []) (dbOffspring True)

-- | Return all modifications to the schedule which introduce an extra
-- delay. Only introduce delays around CVar actions and lifts.
dbOffspring :: Bool -> [(CVarId, Bool)] -> Trace -> [[Decision]]
dbOffspring lifts _ = offspring where
  offspring ((d, alts, ta):ds)
    | interesting lifts ta = [[fst n] | n <- alts] ++ [d : n | n <- offspring ds, not $ null n]
    | otherwise = [d : n | n <- dbOffspring lifts [] ds, not $ null n]
  offspring [] = []

-- * SCT runners

-- | SCT via schedule bounding. Results are proeduced as a lazy
-- forest, where each level represents one bounding level.
--
-- Schedules are generated by running the computation with a
-- deterministic scheduler with some initial list of decisions, after
-- which non-pre-emptive decisions are made. The generated suffix is
-- then used to generate \"siblings\" (schedule fragments which, when
-- appended to the prefix, form the prefix of a new schedule which
-- does not increase the bound), and \"offspring\" (like siblings, but
-- the bound has been increased by one). It is important that siblings
-- and offspring are unique, and that the same
-- prefix+sibling/offspring cannot arise from two distinct traces, as
-- otherwise the runner may loop. In addition to the trace, the
-- sibling and offspring functions take a list of whether each 'CVar'
-- existing at this point is full or not, allowing blocking behaviour
-- to be factored into decision-making.
--
-- For example, the siblings in the pre-emption bounding runner are
-- those schedule fragments which, when appended to the prefix, form
-- the prefix of a new schedule which does not introduce any new
-- pre-emptions; and the offspring do introduce one new pre-emption.
sctBounded :: ([(CVarId, Bool)] -> Trace -> [[Decision]])
           -- ^ Sibling generation function.
           -> ([(CVarId, Bool)] -> Trace -> [[Decision]])
           -- ^ Child generation function.
           -> (forall t. Conc t a) -> [SCTTree a]
sctBounded siblings offspring c = go [] where
  go ds = case res of
    Left  f -> SCTTree (Left  f) trace [] : concatMap go sibs
    Right a -> SCTTree (Right a) trace (concatMap go offs) : concatMap go sibs

    where
      (res, _, trace) = runConc prefixSched ds c

      (p, suff) = splitAt (length ds) trace
      pref      = map (\(a,_,_) -> a) p

      cvstate = computeCVState p

      sibs = [ pref ++ y | y <- siblings  cvstate suff]
      offs = [ pref ++ y | y <- offspring cvstate suff]

-- | Variant of 'sctBounded' for computations which do 'IO'.
sctBoundedIO :: ([(CVarId, Bool)] -> Trace -> [[Decision]])
             -> ([(CVarId, Bool)] -> Trace -> [[Decision]])
             -> (forall t. ConcIO t a) -> IO [SCTTreeIO a]
sctBoundedIO siblings offspring c = go [] where
  go ds = do
    (res, _, trace) <- runConcIO prefixSched ds c

    let (p, suff) = splitAt (length ds + 1) trace
    let pref      = map (\(a,_,_) -> a) p

    let cvstate = computeCVState p

    let sibs = [ pref ++ y | y <- siblings  cvstate suff]
    let offs = [ pref ++ y | y <- offspring cvstate suff]

    sibs' <- concat <$> mapM go sibs

    return $ case res of
      Left  f -> SCTTreeIO (Left f)  trace (return []) : sibs'
      Right a -> SCTTreeIO (Right a) trace (concat <$> mapM go offs) : sibs'

-- | Compute the state of every 'CVar' after executing the given
-- prefix trace. This assumes that the trace is valid (no double-puts,
-- etc).
computeCVState :: Trace -> [(CVarId, Bool)]
computeCVState = updateCVState []

-- | Compute the state of every 'CVar' from the given starting point
-- after executing the given prefix trace. This assumes that the trace
-- is valid.
updateCVState :: [(CVarId, Bool)] -> Trace -> [(CVarId, Bool)]
updateCVState = foldl' go where
  go state (_, _, New  c)   = (c, False) : state
  go state (_, _, Put  c _) = (c, True)  : filter ((/=c) . fst) state
  go state (_, _, Take c _) = (c, False) : filter ((/=c) . fst) state
  go state (_, _, TryPut  c b _) = (c, b)   : filter ((/=c) . fst) state
  go state (_, _, TryTake c b _) = (c, not b) : filter ((/=c) . fst) state
  go state _ = state

-- * Prefix scheduler

-- | Scheduler which uses a list of scheduling decisions to drive the
-- initial decisions.
prefixSched :: Scheduler [Decision]
prefixSched = force $ \s prior threads@((next,_):|_) -> case s of
  -- If we have a decision queued, make it.
  (Start t:ds)    -> (t, ds)
  (Continue:ds)   -> (fromMaybe 0 prior, ds)
  (SwitchTo t:ds) -> (t, ds)

  -- Otherwise just use a non-pre-emptive scheduler.
  [] -> case prior of
    Just prior' | prior' `elem` map fst (toList threads) -> (prior', [])
    _ -> (next, [])

-- * Utils

-- | Check if a 'ThreadAction' might be an interesting candidate for
-- pre-empting or delaying.
interesting :: Bool -> ThreadAction -> Bool
interesting _ (Put _ _)       = True
interesting _ (TryPut _ _ _)  = True
interesting _ (Take _ _)      = True
interesting _ (TryTake _ _ _) = True
interesting _ (Read _)        = True
interesting _ (ReadRef _)     = True
interesting _ (ModRef _)      = True
interesting _ (STM _)         = True
interesting _ (ThrowTo _)     = True
interesting _ (SetMasking _ _) = True
interesting _ (ResetMasking _ _ ) = True
interesting l Lift = l
interesting _ _ = False

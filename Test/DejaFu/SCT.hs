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
pbSiblings :: Trace -> [[Decision]]
pbSiblings = siblings . map (\(d,a,_) -> (d,a)) where
  siblings ((Start    i, alts):ds) = [[a] | a@(Start    _) <- alts] ++ [Start    i : o | o <- siblings ds, not $ null o]
  siblings ((SwitchTo i, alts):ds) = [[a] | a@(SwitchTo _) <- alts] ++ [SwitchTo i : o | o <- siblings ds, not $ null o]
  siblings ((d, _):ds) = [d : o | o <- siblings ds, not $ null o]
  siblings [] = []

-- | Return all modifications to this schedule which do introduce an
-- extra pre-emption. Only introduce pre-emptions around CVar actions
-- and lifts.
pbOffspring :: Bool -> Trace -> [[Decision]]
pbOffspring lifts ((Continue, alts, ta):ds)
  | interesting lifts ta = [[n] | n@(SwitchTo _) <- alts] ++ [Continue : n | n <- pbOffspring lifts ds, not $ null n]
  | otherwise = [Continue : n | n <- pbOffspring lifts ds, not $ null n]

pbOffspring lifts ((d, _, _):ds) = [d : n | n <- pbOffspring lifts ds, not $ null n]
pbOffspring _ [] = []

-- | Check the pre-emption count of some scheduling decisions.
preEmpCount :: [Decision] -> Int
preEmpCount (SwitchTo _:ss) = 1 + preEmpCount ss
preEmpCount (_:ss) = preEmpCount ss
preEmpCount [] = 0

-- * Delay bounding

-- | An SCT runner using a delay-bounding scheduler.
sctDelayBound :: (forall t. Conc t a) -> [SCTTree a]
sctDelayBound = sctBounded (const []) (dbOffspring False)

-- | Variant of 'sctDelayBound' for computations which do 'IO'.
sctDelayBoundIO :: (forall t. ConcIO t a) -> IO [SCTTreeIO a]
sctDelayBoundIO = sctBoundedIO (const []) (dbOffspring True)

-- | Return all modifications to the schedule which introduce an extra
-- delay. Only introduce delays around CVar actions and lifts.
dbOffspring :: Bool -> Trace -> [[Decision]]
dbOffspring lifts ((d, alts, ta):ds)
  | interesting lifts ta = [[n] | n <- alts] ++ [d : n | n <- dbOffspring lifts ds, not $ null n]
  | otherwise = [d : n | n <- dbOffspring lifts ds, not $ null n]

dbOffspring _ [] = []

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
-- otherwise the runner may loop.
--
-- For example, the siblings in the pre-emption bounding runner are
-- those schedule fragments which, when appended to the prefix, form
-- the prefix of a new schedule which does not introduce any new
-- pre-emptions; and the offspring do introduce one new pre-emption.
sctBounded :: (Trace -> [[Decision]])
           -- ^ Sibling generation function.
           -> (Trace -> [[Decision]])
           -- ^ Child generation function.
           -> (forall t. Conc t a) -> [SCTTree a]
sctBounded siblings offspring c = go [] where
  go ds = case res of
    Left  f -> SCTTree (Left  f) trace [] : concatMap go sibs
    Right a -> SCTTree (Right a) trace (concatMap go offs) : concatMap go sibs

    where
      (res, _, trace) = runConc prefixSched ds c

      (pref, suff) = let (p, s) = splitAt (length ds) trace in (map (\(a,_,_) -> a) p, s)

      sibs = [ pref ++ y | y <- siblings  suff]
      offs = [ pref ++ y | y <- offspring suff]

-- | Variant of 'sctBounded' for computations which do 'IO'.
sctBoundedIO :: (Trace -> [[Decision]])
             -> (Trace -> [[Decision]])
             -> (forall t. ConcIO t a) -> IO [SCTTreeIO a]
sctBoundedIO siblings offspring c = go [] where
  go ds = do
    (res, _, trace) <- runConcIO prefixSched ds c

    let (pref, suff) = let (p, s) = splitAt (length ds + 1) trace in (map (\(a,_,_) -> a) p, s)

    let sibs = [ pref ++ y | y <- siblings  suff]
    let offs = [ pref ++ y | y <- offspring suff]

    sibs' <- concat <$> mapM go sibs

    return $ case res of
      Left  f -> SCTTreeIO (Left f)  trace (return []) : sibs'
      Right a -> SCTTreeIO (Right a) trace (concat <$> mapM go offs) : sibs'

-- * Prefix scheduler

-- | Scheduler which uses a list of scheduling decisions to drive the
-- initial decisions.
prefixSched :: Scheduler [Decision]
prefixSched = force $ \s prior threads@(next:|_) -> case s of
  -- If we have a decision queued, make it.
  (Start t:ds)    -> (t, ds)
  (Continue:ds)   -> (fromMaybe 0 prior, ds)
  (SwitchTo t:ds) -> (t, ds)

  -- Otherwise just use a non-pre-emptive scheduler.
  [] -> case prior of
    Just prior' | prior' `elem` toList threads -> (prior', [])
    _ -> (next, [])

-- * Utils

-- | Check if a 'ThreadAction' might be an interesting candidate for
-- pre-empting or delaying.
interesting :: Bool -> ThreadAction -> Bool
interesting _ (Put _ _)       = True
interesting _ (TryPut _ _ _)  = True
interesting _ (Take _ _)      = True
interesting _ (TryTake _ _ _) = True
interesting _ (BlockedPut _)  = True
interesting _ (Read _)        = True
interesting _ (BlockedRead _) = True
interesting _ (BlockedTake _) = True
interesting _ (ReadRef _)     = True
interesting _ (ModRef _)      = True
interesting _ (STM _)         = True
interesting _ BlockedSTM      = True
interesting _ (ThrowTo _)     = True
interesting _ (SetMasking _ _) = True
interesting _ (ResetMasking _ _ ) = True
interesting l Lift = l
interesting _ _ = False

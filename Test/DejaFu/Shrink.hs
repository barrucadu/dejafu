{-# LANGUAGE Rank2Types #-}

-- | Functions for attempting to find maximally simple traces
-- producing a given result.
module Test.DejaFu.Shrink
  ( shrink
  , shrinkIO

  -- * Utilities
  -- | If you wanted to implement your own shrinking logic, these are
  -- the building blocks.
  , candidates
  , try
  , tryIO
  , essential
  , simplest
  ) where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (sortBy, isPrefixOf, nubBy)
import Data.Maybe (fromJust, listToMaybe)
import Data.Ord (comparing)
import Test.DejaFu.Deterministic
import Test.DejaFu.Deterministic.Internal
import Test.DejaFu.Deterministic.IO (ConcIO)
import Test.DejaFu.SCT.Bounding
import Test.DejaFu.SCT.Internal

-- | Attempt to find a trace with a minimal number of pre-emptions
-- that gives rise to the desired output.
shrink :: Eq a => (Maybe a, Trace) -> (forall t. Conc t a) -> Trace
shrink (result, trace) t = shrink' [] trace where
  shrink' e trc = case nextscts of
    -- No candidates for further shrinking found
    [] -> trc
    -- Attempt to shrink further. Safe because shrink'' will always
    -- return at least one result.
    ts -> fromJust . simplest $ map (uncurry shrink') ts

    where
      -- Get all candidate trace prefixes which start with the given
      -- essential portion.
      cands = filter (\(ds, _) -> e `isPrefixOf` ds) $ candidates trc

      -- Get traces for further shrinking, by finding the essential
      -- prefixes out of the candidates
      nextscts = concatMap step cands

      -- Pick a candidate for further simplification, and attempt to
      -- identify a new essential trace prefix.
      step (pref, tid) =
        let tries = filter (/=trace) $ try (==result) pref t
        in case simplest tries of
             -- No further candidates available.
             Nothing -> []
             Just best ->
               if essential (pref, tid) tries
               -- If the pre-emption is essential, we have a new
               -- essential prefix.
               then [(pref ++ [SwitchTo tid], best)]
               -- If not, just re-use the original prefix.
               else [(e, best)]

-- | Variant of 'shrink' for computations which do 'IO'.
shrinkIO :: Eq a => (Maybe a, Trace) -> (forall t. ConcIO t a) -> IO Trace
shrinkIO (result, trace) t = shrink' [] trace where
  shrink' e trc = do
    let cands = filter (\(ds, _) -> e `isPrefixOf` ds) $ candidates trc
    nextscts <- concat <$> mapM step cands

    case nextscts of
      [] -> return trc
      ts -> fromJust . simplest <$> mapM (uncurry shrink') ts

    where
      step (pref, tid) = do
        tries <- tryIO (==result) pref t
        return $
          case simplest tries of
            Nothing   -> []
            Just best ->
              if essential (pref, tid) tries
              then [(pref ++ [SwitchTo tid], best)]
              else [(e, best)]

-- | Generate all candidate trace prefixes from a trace. These are
-- produced by attempting to drop one pre-emption. Furthermore, the
-- 'ThreadId' of the thread which performed the dropped pre-emption is
-- also returned.
candidates :: Trace -> [([Decision], ThreadId)]
candidates = candidates' [] where
  candidates' _ [] = []
  candidates' ps ((SwitchTo i, _, _):ts) = (reverse ps, i) : candidates' (SwitchTo i : ps) ts
  candidates' ps ((t, _, _):ts) = candidates' (t : ps) ts

-- | Try all traces with a given trace prefix, which introduce no more
-- than one new pre-emption, and return those which satisfy the
-- predicate.
try :: (Maybe a -> Bool) -> [Decision] -> (forall t. Conc t a) -> [Trace]
try p pref c = map snd . filter (p . fst) $ sctPreBoundOffset pref c

-- | Variant of 'try' for computations which do 'IO'.
tryIO :: (Maybe a -> Bool) -> [Decision] -> (forall t. ConcIO t a) -> IO [Trace]
tryIO p pref c = map snd . filter (p . fst) <$> sctPreBoundOffsetIO pref c

-- | Given a list of 'Trace's which follow on from a given prefix,
-- determine if the removed pre-emption is /essential/. That is, every
-- 'Trace' starts with the prefix followed immediately by a
-- pre-emption to the given thread.
essential :: ([Decision], ThreadId) -> [Trace] -> Bool
essential (ds, tid) = all ((pref `isPrefixOf`) . map (\(d,_,_) -> d)) where
  pref = ds ++ [SwitchTo tid]

-- | Return the simplest 'Trace' from a collection. Roughly, the
-- one with the fewest pre-emptions. If the list is empty, return
-- 'Nothing'.
simplest :: [Trace] -> Maybe Trace
simplest = listToMaybe . nubBy (lexico `on` map (\(d,_,_) -> d)) . restrict contextSwitch . restrict preEmpCount where

  -- Favour schedules with fewer context switches if they have the
  -- same number of pre-emptions.
  contextSwitch (Start _:ss) = 1 + contextSwitch ss
  contextSwitch (_:ss) = contextSwitch ss
  contextSwitch [] = 0::Int -- Prevents a warning about defaulting.

  -- Favour shorter schedules with lexicographically "smaller" context
  -- switches if all else is equal.
  lexico (Continue:as) (b:bs) = b /= Continue || lexico as bs
  lexico (Start i:_) (Start j:_) = i < j
  lexico (SwitchTo i:_) (SwitchTo j:_) = i < j
  lexico (_:as) (_:bs) = lexico as bs
  lexico [] _ = True
  lexico _ [] = False

  -- Find the best element(s) of the list and drop all worse ones.
  restrict f xs =
    let f' = f . map (\(d,_,_) -> d)
    in case sortBy (comparing $ f') xs of
         [] -> []
         ys -> let best = f' $ head ys in filter ((==best) . f') ys

-- | Like pre-emption bounding, but rather than starting from nothing,
-- use the given trace prefix.
sctPreBoundOffset :: [Decision] -> (forall t. Conc t a) -> [(Maybe a, Trace)]
sctPreBoundOffset ds = runSCT' prefixSched (initialS', initialG) bTerm (bStep pbSiblings (pbOffspring False) pb) where
  initialS' = initialS { _decisions = tail ds, _prefixlen = length ds - 1 }
  pb = preEmpCount ds + 1

-- | Variant of 'sctPreBoundOffset' for computations which do 'IO'.
sctPreBoundOffsetIO :: [Decision] -> (forall t. ConcIO t a) -> IO [(Maybe a, Trace)]
sctPreBoundOffsetIO ds = runSCTIO' prefixSched (initialS', initialG) bTerm (bStep pbSiblings (pbOffspring False) pb) where
  initialS' = initialS { _decisions = tail ds, _prefixlen = length ds - 1 }
  pb = preEmpCount ds + 1

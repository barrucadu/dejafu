{-# LANGUAGE Rank2Types #-}

-- Building blocks for SCT runners based on schedule bounding, with
-- implementations of pre-emption bounding and delay bounding.
module Control.Monad.Conc.SCT.Bounding where

import Control.DeepSeq (NFData(..), force)
import Control.Monad.Conc.Fixed
import Control.Monad.Conc.SCT.Internal
import Data.List.Extra

import qualified Control.Monad.Conc.Fixed.IO as CIO

-- * Pre-emption bounding

-- | An SCT runner using a pre-emption bounding scheduler. Schedules
-- are explored systematically, in a depth-first fashion.
sctPreBound :: Int -> (forall t. Conc t a) -> [(Maybe a, SCTTrace)]
sctPreBound = sctBounded pbSiblings (pbOffspring False)

-- | Variant of 'sctPreBound' using 'IO'. See usual caveats about IO.
sctPreBoundIO :: Int -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
sctPreBoundIO = sctBoundedIO pbSiblings (pbOffspring True)

-- | Return all modifications to this schedule which do not introduce
-- extra pre-emptions.
pbSiblings :: SCTTrace -> [[Decision]]
pbSiblings = siblings . map (\(d,a,_) -> (d,a)) where
  siblings ((Start    i, alts):ds) = [[a] | a@(Start    _) <- alts] ++ [Start    i : o | o <- siblings ds, not $ null o]
  siblings ((SwitchTo i, alts):ds) = [[a] | a@(SwitchTo _) <- alts] ++ [SwitchTo i : o | o <- siblings ds, not $ null o]
  siblings ((d, _):ds) = [d : o | o <- siblings ds, not $ null o]
  siblings [] = []

-- | Return all modifications to this schedule which do introduce an
-- extra pre-emption. Only introduce pre-emptions around CVar actions
-- and lifts.
pbOffspring :: Bool -> SCTTrace -> [[Decision]]
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

-- | An SCT runner using a delay-bounding scheduler. Schedules are
-- explored systematically, in a depth-first fashion.
sctDelayBound :: Int -> (forall t. Conc t a) -> [(Maybe a, SCTTrace)]
sctDelayBound = sctBounded (const []) (dbOffspring False)

-- | Variant of 'sctDelayBound' using 'IO'. See usual caveats about
-- IO.
sctDelayBoundIO :: Int -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
sctDelayBoundIO = sctBoundedIO (const []) (dbOffspring True)

-- | Return all modifications to the schedule which introduce an extra
-- delay. Only introduce delays around CVar actions and lifts.
dbOffspring :: Bool -> SCTTrace -> [[Decision]]
dbOffspring lifts ((d, alts, ta):ds)
  | interesting lifts ta = [[n] | n <- alts] ++ [d : n | n <- dbOffspring lifts ds, not $ null n]
  | otherwise = [d : n | n <- dbOffspring lifts ds, not $ null n]

dbOffspring _ [] = []

-- * SCT runners

-- | An SCT runner using schedule bounding.
sctBounded :: (SCTTrace -> [[Decision]])
           -- ^ Sibling generation function.
           -> (SCTTrace -> [[Decision]])
           -- ^ Child generation function.
           -> Int
           -- ^ Bound, anything < 0 will be interpreted as no bound.
           -> (forall t. Conc t a) -> [(Maybe a, SCTTrace)]
sctBounded siblings offspring b = runSCT' prefixSched (initialS, initialG) bTerm (bStep siblings offspring b)

-- | Variant of 'sctBounded' using 'IO'.
sctBoundedIO :: (SCTTrace -> [[Decision]])
             -> (SCTTrace -> [[Decision]])
             -> Int
             -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
sctBoundedIO siblings offspring b = runSCTIO' prefixSched (initialS, initialG) bTerm (bStep siblings offspring b)

-- * State

-- | State for the prefix scheduler.
data Sched = S
  { _decisions :: [Decision]
  -- ^ The list of decisions still to make.
  , _prefixlen :: Int
  -- ^ How long the prefix originally was.
  }

instance NFData Sched where
  rnf s = rnf (_decisions s, _prefixlen s)

-- | State for the bounded runner.
data State = P
  { _next :: Stream Int [Decision]
  -- ^ Schedules to try.
  , _halt :: Bool
  -- ^ Indicates more schedules couldn't be found, and to halt
  -- immediately.
  }

-- | Initial scheduler state for the prefix scheduler.
initialS :: Sched
initialS = S { _decisions = [], _prefixlen = 0 }

-- | Initial runner state for the bounded runner.
initialG :: State
initialG = P { _next = Empty 0, _halt = False }

-- * Prefix scheduler

-- | Scheduler which uses a list of scheduling decisions to drive the
-- initial decisions.
prefixSched :: SCTScheduler Sched
prefixSched = force . makeSCT $ \s prior threads@(next:|_) -> case _decisions s of
  -- If we have a decision queued, make it.
  (Start t:ds)    -> (t,     s { _decisions = ds })
  (Continue:ds)   -> (prior, s { _decisions = ds })
  (SwitchTo t:ds) -> (t,     s { _decisions = ds })

  -- Otherwise just use a non-pre-emptive scheduler.
  [] | prior `elem` toList threads -> (prior, s)
     | otherwise -> (next,  s)

-- * Bounded runner

-- | Termination function: checks for the halt flag.
bTerm :: (a, State) -> Bool
bTerm (_, g) = _halt g

-- | Schedule bounding state step function: computes remaining
-- schedules to try and chooses one.
--
-- This effectively produces schedules in a depth-first order, rather
-- than breadth-first. This means it will explore some schedules with
-- a higher bound before all the ones with a lower bound. Testing with
-- a very concurrent problem (finding a deadlock in 100 dining
-- philosophers) has revealed this may work better in practice.
bStep :: (SCTTrace -> [[Decision]])
      -- ^ Sibling generation function.
      -> (SCTTrace -> [[Decision]])
      -- ^ Offspring generation function.
      -> Int
      -- ^ Bound.
      -> (Sched, State) -> SCTTrace -> (Sched, State)
bStep siblings offspring blim (s, g) t = case _next g of
  -- We have schedules remaining, so run the next
  Stream (b, x:|xs) rest
    | b /= blim  -> (s' x, g { _next = (b+1, next) +| (b, this) +| (b, xs) +| rest })
    | otherwise -> (s' x, g { _next =                (b, this) +| (b, xs) +| rest })

  -- We have no schedules remaining, try to generate some more.
  --
  -- If there are no more schedules, halt.
  Empty b ->
    case (this, next) of
      -- We still have schedules in the current bound, so add those to
      -- the queue (and any schedules from the next bound if we're not at
      -- the limit)
      (x:xs, _)
        | b /= blim  -> (s' x, g { _next = (b+1, next) +| (b, xs) +| Empty b })
        | otherwise -> (s' x, g { _next =                (b, xs) +| Empty b })

      -- No schedules left in this bound, but if we have some more from
      -- the next bound (and we're not at the limit) add those.
      ([], x:xs)
        | b /= blim  -> (s' x, g { _next = (b+1, xs) +| Empty (b+1) })

      -- No schedules left at all, so halt.
      _ -> halt

  where
    (pref, suff) = splitAtF (\((Start 0,_,_):px) -> (map (\(d,_,_) -> d) px ++)) id (_prefixlen s + 1) t

    -- | New scheduler state, with a given list of initial decisions.
    s' ds = initialS { _decisions = ds, _prefixlen = length ds }

    -- | The halt state
    halt = (initialS, g { _halt = True })

    -- | All (new, unique) schedules we get from the current one
    -- WITHOUT increasing the bound.
    this = [ pref y | y <- siblings suff]

    -- | All (new, unique) schedules we get from the current one with
    -- ONE increase to the bound.
    next = [ pref y | y <- offspring suff]

-- * Utils

-- | Check if a 'ThreadAction' might be an interesting candidate for
-- pre-empting or delaying.
interesting :: Bool -> ThreadAction -> Bool
interesting _ (Put _)       = True
interesting _ (TryPut _ _)  = True
interesting _ (Take _)      = True
interesting _ (TryTake _ _) = True
interesting _ BlockedPut  = True
interesting _ Read        = True
interesting _ BlockedRead = True
interesting _ BlockedTake = True
interesting l Lift = l
interesting _ _ = False

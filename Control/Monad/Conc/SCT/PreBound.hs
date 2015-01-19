{-# LANGUAGE RankNTypes #-}

-- Pre-emption bounding SCT runner for Conc monads.
module Control.Monad.Conc.SCT.PreBound
  ( -- * SCT Runners
    sctPreBound
  , sctPreBoundIO
  -- * Utils
  , preEmpCount
  ) where

import Control.DeepSeq (NFData(..), force)
import Control.Monad.Conc.Fixed
import Control.Monad.Conc.SCT.Internal

import qualified Control.Monad.Conc.Fixed.IO as CIO

-- * SCT Runners

-- | An SCT runner using a pre-emption bounding scheduler. Schedules
-- are explored systematically, in a depth-first fashion.
sctPreBound :: Int
            -- ^ The pre-emption bound. Anything < 0 will be
            -- interpreted as no bound.
            -> (forall t. Conc t a) -> [(Maybe a, SCTTrace)]
sctPreBound pb = runSCT' pbSched (pbInitialS, pbInitialG) (pbTerm pb) (pbStep pb False)

-- | Variant of 'sctPreBound' using 'IO'. See usual caveats about IO.
sctPreBoundIO :: Int -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
sctPreBoundIO pb = runSCTIO' pbSched (pbInitialS, pbInitialG) (pbTerm pb) (pbStep pb True)

-- * Utils

-- | Check the pre-emption count of some scheduling decisions.
preEmpCount :: [Decision] -> Int
preEmpCount (SwitchTo _:ss) = 1 + preEmpCount ss
preEmpCount (_:ss) = preEmpCount ss
preEmpCount [] = 0

-- * State

-- | Data type representing a lazy, chunky, stream of data.
data Lazy a = Lazy (NonEmpty a) (Lazy a) | Empty

-- | Prepend a value onto a lazy stream.
(+|) :: [a] -> Lazy a -> Lazy a
[]     +| l = l
(x:xs) +| l = Lazy (x:|xs) l

infixr +|

-- | State for the PB scheduler.
data PBSched = S
  { _decisions :: [Decision]
  -- ^ The list of decisions still to make.
  , _prefix :: SchedTrace
  -- ^ The trace prefix corresponding to the decisions.
  , _suffix :: SchedTrace
  -- ^ The trace suffix corresponding to independent decisions.
  }

instance NFData PBSched where
  rnf s = rnf (_decisions s, _prefix s, _suffix s)

-- | State for the PB runner.
data PBState = P
  { _pc :: Int
  -- ^ Current pre-emption count.
  , _next :: Lazy [Decision]
  -- ^ Schedules to try.
  , _halt :: Bool
  -- ^ Indicates more schedules couldn't be found, and to halt
  -- immediately.
  }

-- | Initial scheduler state for the PB scheduler.
pbInitialS :: PBSched
pbInitialS = S { _decisions = [], _prefix = [], _suffix = [] }

-- | Initial runner state for the PB scheduler.
pbInitialG :: PBState
pbInitialG = P { _pc = 0, _next = Empty, _halt = False }

-- * PB Scheduler

-- | Pre-emption bounding scheduler, which uses a queue of scheduling
-- decisions to drive the initial trace, returning the generated
-- suffix.
pbSched :: SCTScheduler PBSched
pbSched (s, trc) prior threads@(next:|_) = force $ case _decisions s of
  -- If we have a decision queued, make it.
  (Start t:ds)    -> let trc' = (Start t,    alters t)     in (t,     (s { _decisions = ds, _prefix = trc' : _prefix s}, trc':trc))
  (Continue:ds)   -> let trc' = (Continue,   alters prior) in (prior, (s { _decisions = ds, _prefix = trc' : _prefix s}, trc':trc))
  (SwitchTo t:ds) -> let trc' = (SwitchTo t, alters t)     in (t,     (s { _decisions = ds, _prefix = trc' : _prefix s}, trc':trc))

  -- Otherwise just use a non-pre-emptive scheduler.
  [] | prior `elem` threads' -> let trc' = (Continue,   alters prior) in (prior, (s { _suffix = trc' : _suffix s}, trc':trc))
     | otherwise             -> let trc' = (Start next, alters next)  in (next,  (s { _suffix = trc' : _suffix s}, trc':trc))

  where
    alters tid
      | tid == prior           = map SwitchTo $ filter (/=prior) threads'
      | prior `elem` threads' = Continue : map SwitchTo (filter (\t -> t /= prior && t /= tid) threads')
      | otherwise             = map Start $ filter (/=tid) threads'

    threads' = toList threads

-- | Pre-emption bounding termination function: terminates on attempt
-- to start a PB above the limit.
pbTerm :: Int -> (a, PBState) -> Bool
pbTerm pb (_, g)
  | pb < 0    = _halt g
  | otherwise = (_pc g == pb + 1) || _halt g

-- | Pre-emption bounding state step function: computes remaining
-- schedules to try and chooses one.
--
-- This effectively produces schedules in a depth-first order, rather
-- than breadth-first. This means it will explore some schedules with
-- a higher pre-emption count before all the ones with a lower
-- count. Testing with a very concurrent problem (finding a deadlock
-- in 100 dining philosophers) has revealed this may work better in
-- practice.
pbStep :: Int
       -- ^ Pre-emption bound.
       -> Bool
       -- ^ Whether to consider pre-emptions around lifts.
       -> (PBSched, PBState) -> SCTTrace -> (PBSched, PBState)
pbStep pb lifts (s, g) t = case _next g of
  -- We have schedules remaining, so run the next
  Lazy (x:|xs) rest
    | pb /= _pc g -> (s' x, g { _next = nextPB +| thisPB +| xs +| rest })
    | otherwise  -> (s' x, g { _next =           thisPB +| xs +| rest })

  -- We have no schedules remaining, try to generate some more.
  --
  -- If there are no more schedules, halt.
  Empty ->
    case thisPB of
      (x:xs)
        | pb /= _pc g -> (s' x, g { _next = nextPB +| xs +| Empty })
        | otherwise  -> (s' x, g { _next =           xs +| Empty })
      [] -> (s' [], g { _halt = True })

  where
    -- The prefix and suffix are in reverse order, fix those.
    pref = reverse $ _prefix s
    suff = reverse $ _suffix s

    -- A prefix we can append decisions to, and a suffix with
    -- 'ThreadAction' information.
    pref' rest = if null pref then (\((d,_,_):_) -> d:rest) t else map fst pref ++ rest
    suff' = drop (length pref) t

    -- | New scheduler state, with a given list of initial decisions.
    s' ds = pbInitialS { _decisions = tail ds }

    -- | All schedules we get from the current one WITHOUT introducing
    -- any pre-emptions.
    thisPB = [ pref' y | y <- siblings suff]

    -- | All schedules we get from the current one with ONE extra
    -- pre-emption.
    nextPB = [ pref' y | y <- offspring lifts suff']

-- * Utils (Internal)

-- | Return all modifications to this schedule which do not introduce
-- extra pre-emptions.
siblings :: SchedTrace -> [[Decision]]
siblings ((Start    i, alts):ds) = [Start    i : o | o <- siblings ds, not $ null o] ++ [[a] | a <- alts]
siblings ((SwitchTo i, alts):ds) = [SwitchTo i : o | o <- siblings ds, not $ null o] ++ [[a] | a <- alts]
siblings ((d, _):ds) = [d : o | o <- siblings ds, not $ null o]
siblings [] = []

-- | Return all modifications to this schedule which do introduce an
-- extra pre-emption. Only introduce pre-emptions around CVar actions.
offspring :: Bool -> SCTTrace -> [[Decision]]
offspring lifts ((Continue, alts, ta):ds)
  | preCand lifts ta = [Continue : n | n <- offspring lifts ds, not $ null n] ++ [[n] | n <- alts]
  | otherwise = [Continue : n | n <- offspring lifts ds, not $ null n]
offspring lifts ((d, _, _):ds) = [d : n | n <- offspring lifts ds]
offspring _ [] = []

-- | Check if a 'ThreadAction' is a candidate for pre-emption.
preCand :: Bool -> ThreadAction -> Bool
preCand _ (Put _)       = True
preCand _ (TryPut _ _)  = True
preCand _ (Take _)      = True
preCand _ (TryTake _ _) = True
preCand _ BlockedPut  = True
preCand _ Read        = True
preCand _ BlockedRead = True
preCand _ BlockedTake = True
preCand b Lift = b
preCand _ _ = False

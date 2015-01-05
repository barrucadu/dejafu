{-# LANGUAGE RankNTypes  #-}

-- | A runner for concurrent monads to systematically detect
-- concurrency errors such as data races and deadlocks.
--
-- As an example, consider this program, which has two locks and a
-- shared variable. Two threads are spawned, which claim the locks,
-- update the shared variable, and release the locks. The main thread
-- waits for them both to terminate, and returns the final result.
--
-- > bad :: ConcCVar cvar m => m Int
-- > bad = do
-- >   a <- newEmptyCVar
-- >   b <- newEmptyCVar
-- > 
-- >   c <- newCVar 0
-- > 
-- >   j1 <- spawn $ lock a >> lock b >> modifyCVar_ c (return . succ) >> unlock b >> unlock a
-- >   j2 <- spawn $ lock b >> lock a >> modifyCVar_ c (return . pred) >> unlock a >> unlock b
-- > 
-- >   takeCVar j1
-- >   takeCVar j2
-- > 
-- >   takeCVar c
--
-- The correct result is 0, as it starts out as 0 and is incremented
-- and decremented by threads 1 and 2, respectively. However, note the
-- order of acquisition of the locks in the two threads. If thread 2
-- pre-empts thread 1 between the acquisition of the locks (or if
-- thread 1 pre-empts thread 2), a deadlock situation will arise, as
-- thread 1 will have lock `a` and be waiting on `b`, and thread 2
-- will have `b` and be waiting on `a`.

module Control.Monad.Conc.SCT
 ( -- * Types
   SCTScheduler
 , SchedTrace
 , SCTTrace
 , Decision(..)

-- * SCT Runners
 , runSCT
 , runSCTIO
 , runSCT'
 , runSCTIO'

 -- * Random Schedulers
 , sctRandom
 , sctRandomNP

 -- * Pre-emption Bounding
 , sctPreBound
 , sctPreBoundIO
 , preEmpCount

 -- * Utilities
 , toSCT
 , showTrace
 , ordNub
 ) where

import Control.Monad.Conc.Fixed
import System.Random (RandomGen)

import qualified Control.Monad.Conc.Fixed.IO as CIO
import qualified Data.Set as Set

-- * Types

-- | An @SCTScheduler@ is like a regular 'Scheduler', except it builds
-- a trace of scheduling decisions made.
type SCTScheduler s = Scheduler (s, SchedTrace)

-- | A @SchedTrace@ is just a list of all the decisions that were made,
-- with the alternative decisions that could have been made at each
-- step
type SchedTrace = [(Decision, [Decision])]

-- | A @SCTTrace@ is a combined 'SchedTrace' and 'Trace'.
type SCTTrace = [(Decision, [Decision], ThreadAction)]

-- | Scheduling decisions are based on the state of the running
-- program, and so we can capture some of that state in recording what
-- specific decision we made.
data Decision =
    Start ThreadId
  -- ^ Start a new thread, because the last was blocked (or it's the
  -- initial thread).
  | Continue
  -- ^ Continue running the last thread for another step.
  | SwitchTo ThreadId
  -- ^ Pre-empt the running thread, and switch to another.
  deriving (Eq, Ord, Show)

-- * SCT Runners

-- | Run a concurrent program under a given scheduler a number of
-- times, collecting the results and the scheduling that gave rise to
-- them.
--
-- The initial state for each run is the final state of the last run,
-- so it is important that the scheduler actually maintain some
-- internal state, or all the results will be identical.
runSCT :: SCTScheduler s -> s -> Int -> (forall t. Conc t a) -> [(Maybe a, SCTTrace)]
runSCT sched s n = runSCT' sched s n term step where
  term _  g = g == 0
  step s' g _ = (s', g - 1)

-- | A varant of 'runSCT' for concurrent programs that do 'IO'.
--
-- Warning! The IO will be executed lots of times, in lots of
-- interleavings! Be very confident that nothing in a 'liftIO' can
-- block on the action of another thread, or you risk deadlocking this
-- function!
runSCTIO :: SCTScheduler s -> s -> Int -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
runSCTIO sched s n = runSCTIO' sched s n term step where
  term _  g = g == 0
  step s' g _ = (s', g - 1)

-- | Run a concurrent program under a given scheduler, where the SCT
-- runner itself maintains some internal state, and has a function to
-- produce a new scheduler state for each run, and decide termination
-- based on the internal state.
--
-- Note: the state step function takes the state returned by the
-- scheduler, not the initial state!
runSCT' :: SCTScheduler s -- ^ The scheduler
        -> s -- ^ The scheduler's initial satte
        -> g -- ^ The runner's initial state
        -> (s -> g -> Bool) -- ^ Termination decider
        -> (s -> g -> SCTTrace -> (s, g)) -- ^ State step function
        -> (forall t. Conc t a) -- ^ Conc program
        -> [(Maybe a, SCTTrace)]
runSCT' sched s g term step c
  | term s g = []
  | otherwise = (res, trace) : rest where

  (res, (s', strace), ttrace) = runConc' sched (s, [(Start 0, [])]) c

  trace = scttrace strace ttrace

  (s'', g') = step s' g trace

  rest = runSCT' sched s'' g' term step c

-- | A variant of runSCT' for concurrent programs that do IO.
--
-- Warning! The IO will be executed lots of times, in lots of
-- interleavings! Be very confident that nothing in a 'liftIO' can
-- block on the action of another thread, or you risk deadlocking this
-- function!
runSCTIO' :: SCTScheduler s -> s -> g -> (s -> g -> Bool) -> (s -> g -> SCTTrace -> (s, g)) -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
runSCTIO' sched s g term step c
  | term s g = return []
  | otherwise = do
    (res, (s', strace), ttrace) <- CIO.runConc' sched (s, [(Start 0, [])]) c

    let trace = scttrace strace ttrace
    let (s'', g') = step s' g trace

    rest <- runSCTIO' sched s'' g' term step c

    return $ (res, trace) : rest

-- * Random Schedulers

-- | A simple pre-emptive random scheduler.
sctRandom :: RandomGen g => SCTScheduler g
sctRandom = toSCT randomSched

-- | A random scheduler with no pre-emption.
sctRandomNP :: RandomGen g => SCTScheduler g
sctRandomNP = toSCT randomSchedNP

-- * Pre-emption bounding

data PreBoundState = P
  { _pc :: Int
  -- ^ Current pre-emption count.
  , _next :: [[Decision]]
  -- ^ Schedules to try in this pc.
  , _done :: [SCTTrace]
  -- ^ Schedules completed in this pc.
  , _halt :: Bool
  -- ^ Indicates more schedules couldn't be found, and to halt
  -- immediately.
  }

-- | An SCT runner using a pre-emption bounding scheduler. Schedules
-- will be explored systematically, starting with all
-- pre-emption-count zero schedules, and gradually adding more
-- pre-emptions.
sctPreBound :: Int -- ^ The pre-emption bound. Anything < 0 will be
                  -- interpreted as 0.
            -> (forall t. Conc t a) -> [(Maybe a, SCTTrace)]
sctPreBound pb = runSCT' pbSched s g (pbTerm pb') (pbStep pb') where
  s = ([], [], [])
  g = P { _pc = 0, _next = [], _done = [], _halt = False }
  pb' = if pb < 0 then 0 else pb

-- | Variant of 'sctPreBound' using 'IO'. See usual caveats about IO.
sctPreBoundIO :: Int -> (forall t. CIO.Conc t a) -> IO [(Maybe a, SCTTrace)]
sctPreBoundIO pb = runSCTIO' pbSched s g (pbTerm pb') (pbStep pb') where
  s = ([], [], [])
  g = P { _pc = 0, _next = [], _done = [], _halt = False }
  pb' = if pb < 0 then 0 else pb

-- | Pre-emption bounding scheduler, which uses a queue of scheduling
-- decisions to drive the initial trace, returning the generated
-- suffix.
pbSched :: SCTScheduler ([Decision], SchedTrace, SchedTrace)
pbSched ((d, pref, suff), trc) prior threads@(next:_) = case d of
  -- If we have a decision queued, make it.
  (Start t:ds)    -> let trc' = [(Start t,    alters t)]     in (t,     ((ds, pref ++ trc', []), trc ++ trc'))
  (Continue:ds)   -> let trc' = [(Continue,   alters prior)] in (prior, ((ds, pref ++ trc', []), trc ++ trc'))
  (SwitchTo t:ds) -> let trc' = [(SwitchTo t, alters t)]     in (t,     ((ds, pref ++ trc', []), trc ++ trc'))

  -- Otherwise just use a non-pre-emptive scheduler.
  [] | prior `elem` threads -> let trc' = [(Continue, alters prior)] in (prior, (([], pref, suff ++ trc'), trc ++ trc'))
     | otherwise            -> let trc' = [(Continue, alters next)]  in (next,  (([], pref, suff ++ trc'), trc ++ trc'))

  where
    alters tid
      | tid == prior          = map SwitchTo $ filter (/=prior) threads
      | prior `elem` threads = Continue : map SwitchTo (filter (\t -> t /= prior && t /= tid) threads)
      | otherwise            = map Start $ filter (/=tid) threads

-- | Pre-emption bounding termination function: terminates on attempt
-- to start a PB above the limit.
pbTerm :: Int -> a -> PreBoundState -> Bool
pbTerm pb _ g = (_pc g == pb + 1) || _halt g

-- | Pre-emption bounding state step function: computes remaining
-- schedules to try and chooses one.
pbStep :: Int -> (a, SchedTrace, SchedTrace) -> PreBoundState -> SCTTrace -> (([Decision], SchedTrace, SchedTrace), PreBoundState)
pbStep pb (_, pref, suff) g t = case _next g of
  -- We have schedules remaining in this PB, so run the next
  (x:xs) -> (s' x, g { _next = xs ++ thisPB, _done = done' })

  -- We have no schedules remaining, try to generate some more.
  --
  -- If there are no more schedules in this PB, and this isn't the
  -- last PB, advance to the next.
  --
  -- If there are no schedules in the next PB, halt.
  [] ->
    case thisPB of
      (x:xs) -> (s' x, g { _next = xs, _done = done' })
      [] -> if _pc g == pb
           then halt
           else case nextPB of
                  (x:xs) -> (s' x, g { _pc = pc', _next = xs, _done = [] })
                  [] -> halt

  where
    halt  = (([], [], []), g { _halt = True })
    done' = t : _done g
    pc'   = _pc g + 1

    s' ds = (tail ds, [], [])

    thisPB = [ map fst pref ++ y | y <- others suff ]
    nextPB = [ y | y <- ordNub $ concatMap next done', preEmpCount y == pc' ]

    -- | Return all modifications to this schedule which do not
    -- introduce extra pre-emptions.
    others ((Start    i, alts):ds) = [Start    i : o | o <- others ds] ++ [[a] | a <- alts]
    others ((SwitchTo i, alts):ds) = [SwitchTo i : o | o <- others ds] ++ [[a] | a <- alts]
    others ((d, _):ds) = [d : o | o <- others ds]
    others [] = []

    -- | Return all modifications to this schedule which do introduce
    -- an extra pre-emption. Only introduce pre-emptions around CVar
    -- actions.
    next ((Continue, alts, Put _):ds)       = [Continue : n | n <- next ds] ++ [[n] | n <- alts]
    next ((Continue, alts, BlockedPut):ds)  = [Continue : n | n <- next ds] ++ [[n] | n <- alts]
    next ((Continue, alts, TryPut _ _):ds)  = [Continue : n | n <- next ds] ++ [[n] | n <- alts]
    next ((Continue, alts, Read):ds)        = [Continue : n | n <- next ds] ++ [[n] | n <- alts]
    next ((Continue, alts, BlockedRead):ds) = [Continue : n | n <- next ds] ++ [[n] | n <- alts]
    next ((Continue, alts, Take _):ds)      = [Continue : n | n <- next ds] ++ [[n] | n <- alts]
    next ((Continue, alts, BlockedTake):ds) = [Continue : n | n <- next ds] ++ [[n] | n <- alts]
    next ((Continue, alts, TryTake _ _):ds) = [Continue : n | n <- next ds] ++ [[n] | n <- alts]
    next ((d, _, _):ds) = [d : n | n <- next ds]
    next [] = []

-- | Check the pre-emption count of some scheduling decisions.
preEmpCount :: [Decision] -> Int
preEmpCount (SwitchTo _:ss) = 1 + preEmpCount ss
preEmpCount (_:ss) = preEmpCount ss
preEmpCount [] = 0

-- * Utils

-- | Convert a 'Scheduler' to an 'SCTScheduler' by recording the
-- trace.
toSCT :: Scheduler s -> SCTScheduler s
toSCT sched (s, trace) prior threads = (tid, (s', trace ++ [(decision, alters)])) where
  (tid, s') = sched s prior threads

  decision | tid == prior         = Continue
           | prior `elem` threads = SwitchTo tid
           | otherwise            = Start tid

  alters | tid == prior         = map SwitchTo $ filter (/=prior) threads
         | prior `elem` threads = Continue : map SwitchTo (filter (\t -> t /= prior && t /= tid) threads)
         | otherwise            = map Start $ filter (/=tid) threads

-- | Pretty-print a scheduler trace.
showTrace :: SchedTrace -> String
showTrace = trace "" 0 . map fst where
    trace prefix num (Start tid:ds)    = thread prefix num ++ trace ("S" ++ show tid) 1 ds
    trace prefix num (SwitchTo tid:ds) = thread prefix num ++ trace ("P" ++ show tid) 1 ds
    trace prefix num (Continue:ds)     = trace prefix (num + 1) ds
    trace prefix num []                = thread prefix num

    thread prefix num = prefix ++ replicate num '-'

-- | Zip a list of 'SchedTrace's and a 'Trace' together into an
-- 'SCTTrace'.
scttrace :: SchedTrace -> Trace -> SCTTrace
scttrace = zipWith $ \(d, alts) (_, act) -> (d, alts, act)

-- | O(nlogn) nub, <https://github.com/nh2/haskell-ordnub>
ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty where
  go _ [] = []
  go s (x:xs)
    | x `Set.member` s = go s xs
    | otherwise = x : go (Set.insert x s) xs

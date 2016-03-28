-- | Dynamic partial-order reduction.
module Test.DejaFu.DPOR where

import Control.DeepSeq (NFData(..))
import Data.List (foldl')

-- | Scheduling decisions are based on the state of the running
-- program, and so we can capture some of that state in recording what
-- specific decision we made.
data Decision thread_id =
    Start thread_id
  -- ^ Start a new thread, because the last was blocked (or it's the
  -- start of computation).
  | Continue
  -- ^ Continue running the last thread for another step.
  | SwitchTo thread_id
  -- ^ Pre-empt the running thread, and switch to another.
  deriving (Eq, Show)

instance NFData thread_id => NFData (Decision thread_id) where
  rnf (Start    tid) = rnf tid
  rnf (SwitchTo tid) = rnf tid
  rnf d = d `seq` ()

-- | Get the resultant thread identifier of a 'Decision', with a default case
-- for 'Continue'.
tidOf ::
    t
  -- ^ The @Continue@ case.
  -> Decision t
  -- ^ The decision.
  -> t
tidOf _ (Start t)    = t
tidOf _ (SwitchTo t) = t
tidOf tid _          = tid

-- | Get the 'Decision' that would have resulted in this thread identifier,
-- given a prior thread (if any) and list of runnable threads.
decisionOf :: (Eq thread_id, Foldable f)
  => Maybe thread_id
  -- ^ The prior thread.
  -> f thread_id
  -- ^ The runnable threads.
  -> thread_id
  -- ^ The current thread.
  -> Decision thread_id
decisionOf Nothing _ chosen = Start chosen
decisionOf (Just prior) runnable chosen
  | prior == chosen = Continue
  | prior `elem` runnable = SwitchTo chosen
  | otherwise = Start chosen

-- | Get the tid of the currently active thread after executing a
-- series of decisions. The list MUST begin with a 'Start', if it
-- doesn't an error will be thrown.
activeTid ::
    [Decision thread_id]
  -- ^ The sequence of decisions that have been made.
  -> thread_id
activeTid (Start tid:ds) = foldl' tidOf tid ds
activeTid _ = error "activeTid: first decision MUST be a 'Start'."

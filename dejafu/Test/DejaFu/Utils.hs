-- |
-- Module      : Test.DejaFu.Utils
-- Copyright   : (c) 2017--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Utility functions for users of dejafu.
module Test.DejaFu.Utils where

import           Control.Exception (Exception(..), displayException)
import           Data.List         (intercalate, minimumBy)
import           Data.Maybe        (mapMaybe)
import           Data.Ord          (comparing)

import           Test.DejaFu.Types

-------------------------------------------------------------------------------
-- * Traces

-- | Turn a 'Trace' into an abbreviated form.
--
-- @since 1.3.2.0
toTIdTrace :: Trace -> [(ThreadId, ThreadAction)]
toTIdTrace =
  tail . scanl (\(t, _) (d, _, a) -> (tidOf t d, a)) (initialThread, undefined)

-- | Pretty-print a trace, including a key of the thread IDs (not
-- including thread 0). Each line of the key is indented by two
-- spaces.
--
-- @since 0.5.0.0
showTrace :: Trace -> String
showTrace []  = "<trace discarded>"
showTrace trc = intercalate "\n" $ go False trc : strkey where
  go _ ((_,_,CommitCRef _ _):rest) = "C-" ++ go False rest
  go _ ((Start    (ThreadId (Id _ i)),_,a):rest) = "S" ++ show i ++ "-" ++ go (didYield a) rest
  go y ((SwitchTo (ThreadId (Id _ i)),_,a):rest) = (if y then "p" else "P") ++ show i ++ "-" ++ go (didYield a) rest
  go _ ((Continue,_,a):rest) = '-' : go (didYield a) rest
  go _ _ = ""

  strkey =
    ["  " ++ show i ++ ": " ++ name | (i, name) <- threadNames trc]

  didYield Yield = True
  didYield (ThreadDelay _) = True
  didYield _ = False

-- | Get all named threads in the trace.
--
-- @since 0.7.3.0
threadNames :: Trace -> [(Int, String)]
threadNames = mapMaybe go where
  go (_, _, Fork   (ThreadId (Id (Just name) i))) = Just (i, name)
  go (_, _, ForkOS (ThreadId (Id (Just name) i))) = Just (i, name)
  go _ = Nothing

-- | Find the \"simplest\" trace leading to each result.
simplestsBy :: (x -> x -> Bool) -> [(x, Trace)] -> [(x, Trace)]
simplestsBy f = map choose . collect where
  collect = groupBy' [] (\(a,_) (b,_) -> f a b)
  choose  = minimumBy . comparing $ \(_, trc) ->
    let switchTos = length . filter (\(d,_,_) -> case d of SwitchTo _ -> True; _ -> False)
        starts    = length . filter (\(d,_,_) -> case d of Start    _ -> True; _ -> False)
        commits   = length . filter (\(_,_,a) -> case a of CommitCRef _ _ -> True; _ -> False)
    in (switchTos trc, commits trc, length trc, starts trc)

  groupBy' res _ [] = res
  groupBy' res eq (y:ys) = groupBy' (insert' eq y res) eq ys

  insert' _ x [] = [[x]]
  insert' eq x (ys@(y:_):yss)
    | x `eq` y  = (x:ys) : yss
    | otherwise = ys : insert' eq x yss
  insert' _ _ ([]:_) = undefined

-------------------------------------------------------------------------------
-- * Failures

-- | Pretty-print a failure
--
-- @since 0.4.0.0
showFail :: Failure -> String
showFail Abort = "[abort]"
showFail Deadlock = "[deadlock]"
showFail STMDeadlock = "[stm-deadlock]"
showFail InternalError = "[internal-error]"
showFail (UncaughtException exc) = "[" ++ displayException exc ++ "]"
showFail IllegalSubconcurrency = "[illegal-subconcurrency]"
showFail IllegalDontCheck = "[illegal-dontcheck]"

-------------------------------------------------------------------------------
-- * Scheduling

-- | Get the resultant thread identifier of a 'Decision', with a default case
-- for 'Continue'.
--
-- @since 0.5.0.0
tidOf :: ThreadId -> Decision -> ThreadId
tidOf _ (Start t)    = t
tidOf _ (SwitchTo t) = t
tidOf tid _          = tid

-- | Get the 'Decision' that would have resulted in this thread
-- identifier, given a prior thread (if any) and collection of threads
-- which are unblocked at this point.
--
-- @since 0.5.0.0
decisionOf :: Foldable f
  => Maybe ThreadId
  -- ^ The prior thread.
  -> f ThreadId
  -- ^ The threads.
  -> ThreadId
  -- ^ The current thread.
  -> Decision
decisionOf Nothing _ chosen = Start chosen
decisionOf (Just prior) runnable chosen
  | prior == chosen = Continue
  | prior `elem` runnable = SwitchTo chosen
  | otherwise = Start chosen

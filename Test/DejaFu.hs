{-# LANGUAGE Rank2Types #-}

-- | Useful functions for writing SCT test cases for @Conc@
-- computations.
module Test.DejaFu
  ( doTests
  , doTests'
  , autocheck
  , autocheckIO
  -- * Test cases
  , Result(..)
  , runTest
  , runTestIO
  , runTest'
  , runTestIO'
  -- * Predicates
  , Predicate
  , deadlocksNever
  , deadlocksAlways
  , deadlocksSometimes
  , alwaysSame
  , alwaysTrue
  , alwaysTrue2
  , somewhereTrue
  , somewhereTrue2
  -- * Utilities
  , pAnd
  , pNot
  , rForgetful
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.DeepSeq (NFData(..))
import Control.Monad (when, void)
import Data.Function (on)
import Data.List (foldl')
import Data.List.Extra
import Data.Maybe (mapMaybe, isJust, isNothing)
import Test.DejaFu.Deterministic
import Test.DejaFu.SCT.Internal
import Test.DejaFu.SCT.Bounding
import Test.DejaFu.Shrink

import qualified Test.DejaFu.Deterministic.IO as CIO

-- * Test suites

-- | Run a collection of tests (with a pb of 2), printing results to
-- stdout, and returning 'True' iff all tests pass.
doTests :: Show a
        => Bool
        -- ^ Whether to print test passes.
        -> [(String, Result a)]
        -- ^ The test cases
        -> IO Bool
doTests = doTests' show

-- | Variant of 'doTests' which takes a result printing function.
doTests' :: (a -> String) -> Bool -> [(String, Result a)] -> IO Bool
doTests' showf verbose tests = do
  results <- mapM (doTest showf verbose) tests
  return $ and results

-- | Automatically test a computation. In particular, look for
-- deadlocks and multiple return values.
autocheck :: (Eq a, Show a) => (forall t. Conc t a) -> IO Bool
autocheck t = doTests True cases where
  cases = [ ("Never Deadlocks",   runTest deadlocksNever t)
          , ("Consistent Result", runTest alwaysSame     t)
          ]

-- | Automatically test an 'IO' computation. In particular, look for
-- deadlocks and multiple return values. See usual caveats about 'IO'.
autocheckIO :: (Eq a, Show a) => (forall t. CIO.Conc t a) -> IO Bool
autocheckIO t = do
  dead <- runTestIO deadlocksNever t
  same <- runTestIO alwaysSame     t
  doTests True [ ("Never Deadlocks",   dead)
               , ("Consistent Result", same)
              ]

-- | Run a test and print to stdout
doTest :: (a -> String) -> Bool -> (String, Result a) -> IO Bool
doTest showf verbose (name, result) = do
  if _pass result
  then
    -- If verbose, display a pass message.
    when verbose $
      putStrLn $ "\27[32m[pass]\27[0m " ++ name ++ " (checked: " ++ show (_casesChecked result) ++ ")"
  else do
    -- Display a failure message, and the first 5 (simplified) failed traces
    putStrLn ("\27[31m[fail]\27[0m " ++ name ++ " (checked: " ++ show (_casesChecked result) ++ ")")
    let traces = let (rs, ts) = unzip . take 5 $ _failures result in rs `zip` simplify ts
    mapM_ (\(r, t) -> putStrLn $ "\t" ++ maybe "[deadlock]" showf r ++ " " ++ showtrc t) traces
    when (moreThan (_failures result) 5) $
      putStrLn "\t..."

  return $ _pass result

-- | Simplify a collection of traces, by attempting to factor out
-- common prefixes and suffixes.
simplify :: [SCTTrace] -> [(SCTTrace, SCTTrace, SCTTrace)]
simplify [t] = [([], t, [])]
simplify ts = map (\t -> (pref, drop plen $ take (length t - slen) t, suff)) ts where
  pref = commonPrefix ts
  plen = length pref
  suff = commonSuffix ts
  slen = length suff

-- | Pretty-print a simplified trace
showtrc :: (SCTTrace, SCTTrace, SCTTrace) -> String
showtrc (p, t, s) = case (p, s) of
  ([], []) -> hilight ++ showtrc' t ++ reset
  ([], _)  -> hilight ++ showtrc' t ++ reset ++ s'
  (_, [])  -> p' ++ hilight ++ showtrc' t ++ reset
  (_, _)   -> p' ++ hilight ++ showtrc' t ++ reset ++ s'

  where
    showtrc' = showTrace . map (\(d,as,_) -> (d,as))
    hilight = "\27[33m"
    reset   = "\27[0m"
    p' = (if length p > 50 then ("..." ++) . reverse . take 50 . reverse else id) $ showtrc' p
    s' = (if length s > 50 then (++ "...") . take 50 else id) $ showtrc' s

-- * Test cases

-- | The results of a test, including information on the number of
-- cases checked, and number of total cases. Be careful if using the
-- total number of cases, as that value may be very big, and (due to
-- laziness) will actually force a lot more computation!.
data Result a = Result
  { _pass         :: Bool
  -- ^ Whether the test passed or not.
  , _casesChecked :: Int
  -- ^ The number of cases checked.
  , _casesTotal   :: Int
  -- ^ The total number of cases.
  , _failures :: [(Maybe a, SCTTrace)]
  -- ^ The failed cases, if any.
  } deriving (Show, Eq)

instance NFData a => NFData (Result a) where
  rnf r = rnf (_pass r, _casesChecked r, _casesTotal r, _failures r)

instance Functor Result where
  fmap f r = r { _failures = map (first $ fmap f) $ _failures r }

-- | Run a test using the pre-emption bounding scheduler, with a bound
-- of 2, and attempt to shrink any failing traces.
runTest :: Eq a => Predicate a -> (forall t. Conc t a) -> Result a
runTest = runTest' 2

-- | Variant of 'runTest' using 'IO'. See usual caveats about 'IO'.
runTestIO :: Eq a => Predicate a -> (forall t. CIO.Conc t a) -> IO (Result a)
runTestIO = runTestIO' 2

-- | Run a test using the pre-emption bounding scheduler, and attempt
-- to shrink any failing traces.
runTest' :: Eq a => Int -> Predicate a -> (forall t. Conc t a) -> Result a
runTest' pb predicate conc = andShrink . predicate $ sctPreBound pb conc where
  andShrink r
    | null $ _failures r = r
    | otherwise = r { _failures = uniques . map (\failure@(res, _) -> (res, shrink failure conc)) $ _failures r }

-- | Variant of 'runTest'' using 'IO'. See usual caveats about 'IO'.
runTestIO' :: Eq a => Int -> Predicate a -> (forall t. CIO.Conc t a) -> IO (Result  a)
runTestIO' pb predicate conc = (predicate <$> sctPreBoundIO pb conc) >>= andShrink where
  andShrink r
    | null $ _failures r = return r
    | otherwise = (\fs -> r { _failures = uniques fs }) <$>
      mapM (\failure@(res, _) -> (\trc' -> (res, trc')) <$> shrinkIO failure conc) (_failures r)

-- | Find unique failures and return the simplest trace for each
-- failure.
uniques :: Eq a => [(Maybe a, SCTTrace)] -> [(Maybe a, SCTTrace)]
uniques = mapMaybe (foldl' simplest' Nothing) . groupByIsh ((==) `on` fst) where
  simplest' Nothing r = Just r
  simplest' r@(Just (_, trc)) s@(_, trc')
    | simplest [trc, trc'] == Just trc = r
    | otherwise = Just s

-- * Predicates

-- | A @Predicate@ is a function which collapses a list of results
-- into a 'Result'.
type Predicate a = [(Maybe a, SCTTrace)] -> Result a

-- | Check that a computation never deadlocks.
deadlocksNever :: Predicate a
deadlocksNever = alwaysTrue isJust

-- | Check that a computation always deadlocks.
deadlocksAlways :: Predicate a
deadlocksAlways = alwaysTrue isNothing

-- | Check that a computation deadlocks at least once.
deadlocksSometimes :: Predicate a
deadlocksSometimes = somewhereTrue isNothing

-- | Check that the result of a computation is always the same. In
-- particular this means either: (a) it always deadlocks, or (b) the
-- result is always 'Just' @x@, for some fixed @x@.
alwaysSame :: Eq a => Predicate a
alwaysSame = alwaysTrue2 (==)

-- | Check that the result of a unary boolean predicate is always
-- true. An empty list of results counts as 'True'.
alwaysTrue :: (Maybe a -> Bool) -> Predicate a
alwaysTrue p xs = go xs Result { _pass = True, _casesChecked = 0, _casesTotal = len, _failures = failures } where
  go [] res = res
  go ((y,_):ys) res
    | p y = go ys $ incCC res
    | otherwise = incCC res { _pass = False }

  (len, failures) = findFailures1 p xs

-- | Check that the result of a binary boolean predicate is always
-- true between adjacent pairs of results. An empty list of results
-- counts as 'True'.
--
-- If the predicate fails, *both* (result,trace) tuples will be added
-- to the failures list.
alwaysTrue2 :: (Maybe a -> Maybe a -> Bool) -> Predicate a
alwaysTrue2 _ [_] = Result { _pass = True, _casesChecked = 1, _casesTotal = 1, _failures = [] }
alwaysTrue2 p xs  = go xs Result { _pass = True, _casesChecked = 0, _casesTotal = len, _failures = failures } where
  go [] = id
  go [(y1,_),(y2,_)]    = check y1 y2 []
  go ((y1,_):(y2,t):ys) = check y1 y2 ((y2,t) : ys)

  check y1 y2 ys res
    | p y1 y2   = go ys $ incCC res
    | otherwise = incCC res { _pass = False }

  (len, failures) = findFailures2 p xs

-- | Check that the result of a unary boolean predicate is true at
-- least once. An empty list of results counts as 'False'.
somewhereTrue :: (Maybe a -> Bool) -> Predicate a
somewhereTrue p xs = go xs Result { _pass = False, _casesChecked = 0, _casesTotal = len, _failures = failures } where
  go [] res = res
  go ((y,_):ys) res
    | p y = incCC res { _pass = True }
    | otherwise = go ys $ incCC res

  (len, failures) = findFailures1 p xs

-- | Check that the result of a binary boolean predicate is true
-- between at least one adjacent pair of results. An empty list of
-- results counts as 'False'.
--
-- If the predicate fails, *both* (result,trace) tuples will be added
-- to the failures list.
somewhereTrue2 :: (Maybe a -> Maybe a -> Bool) -> Predicate a
somewhereTrue2 _ [x] = Result { _pass = False, _casesChecked = 1, _casesTotal = 1, _failures = [x] }
somewhereTrue2 p xs  = go xs Result { _pass = False, _casesChecked = 0, _casesTotal = len, _failures = failures } where
  go [] = id
  go [(y1,_),(y2,_)]    = check y1 y2 []
  go ((y1,_):(y2,t):ys) = check y1 y2 ((y2,t) : ys)

  check y1 y2 ys res
    | p y1 y2   = incCC res { _pass = True }
    | otherwise = go ys $ incCC res

  (len, failures) = findFailures2 p xs

-- * Utils

-- | Compose two predicates sequentially.
pAnd :: Predicate a -> Predicate a -> Predicate a
pAnd p q xs = if _pass r1 then r2 else r1 where
  r1 = p xs
  r2 = q xs

-- | Invert the result of a predicate.
pNot :: Predicate a -> Predicate a
pNot p xs = r { _pass = not $ _pass r } where
  r = p xs

-- | Throw away the failures information in a Result (useful for
-- storing them in a list).
rForgetful :: Result a -> Result ()
rForgetful = void

-- | Increment the cases checked
incCC :: Result a -> Result a
incCC r = r { _casesChecked = _casesChecked r + 1 }

-- | Get the length of the list and find the failing cases in one
-- traversal.
findFailures1 :: (Maybe a -> Bool) -> [(Maybe a, SCTTrace)] -> (Int, [(Maybe a, SCTTrace)])
findFailures1 p xs = findFailures xs 0 [] where
  findFailures [] l fs = (l, fs)
  findFailures ((z,t):zs) l fs
    | p z = findFailures zs (l+1) fs
    | otherwise = findFailures zs (l+1) ((z,t):fs)

-- | Get the length of the list and find the failing cases in one
-- traversal.
findFailures2 :: (Maybe a -> Maybe a -> Bool) -> [(Maybe a, SCTTrace)] -> (Int, [(Maybe a, SCTTrace)])
findFailures2 p xs = findFailures xs 0 [] where
  findFailures [] l fs = (l, fs)
  findFailures [_] l fs = (l+1, fs)
  findFailures ((z1,t1):(z2,t2):zs) l fs
    | p z1 z2 = findFailures ((z2,t2):zs) (l+1) fs
    | otherwise = findFailures ((z2,t2):zs) (l+1) ((z1,t1):(z2,t2):fs)

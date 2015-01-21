{-# LANGUAGE Rank2Types #-}

-- | Useful functions for writing SCT test cases for @Conc@
-- computations.
module Control.Monad.Conc.SCT.Tests
  ( -- * Test cases
    Result(..)
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
  ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.Monad.Conc.Fixed
import Control.Monad.Conc.SCT.PreBound
import Data.Maybe (isJust, isNothing)

import qualified Control.Monad.Conc.Fixed.IO as CIO

-- * Test cases

-- | The results of a test, including information on the number of
-- cases checked, and number of total cases. Be careful if using the
-- total number of cases, as that value may be very big, and (due to
-- laziness) will actually force a lot more computation!.
data Result = Result
  { _pass         :: Bool
  -- ^ Whether the test passed or not.
  , _casesChecked :: Int
  -- ^ The number of cases checked.
  , _casesTotal   :: Int
  -- ^ The total number of cases.
  }

instance NFData Result where
  rnf r = rnf (_pass r, _casesChecked r, _casesTotal r)

-- | Run a test using the pre-emption bounding scheduler, with a bound
-- of 2.
runTest :: Predicate a -> (forall t. Conc t a) -> Result
runTest = runTest' 2

-- | Variant of 'runTest' using 'IO'. See usual caveats about 'IO'.
runTestIO :: Predicate a -> (forall t. CIO.Conc t a) -> IO Result
runTestIO = runTestIO' 2

-- | Run a test using the pre-emption bounding scheduler.
runTest' :: Int -> Predicate a -> (forall t. Conc t a) -> Result
runTest' pb predicate conc = predicate . map fst $ sctPreBound pb conc

-- | Variant of 'runTest'' using 'IO'. See usual caveats about 'IO'.
runTestIO' :: Int -> Predicate a -> (forall t. CIO.Conc t a) -> IO Result
runTestIO' pb predicate conc = predicate . map fst <$> sctPreBoundIO pb conc

-- * Predicates

-- | A @Predicate@ is a function which collapses a list of results
-- into a 'Result'.
type Predicate a = [Maybe a] -> Result

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
alwaysTrue p = pNot $ somewhereTrue (not . p)

-- | Check that the result of a binary boolean predicate is always
-- true between adjacent pairs of results. An empty list of results
-- counts as 'True'.
alwaysTrue2 :: (Maybe a -> Maybe a -> Bool) -> Predicate a
alwaysTrue2 p = pNot $ somewhereTrue2 (\a b -> not $ p a b)

-- | Check that the result of a unary boolean predicate is true at
-- least once. An empty list of results counts as 'False'.
somewhereTrue :: (Maybe a -> Bool) -> Predicate a
somewhereTrue p xs = go xs Result { _pass = False, _casesChecked = 0, _casesTotal = length xs } where
  go [] res = res
  go (y:ys) res
    | p y = incCC res { _pass = True }
    | otherwise = go ys $ incCC res

-- | Check that the result of a binary boolean predicate is true
-- between at least one adjacent pair of results. An empty list of
-- results counts as 'False'.
somewhereTrue2 :: (Maybe a -> Maybe a -> Bool) -> Predicate a
somewhereTrue2 _ [_] = Result { _pass = False, _casesChecked = 1, _casesTotal = 1 }
somewhereTrue2 p xs  = go xs Result { _pass = False, _casesChecked = 0, _casesTotal = length xs } where
  go []         = id
  go [y1,y2]    = check y1 y2 []
  go (y1:y2:ys) = check y1 y2 (y2 : ys)

  check y1 y2 ys res
    | p y1 y2   = incCC res { _pass = True }
    | otherwise = go ys $ incCC res

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

-- | Increment the cases checked
incCC :: Result -> Result
incCC r = r { _casesChecked = _casesChecked r + 1 }

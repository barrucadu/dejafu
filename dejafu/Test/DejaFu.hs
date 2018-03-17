{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Test.DejaFu
Copyright   : (c) 2015--2018 Michael Walker
License     : MIT
Maintainer  : Michael Walker <mike@barrucadu.co.uk>
Stability   : experimental
Portability : LambdaCase, MultiParamTypeClasses, TupleSections

dejafu is a library for unit-testing concurrent Haskell programs,
written using the [concurrency](https://hackage.haskell.org/package/concurrency)
package's 'MonadConc' typeclass.

__A first test:__ This is a simple concurrent program which forks two
threads and each races to write to the same @MVar@:

>>> :{
let example = do
      var <- newEmptyMVar
      fork (putMVar var "hello")
      fork (putMVar var "world")
      readMVar var
:}

We can test it with dejafu like so:

>>> autocheck example
[pass] Never Deadlocks
[pass] No Exceptions
[fail] Consistent Result
    "hello" S0----S1--S0--
<BLANKLINE>
    "world" S0----S2--S0--
False

The 'autocheck' function takes a concurrent program to test and looks
for some common unwanted behaviours: deadlocks, uncaught exceptions in
the main thread, and nondeterminism.  Here we see the program is
nondeterministic, dejafu gives us all the distinct results it found
and, for each, a summarised execution trace leading to that result:

 * \"Sn\" means that thread \"n\" started executing after the previous
   thread terminated or blocked.

 * \"Pn\" means that thread \"n\" started executing, even though the
   previous thread could have continued running.

 * Each \"-\" represents one \"step\" of the computation.

__Failures:__ If a program doesn't terminate successfully, we say it
has /failed/.  dejafu can detect a few different types of failure:

 * 'Deadlock', if every thread is blocked.

 * 'STMDeadlock', if every thread is blocked /and/ the main thread is
   blocked in an STM transaction.

 * 'UncaughtException', if the main thread is killed by an exception.

There are two types of failure which dejafu itself may raise:

 * 'Abort', used in systematic testing (the default) if there are no
   allowed decisions remaining.  For example, by default any test case
   which takes more than 250 scheduling points to finish will be
   aborted.  You can use the 'systematically' function to supply (or
   disable) your own bounds.

 * 'InternalError', used if something goes wrong.  If you get this and
   aren't using a scheduler you wrote yourself, please [file a
   bug](https://github.com/barrucadu/dejafu/issues).

Finally, there are two failures which can arise through improper use of
dejafu:

 * 'IllegalDontCheck', the "Test.DejaFu.Conc.dontCheck" function is
   used as anything other than the fist action in the main thread.

 * 'IllegalSubconcurrency', the "Test.DejaFu.Conc.subconcurrency"
   function is used when multiple threads exist, or is used inside
   another @subconcurrency@ call.

__Beware of 'liftIO':__ dejafu works by running your test case lots of
times with different schedules.  If you use 'liftIO' at all, make sure
that any @IO@ you perform is deterministic when executed in the same
order.

If you need to test things with /nondeterministc/ @IO@, see the
'autocheckWay', 'dejafuWay', and 'dejafusWay' functions: the
'randomly', 'uniformly', and 'swarmy' testing modes can cope with
nondeterminism.
-}
module Test.DejaFu
  ( -- * Unit testing

    autocheck
  , dejafu
  , dejafus

  -- ** Configuration

  {- |

There are a few knobs to tweak to control the behaviour of dejafu.
The defaults should generally be good enough, but if not you have a
few tricks available.  The main two are: the 'Way', which controls how
schedules are explored; and the 'MemType', which controls how reads
and writes to @CRef@s behave; see "Test.DejaFu.Settings" for a
complete listing.

-}

  , autocheckWay
  , dejafuWay
  , dejafusWay
  , autocheckWithSettings
  , dejafuWithSettings
  , dejafusWithSettings

  , module Test.DejaFu.Settings

  -- ** Manual testing

  {- |

The standard testing functions print their result to stdout, and throw
away some information.  The traces are pretty-printed, and if there
are many failures, only the first few are shown.

If you need more information, use these functions.

-}

  , Result(..)
  , Failure(..)
  , runTest
  , runTestWay

  -- ** Predicates

  {- |

A dejafu test has two parts: the concurrent program to test, and a
predicate to determine if the test passes, based on the results of the
schedule exploration.

All of these predicates discard results and traces as eagerly as
possible, to reduce memory usage.

-}

  , Predicate
  , ProPredicate(..)
  , abortsNever
  , abortsAlways
  , abortsSometimes
  , deadlocksNever
  , deadlocksAlways
  , deadlocksSometimes
  , exceptionsNever
  , exceptionsAlways
  , exceptionsSometimes

  -- *** Helpers

  {- |

Helper functions to produce your own predicates.  Such predicates
discard results and traces as eagerly as possible, to reduce memory
usage.

-}

  , representative
  , alwaysSame
  , alwaysSameOn
  , alwaysSameBy
  , notAlwaysSame
  , alwaysTrue
  , somewhereTrue
  , alwaysNothing
  , somewhereNothing
  , gives
  , gives'

  -- *** Failures

  {- |

Helper functions to identify failures.

-}

  , isInternalError
  , isAbort
  , isDeadlock
  , isUncaughtException
  , isIllegalSubconcurrency
  , isIllegalDontCheck

  -- * Property testing

  {- |

dejafu can also use a property-testing style to test stateful
operations for a variety of inputs.  Inputs are generated using the
[leancheck](https://hackage.haskell.org/package/leancheck) library for
enumerative testing.

__Testing @MVar@ operations with multiple producers__:

These are a little different to the property tests you may be familiar
with from libraries like QuickCheck (and leancheck).  As we're testing
properties of /stateful/ and /concurrent/ things, we need to provide
some extra information.

A property consists of two /signatures/ and a relation between them.
A signature contains:

 * An initialisation function, to construct the initial state.

 * An observation function, to take a snapshot of the state at the
   end.

 * An interference function, to mess with the state in some way.

 * The expression to evaluate, as a function over the state.

>>> import Control.Monad (void)
>>> :{
let sig e = Sig
      { initialise = maybe newEmptyMVar newMVar
      , observe    = \v _ -> tryReadMVar v
      , interfere  = \v _ -> putMVar v 42
      , expression = void . e
      }
:}

This is a signature for operations over @Num n => MVar n@ values where
there are multiple producers.  The initialisation function takes a
@Maybe n@ and constructs an @MVar n@, empty if it gets @Nothing@; the
observation function reads the @MVar@; and the interference function
puts a new value in.

Given this signature, we can check if @readMVar@ is the same as a
@takeMVar@ followed by a @putMVar@:

>>> check $ sig readMVar === sig (\v -> takeMVar v >>= putMVar v)
*** Failure: (seed Just 0)
    left:  [(Nothing,Just 0)]
    right: [(Nothing,Just 0),(Just Deadlock,Just 42)]
False

The two expressions are not equivalent, and we get a counterexample:
if the @MVar@ is nonempty, then the left expression (@readMVar@) will
preserve the value, but the right expression (@\v -> takeMVar v >>=
putMVar v@) may cause it to change.  This is because of the concurrent
interference we have provided: the left term never empties a full
@MVar@, but the Right term does.

-}

  , module Test.DejaFu.Refinement

  -- * Deprecated
  , dejafuDiscard
  ) where

import           Control.Arrow            (first)
import           Control.DeepSeq          (NFData(..))
import           Control.Monad            (unless, when)
import           Control.Monad.Conc.Class (MonadConc)
import           Control.Monad.IO.Class   (MonadIO(..))
import           Data.Function            (on)
import           Data.List                (intercalate, intersperse)
import           Data.Maybe               (catMaybes, isJust, isNothing,
                                           mapMaybe)
import           Data.Profunctor          (Profunctor(..))
import           System.Environment       (lookupEnv)

import           Test.DejaFu.Conc
import           Test.DejaFu.Refinement
import           Test.DejaFu.SCT
import           Test.DejaFu.Settings
import           Test.DejaFu.Types
import           Test.DejaFu.Utils

{- $setup

>>> import Control.Concurrent.Classy hiding (check)

>>> :{
let example = do
      var <- newEmptyMVar
      fork (putMVar var "hello")
      fork (putMVar var "world")
      readMVar var
:}

>>> :{
let relaxed = do
      r1 <- newCRef False
      r2 <- newCRef False
      x <- spawn $ writeCRef r1 True >> readCRef r2
      y <- spawn $ writeCRef r2 True >> readCRef r1
      (,) <$> readMVar x <*> readMVar y
:}

-}

-------------------------------------------------------------------------------
-- DejaFu

-- | Automatically test a computation.
--
-- In particular, look for deadlocks, uncaught exceptions, and
-- multiple return values.  Returns @True@ if all tests pass
--
-- >>> autocheck example
-- [pass] Never Deadlocks
-- [pass] No Exceptions
-- [fail] Consistent Result
--     "hello" S0----S1--S0--
-- <BLANKLINE>
--     "world" S0----S2--S0--
-- False
--
-- @since 1.0.0.0
autocheck :: (MonadConc n, MonadIO n, Eq a, Show a)
  => ConcT n a
  -- ^ The computation to test.
  -> n Bool
autocheck = autocheckWithSettings defaultSettings

-- | Variant of 'autocheck' which takes a way to run the program and a
-- memory model.
--
-- >>> autocheckWay defaultWay defaultMemType relaxed
-- [pass] Never Deadlocks
-- [pass] No Exceptions
-- [fail] Consistent Result
--     (False,True) S0---------S1----S0--S2----S0--
-- <BLANKLINE>
--     (False,False) S0---------S1--P2----S1--S0---
-- <BLANKLINE>
--     (True,False) S0---------S2----S1----S0---
-- <BLANKLINE>
--     (True,True) S0---------S1-C-S2----S1---S0---
-- False
--
-- >>> autocheckWay defaultWay SequentialConsistency relaxed
-- [pass] Never Deadlocks
-- [pass] No Exceptions
-- [fail] Consistent Result
--     (False,True) S0---------S1----S0--S2----S0--
-- <BLANKLINE>
--     (True,True) S0---------S1-P2----S1---S0---
-- <BLANKLINE>
--     (True,False) S0---------S2----S1----S0---
-- False
--
-- @since 1.0.0.0
autocheckWay :: (MonadConc n, MonadIO n, Eq a, Show a)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> ConcT n a
  -- ^ The computation to test.
  -> n Bool
autocheckWay way = autocheckWithSettings . fromWayAndMemType way

-- | Variant of 'autocheck' which takes a settings record.
--
-- >>> autocheckWithSettings (fromWayAndMemType defaultWay defaultMemType) relaxed
-- [pass] Never Deadlocks
-- [pass] No Exceptions
-- [fail] Consistent Result
--     (False,True) S0---------S1----S0--S2----S0--
-- <BLANKLINE>
--     (False,False) S0---------S1--P2----S1--S0---
-- <BLANKLINE>
--     (True,False) S0---------S2----S1----S0---
-- <BLANKLINE>
--     (True,True) S0---------S1-C-S2----S1---S0---
-- False
--
-- >>> autocheckWithSettings (fromWayAndMemType defaultWay SequentialConsistency) relaxed
-- [pass] Never Deadlocks
-- [pass] No Exceptions
-- [fail] Consistent Result
--     (False,True) S0---------S1----S0--S2----S0--
-- <BLANKLINE>
--     (True,True) S0---------S1-P2----S1---S0---
-- <BLANKLINE>
--     (True,False) S0---------S2----S1----S0---
-- False
--
-- @since 1.2.0.0
autocheckWithSettings :: (MonadConc n, MonadIO n, Eq a, Show a)
  => Settings n a
  -- ^ The SCT settings.
  -> ConcT n a
  -- ^ The computation to test.
  -> n Bool
autocheckWithSettings settings = dejafusWithSettings settings
  [ ("Never Deadlocks",   representative deadlocksNever)
  , ("No Exceptions",     representative exceptionsNever)
  , ("Consistent Result", alwaysSame) -- already representative
  ]

-- | Check a predicate and print the result to stdout, return 'True'
-- if it passes.
--
-- A dejafu test has two parts: the program you are testing, and a
-- predicate to determine if the test passes.  Predicates can look for
-- anything, including checking for some expected nondeterminism.
--
-- >>> dejafu "Test Name" alwaysSame example
-- [fail] Test Name
--     "hello" S0----S1--S0--
-- <BLANKLINE>
--     "world" S0----S2--S0--
-- False
--
-- @since 1.0.0.0
dejafu :: (MonadConc n, MonadIO n, Show b)
  => String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check.
  -> ConcT n a
  -- ^ The computation to test.
  -> n Bool
dejafu = dejafuWithSettings defaultSettings

-- | Variant of 'dejafu' which takes a way to run the program and a
-- memory model.
--
-- >>> import System.Random
--
-- >>> dejafuWay (randomly (mkStdGen 0) 100) defaultMemType "Randomly!" alwaysSame example
-- [fail] Randomly!
--     "hello" S0----S1--S0--
-- <BLANKLINE>
--     "world" S0----S2--S0--
-- False
--
-- >>> dejafuWay (randomly (mkStdGen 1) 100) defaultMemType "Randomly!" alwaysSame example
-- [fail] Randomly!
--     "hello" S0----S1--S0--
-- <BLANKLINE>
--     "world" S0----S2--S1-S0--
-- False
--
-- @since 1.0.0.0
dejafuWay :: (MonadConc n, MonadIO n, Show b)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check.
  -> ConcT n a
  -- ^ The computation to test.
  -> n Bool
dejafuWay way = dejafuWithSettings . fromWayAndMemType way

-- | Variant of 'dejafu' which takes a settings record.
--
-- >>> import System.Random
--
-- >>> dejafuWithSettings (fromWayAndMemType (randomly (mkStdGen 1) 100) defaultMemType) "Randomly!" alwaysSame example
-- [fail] Randomly!
--     "hello" S0----S1--S0--
-- <BLANKLINE>
--     "world" S0----S2--S1-S0--
-- False
--
-- @since 1.2.0.0
dejafuWithSettings :: (MonadConc n, MonadIO n, Show b)
  => Settings n a
  -- ^ The SCT settings.
  -> String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check.
  -> ConcT n a
  -- ^ The computation to test.
  -> n Bool
dejafuWithSettings settings name test =
  dejafusWithSettings settings [(name, test)]

-- | Variant of 'dejafuWay' which can selectively discard results.
--
-- >>> dejafuDiscard (\_ -> Just DiscardTrace) defaultWay defaultMemType "Discarding" alwaysSame example
-- [fail] Discarding
--     "hello" <trace discarded>
-- <BLANKLINE>
--     "world" <trace discarded>
-- False
--
-- @since 1.0.0.0
dejafuDiscard :: (MonadConc n, MonadIO n, Show b)
  => (Either Failure a -> Maybe Discard)
  -- ^ Selectively discard results.
  -> Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check.
  -> ConcT n a
  -- ^ The computation to test.
  -> n Bool
dejafuDiscard discard way =
  dejafuWithSettings . set ldiscard (Just discard) . fromWayAndMemType way
{-# DEPRECATED dejafuDiscard "Use dejafuWithSettings instead" #-}

-- | Variant of 'dejafu' which takes a collection of predicates to
-- test, returning 'True' if all pass.
--
-- >>> dejafus [("A", alwaysSame), ("B", deadlocksNever)] example
-- [fail] A
--     "hello" S0----S1--S0--
-- <BLANKLINE>
--     "world" S0----S2--S0--
-- [pass] B
-- False
--
-- @since 1.0.0.0
dejafus :: (MonadConc n, MonadIO n, Show b)
  => [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check.
  -> ConcT n a
  -- ^ The computation to test.
  -> n Bool
dejafus = dejafusWithSettings defaultSettings

-- | Variant of 'dejafus' which takes a way to run the program and a
-- memory model.
--
-- >>> dejafusWay defaultWay SequentialConsistency [("A", alwaysSame), ("B", exceptionsNever)] relaxed
-- [fail] A
--     (False,True) S0---------S1----S0--S2----S0--
-- <BLANKLINE>
--     (True,True) S0---------S1-P2----S1---S0---
-- <BLANKLINE>
--     (True,False) S0---------S2----S1----S0---
-- [pass] B
-- False
--
-- @since 1.0.0.0
dejafusWay :: (MonadConc n, MonadIO n, Show b)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check.
  -> ConcT n a
  -- ^ The computation to test.
  -> n Bool
dejafusWay way = dejafusWithSettings . fromWayAndMemType way

-- | Variant of 'dejafus' which takes a settings record.
--
-- >>> dejafusWithSettings (fromWayAndMemType defaultWay SequentialConsistency) [("A", alwaysSame), ("B", exceptionsNever)] relaxed
-- [fail] A
--     (False,True) S0---------S1----S0--S2----S0--
-- <BLANKLINE>
--     (True,True) S0---------S1-P2----S1---S0---
-- <BLANKLINE>
--     (True,False) S0---------S2----S1----S0---
-- [pass] B
-- False
--
-- @since 1.2.0.0
dejafusWithSettings :: (MonadConc n, MonadIO n, Show b)
  => Settings n a
  -- ^ The SCT settings.
  -> [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check.
  -> ConcT n a
  -- ^ The computation to test.
  -> n Bool
dejafusWithSettings settings tests conc = do
    traces  <- runSCTWithSettings (set ldiscard (Just discarder) settings) conc
    results <- mapM (\(name, test) -> liftIO . doTest name $ chk test traces) tests
    pure (and results)
  where
    discarder = maybe id strengthenDiscard (get ldiscard settings) $ foldr
      (weakenDiscard . pdiscard . snd)
      (const (Just DiscardResultAndTrace))
      tests

    -- for evaluating each individual predicate, we only want the
    -- results/traces it would not discard, but the traces set may
    -- include more than this if the different predicates have
    -- different discard functions, so we do another pass of
    -- discarding.
    chk p rs
      | moreThan 1 rs =
        let go r@(efa, _) = case pdiscard p efa of
              Just DiscardResultAndTrace -> Nothing
              Just DiscardTrace -> Just (efa, [])
              Nothing -> Just r
        in peval p (mapMaybe go rs)
      | otherwise = peval p rs

-------------------------------------------------------------------------------
-- Test cases

-- | The results of a test, including the number of cases checked to
-- determine the final boolean outcome.
--
-- @since 1.0.0.0
data Result a = Result
  { _pass :: Bool
  -- ^ Whether the test passed or not.
  , _failures :: [(Either Failure a, Trace)]
  -- ^ The failing cases, if any.
  , _failureMsg :: String
  -- ^ A message to display on failure, if nonempty
  } deriving (Eq, Show)

instance NFData a => NFData (Result a) where
  rnf r = rnf ( _pass r
              , _failures r
              , _failureMsg r
              )

-- | A failed result, taking the given list of failures.
defaultFail :: [(Either Failure a, Trace)] -> Result a
defaultFail failures = Result False failures ""

-- | A passed result.
defaultPass :: Result a
defaultPass = Result True [] ""

instance Functor Result where
  fmap f r = r { _failures = map (first $ fmap f) $ _failures r }

instance Foldable Result where
  foldMap f r = foldMap f [a | (Right a, _) <- _failures r]

-- | Run a predicate over all executions within the default schedule
-- bounds.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.  This may
-- affect which failing traces are reported, when there is a failure.
--
-- @since 1.0.0.0
runTest :: MonadConc n
  => ProPredicate a b
  -- ^ The predicate to check
  -> ConcT n a
  -- ^ The computation to test
  -> n (Result b)
runTest = runTestWithSettings defaultSettings

-- | Variant of 'runTest' which takes a way to run the program and a
-- memory model.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.  This may
-- affect which failing traces are reported, when there is a failure.
--
-- @since 1.0.0.0
runTestWay :: MonadConc n
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> ProPredicate a b
  -- ^ The predicate to check
  -> ConcT n a
  -- ^ The computation to test
  -> n (Result b)
runTestWay way = runTestWithSettings . fromWayAndMemType way

-- | Variant of 'runTest' which takes a settings record.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.  This may
-- affect which failing traces are reported, when there is a failure.
--
-- @since 1.2.0.0
runTestWithSettings :: MonadConc n
  => Settings n a
  -- ^ The SCT settings.
  -> ProPredicate a b
  -- ^ The predicate to check
  -> ConcT n a
  -- ^ The computation to test
  -> n (Result b)
runTestWithSettings settings p conc =
  let discarder = maybe id strengthenDiscard (get ldiscard settings) (pdiscard p)
  in peval p <$> runSCTWithSettings (set ldiscard (Just discarder) settings) conc


-------------------------------------------------------------------------------
-- Predicates

-- | A @Predicate@ is a function which collapses a list of results
-- into a 'Result', possibly discarding some on the way.
--
-- @Predicate@ cannot be a functor as the type parameter is used both
-- co- and contravariantly.
--
-- @since 1.0.0.0
type Predicate a = ProPredicate a a

-- | A @ProPredicate@ is a function which collapses a list of results
-- into a 'Result', possibly discarding some on the way.
--
-- @since 1.0.0.0
data ProPredicate a b = ProPredicate
  { pdiscard :: Either Failure a -> Maybe Discard
  -- ^ Selectively discard results before computing the result.
  , peval :: [(Either Failure a, Trace)] -> Result b
  -- ^ Compute the result with the un-discarded results.
  }

instance Profunctor ProPredicate where
  dimap f g p = ProPredicate
    { pdiscard = pdiscard p . fmap f
    , peval = fmap g . peval p . map (first (fmap f))
    }

instance Functor (ProPredicate x) where
  fmap = dimap id

-- | Reduce the list of failures in a @ProPredicate@ to one
-- representative trace for each unique result.
--
-- This may throw away \"duplicate\" failures which have a unique
-- cause but happen to manifest in the same way. However, it is
-- convenient for filtering out true duplicates.
--
-- @since 1.0.0.0
representative :: Eq b => ProPredicate a b -> ProPredicate a b
representative p = p
  { peval = \xs ->
      let result = peval p xs
      in result { _failures = simplestsBy (==) (_failures result) }
  }

-- | Check that a computation never aborts.
--
-- @since 1.0.0.0
abortsNever :: Predicate a
abortsNever = alwaysTrue (not . either (==Abort) (const False))

-- | Check that a computation always aborts.
--
-- @since 1.0.0.0
abortsAlways :: Predicate a
abortsAlways = alwaysTrue $ either (==Abort) (const False)

-- | Check that a computation aborts at least once.
--
-- @since 1.0.0.0
abortsSometimes :: Predicate a
abortsSometimes = somewhereTrue $ either (==Abort) (const False)

-- | Check that a computation never deadlocks.
--
-- @since 1.0.0.0
deadlocksNever :: Predicate a
deadlocksNever = alwaysTrue (not . either isDeadlock (const False))

-- | Check that a computation always deadlocks.
--
-- @since 1.0.0.0
deadlocksAlways :: Predicate a
deadlocksAlways = alwaysTrue $ either isDeadlock (const False)

-- | Check that a computation deadlocks at least once.
--
-- @since 1.0.0.0
deadlocksSometimes :: Predicate a
deadlocksSometimes = somewhereTrue $ either isDeadlock (const False)

-- | Check that a computation never fails with an uncaught exception.
--
-- @since 1.0.0.0
exceptionsNever :: Predicate a
exceptionsNever = alwaysTrue (not . either isUncaughtException (const False))

-- | Check that a computation always fails with an uncaught exception.
--
-- @since 1.0.0.0
exceptionsAlways :: Predicate a
exceptionsAlways = alwaysTrue $ either isUncaughtException (const False)

-- | Check that a computation fails with an uncaught exception at least once.
--
-- @since 1.0.0.0
exceptionsSometimes :: Predicate a
exceptionsSometimes = somewhereTrue $ either isUncaughtException (const False)

-- | Check that the result of a computation is always the same. In
-- particular this means either: (a) it always fails in the same way,
-- or (b) it never fails and the values returned are all equal.
--
-- > alwaysSame = alwaysSameBy (==)
--
-- @since 1.0.0.0
alwaysSame :: Eq a => Predicate a
alwaysSame = alwaysSameBy (==)

-- | Check that the result of a computation is always the same by
-- comparing the result of a function on every result.
--
-- > alwaysSameOn = alwaysSameBy ((==) `on` f)
--
-- @since 1.0.0.0
alwaysSameOn :: Eq b => (Either Failure a -> b) -> Predicate a
alwaysSameOn f = alwaysSameBy ((==) `on` f)

-- | Check that the result of a computation is always the same, using
-- some transformation on results.
--
-- @since 1.0.0.0
alwaysSameBy :: (Either Failure a -> Either Failure a -> Bool) -> Predicate a
alwaysSameBy f = ProPredicate
  { pdiscard = const Nothing
  , peval = \xs -> case simplestsBy f xs of
      []  -> defaultPass
      [_] -> defaultPass
      xs' -> defaultFail xs'
  }

-- | Check that the result of a computation is not always the same.
--
-- @since 1.0.0.0
notAlwaysSame :: Eq a => Predicate a
notAlwaysSame = ProPredicate
    { pdiscard = const Nothing
    , peval = \case
        [x] -> defaultFail [x]
        xs  -> go xs (defaultFail [])
    }
  where
    go [y1,y2] res
      | fst y1 /= fst y2 = res { _pass = True }
      | otherwise = res { _failures = y1 : y2 : _failures res }
    go (y1:y2:ys) res
      | fst y1 /= fst y2 = go (y2:ys) res { _pass = True }
      | otherwise = go (y2:ys) res { _failures = y1 : y2 : _failures res }
    go _ res = res

-- | Check that a @Maybe@-producing function always returns 'Nothing'.
--
-- @since 1.0.0.0
alwaysNothing :: (Either Failure a -> Maybe (Either Failure b)) -> ProPredicate a b
alwaysNothing f = ProPredicate
  { pdiscard = maybe (Just DiscardResultAndTrace) (const Nothing) . f
  , peval = \xs ->
      let failures = mapMaybe (\(efa,trc) -> (,trc) <$> f efa) xs
      in Result (null failures) failures ""
  }

-- | Check that the result of a unary boolean predicate is always
-- true.
--
-- @since 1.0.0.0
alwaysTrue :: (Either Failure a -> Bool) -> Predicate a
alwaysTrue p = alwaysNothing (\efa -> if p efa then Nothing else Just efa)

-- | Check that a @Maybe@-producing function returns 'Nothing' at
-- least once.
--
-- @since 1.0.0.0
somewhereNothing :: (Either Failure a -> Maybe (Either Failure b)) -> ProPredicate a b
somewhereNothing f = ProPredicate
  { pdiscard = maybe (Just DiscardTrace) (const Nothing) . f
  , peval = \xs ->
      let failures = map (\(efa,trc) -> (,trc) <$> f efa) xs
      in Result (any isNothing failures) (catMaybes failures) ""
  }

-- | Check that the result of a unary boolean predicate is true at
-- least once.
--
-- @since 1.0.0.0
somewhereTrue :: (Either Failure a -> Bool) -> Predicate a
somewhereTrue p = somewhereNothing (\efa -> if p efa then Nothing else Just efa)

-- | Predicate for when there is a known set of results where every
-- result must be exhibited at least once.
--
-- @since 1.0.0.0
gives :: (Eq a, Show a) => [Either Failure a] -> Predicate a
gives expected = ProPredicate
    { pdiscard = \efa -> if efa `elem` expected then Just DiscardTrace else Nothing
    , peval = \xs -> go expected [] xs $ defaultFail (failures xs)
    }
  where
    go waitingFor alreadySeen ((x, _):xs) res
      -- If it's a result we're waiting for, move it to the
      -- @alreadySeen@ list and continue.
      | x `elem` waitingFor  = go (filter (/=x) waitingFor) (x:alreadySeen) xs res
      -- If it's a result we've already seen, continue.
      | x `elem` alreadySeen = go waitingFor alreadySeen xs res
      -- If it's not a result we expected, fail.
      | otherwise = res

    go [] _ [] res = res { _pass = True }
    go es _ [] res = res { _failureMsg = unlines $ map (\e -> "Expected: " ++ show e) es }

    failures = filter (\(r, _) -> r `notElem` expected)

-- | Variant of 'gives' that doesn't allow for expected failures.
--
-- @since 1.0.0.0
gives' :: (Eq a, Show a) => [a] -> Predicate a
gives' = gives . map Right


-------------------------------------------------------------------------------
-- Utils

-- | Run a test and print to stdout
doTest :: Show a => String -> Result a -> IO Bool
doTest name result = do
    doctest <- isJust <$> lookupEnv "DEJAFU_DOCTEST"
    if _pass result
    then putStrLn (passmsg doctest)
    else do
      -- Display a failure message, and the first 5 (simplified) failed traces
      putStrLn (failmsg doctest)

      unless (null $ _failureMsg result) $
        putStrLn $ _failureMsg result

      let failures = _failures result
      let output = map (\(r, t) -> putStrLn . indent $ either showFail show r ++ " " ++ showTrace t) $ take 5 failures
      sequence_ $ intersperse (putStrLn "") output
      when (moreThan 5 failures) $
        putStrLn (indent "...")

    pure (_pass result)
  where
    passmsg True = "[pass] " ++ name
    passmsg False = "\27[32m[pass]\27[0m " ++ name

    failmsg True = "[fail] " ++ name
    failmsg False = "\27[31m[fail]\27[0m " ++ name

-- | Check if a list is longer than some value, without needing to
-- compute the entire length.
moreThan :: Int -> [a] -> Bool
moreThan n [] = n < 0
moreThan 0 _ = True
moreThan n (_:rest) = moreThan (n-1) rest

-- | Indent every line of a string.
indent :: String -> String
indent = intercalate "\n" . map ("    "++) . lines

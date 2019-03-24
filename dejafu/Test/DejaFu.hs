{-# LANGUAGE TupleSections #-}

{- |
Module      : Test.DejaFu
Copyright   : (c) 2015--2019 Michael Walker
License     : MIT
Maintainer  : Michael Walker <mike@barrucadu.co.uk>
Stability   : experimental
Portability : TupleSections

dejafu is a library for unit-testing concurrent Haskell programs which
are written using the <https://hackage.haskell.org/package/concurrency
concurrency> package's 'MonadConc' typeclass.

For more in-depth documentation, including migration guides from
earlier versions of dejafu, see the <https://dejafu.readthedocs.io
website>.

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
[pass] Successful
[fail] Deterministic
    "hello" S0----S1--S0--
<BLANKLINE>
    "world" S0----S2--S0--
False

The 'autocheck' function takes a concurrent program to test and looks
for concurrency errors and nondeterminism.  Here we see the program is
nondeterministic, dejafu gives us all the distinct results it found
and, for each, a summarised execution trace leading to that result:

 * \"Sn\" means that thread \"n\" started executing after the previous
   thread terminated or blocked.

 * \"Pn\" means that thread \"n\" started executing, even though the
   previous thread could have continued running.

 * Each \"-\" represents one \"step\" of the computation.

__Memory models:__ dejafu supports three different memory models,
which affect how one thread's 'IORef' updates become visible to other
threads.

 * Sequential consistency: a program behaves as a simple interleaving
   of the actions in different threads. When an 'IORef' is written to,
   that write is immediately visible to all threads.

  * Total store order (TSO): each thread has a write buffer.  A thread
    sees its writes immediately, but other threads will only see
    writes when they are committed, which may happen later.  Writes
    are committed in the same order that they are created.

  * Partial store order (PSO): each 'IORef' has a write buffer.  A
    thread sees its writes immediately, but other threads will only
    see writes when they are committed, which may happen later.
    Writes to different 'IORef's are not necessarily committed in the
    same order that they are created.

This small example shows the difference between sequential consistency
and TSO:

>>> :{
let relaxed = do
      r1 <- newIORef False
      r2 <- newIORef False
      x <- spawn $ writeIORef r1 True >> readIORef r2
      y <- spawn $ writeIORef r2 True >> readIORef r1
      (,) <$> readMVar x <*> readMVar y
:}

The 'autocheckWay' function will let us specify the memory model:

>>> autocheckWay defaultWay SequentialConsistency relaxed
[pass] Successful
[fail] Deterministic
    (False,True) S0---------S1----S0--S2----S0--
<BLANKLINE>
    (True,True) S0---------S1-P2----S1---S0---
<BLANKLINE>
    (True,False) S0---------S2----S1----S0---
False

>>> autocheckWay defaultWay TotalStoreOrder relaxed
[pass] Successful
[fail] Deterministic
    (False,True) S0---------S1----S0--S2----S0--
<BLANKLINE>
    (False,False) S0---------S1--P2----S1--S0---
<BLANKLINE>
    (True,False) S0---------S2----S1----S0---
<BLANKLINE>
    (True,True) S0---------S1-C-S2----S1---S0---
False

The result @(False,False)@ is possible using TSO and PSO, but not
sequential consistency.  The \"C\" in the trace shows where a /commit/
action occurred, which makes a write to an 'IORef' visible to all
threads.

__Beware of 'liftIO':__ dejafu works by running your test case lots of
times with different schedules.  If you use 'liftIO' at all, make sure
that any @IO@ you perform is deterministic when executed in the same
order.

If you need to test things with /nondeterministc/ @IO@, see the
'autocheckWay', 'dejafuWay', and 'dejafusWay' functions: the
'randomly' and 'uniformly' testing modes can cope with nondeterminism.
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
and writes to @IORef@s behave; see "Test.DejaFu.Settings" for a
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
  , runTest
  , runTestWay
  , runTestWithSettings

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
  , successful
  , alwaysSame
  , notAlwaysSame
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
  , alwaysSameOn
  , alwaysSameBy
  , notAlwaysSameOn
  , notAlwaysSameBy
  , alwaysTrue
  , somewhereTrue
  , alwaysNothing
  , somewhereNothing
  , gives
  , gives'

  -- *** Conditions

  {- |

Helper functions to identify conditions.

-}

  , Condition(..)
  , isAbort
  , isDeadlock
  , isUncaughtException
  , isInvariantFailure

  -- * Property-based testing

  {- |

dejafu can also use a property-based testing style to test stateful
operations for a variety of inputs.  Inputs are generated using the
<https://hackage.haskell.org/package/leancheck leancheck> library for
enumerative testing.

__Testing @MVar@ operations with multiple producers__: These are a
little different to the property tests you may be familiar with from
libraries like QuickCheck (and leancheck).  As we're testing
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

  -- * Expressing concurrent programs
  , Program
  , Basic
  , ConcT
  , ConcIO

  -- ** Setup and teardown
  , WithSetup
  , WithSetupAndTeardown
  , withSetup
  , withTeardown
  , withSetupAndTeardown

  -- ** Invariants
  , Invariant
  , registerInvariant
  , inspectIORef
  , inspectMVar
  , inspectTVar
) where

import           Control.Arrow          (first)
import           Control.DeepSeq        (NFData(..))
import           Control.Monad          (unless, when)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Either            (isLeft)
import           Data.Function          (on)
import           Data.List              (intercalate, intersperse, partition)
import           Data.Maybe             (catMaybes, isJust, isNothing, mapMaybe)
import           Data.Profunctor        (Profunctor(..))
import           System.Environment     (lookupEnv)

import           Test.DejaFu.Conc
import           Test.DejaFu.Internal
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
      r1 <- newIORef False
      r2 <- newIORef False
      x <- spawn $ writeIORef r1 True >> readIORef r2
      y <- spawn $ writeIORef r2 True >> readIORef r1
      (,) <$> readMVar x <*> readMVar y
:}

-}

-------------------------------------------------------------------------------
-- DejaFu

-- | Automatically test a computation.
--
-- In particular, concurrency errors and nondeterminism.  Returns
-- @True@ if all tests pass
--
-- >>> autocheck example
-- [pass] Successful
-- [fail] Deterministic
--     "hello" S0----S1--S0--
-- <BLANKLINE>
--     "world" S0----S2--S0--
-- False
--
-- @since 2.1.0.0
autocheck :: (MonadDejaFu n, MonadIO n, Eq a, Show a)
  => Program pty n a
  -- ^ The computation to test.
  -> n Bool
autocheck = autocheckWithSettings defaultSettings

-- | Variant of 'autocheck' which takes a way to run the program and a
-- memory model.
--
-- >>> autocheckWay defaultWay defaultMemType relaxed
-- [pass] Successful
-- [fail] Deterministic
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
-- [pass] Successful
-- [fail] Deterministic
--     (False,True) S0---------S1----S0--S2----S0--
-- <BLANKLINE>
--     (True,True) S0---------S1-P2----S1---S0---
-- <BLANKLINE>
--     (True,False) S0---------S2----S1----S0---
-- False
--
-- @since 2.1.0.0
autocheckWay :: (MonadDejaFu n, MonadIO n, Eq a, Show a)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @IORef@ operations.
  -> Program pty n a
  -- ^ The computation to test.
  -> n Bool
autocheckWay way = autocheckWithSettings . fromWayAndMemType way

-- | Variant of 'autocheck' which takes a settings record.
--
-- >>> autocheckWithSettings (fromWayAndMemType defaultWay defaultMemType) relaxed
-- [pass] Successful
-- [fail] Deterministic
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
-- [pass] Successful
-- [fail] Deterministic
--     (False,True) S0---------S1----S0--S2----S0--
-- <BLANKLINE>
--     (True,True) S0---------S1-P2----S1---S0---
-- <BLANKLINE>
--     (True,False) S0---------S2----S1----S0---
-- False
--
-- @since 2.1.0.0
autocheckWithSettings :: (MonadDejaFu n, MonadIO n, Eq a, Show a)
  => Settings n a
  -- ^ The SCT settings.
  -> Program pty n a
  -- ^ The computation to test.
  -> n Bool
autocheckWithSettings settings = dejafusWithSettings settings
  [ ("Successful", representative successful)
  , ("Deterministic", representative alwaysSame)
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
-- @since 2.1.0.0
dejafu :: (MonadDejaFu n, MonadIO n, Show b)
  => String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check.
  -> Program pty n a
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
-- @since 2.1.0.0
dejafuWay :: (MonadDejaFu n, MonadIO n, Show b)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @IORef@ operations.
  -> String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check.
  -> Program pty n a
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
-- @since 2.1.0.0
dejafuWithSettings :: (MonadDejaFu n, MonadIO n, Show b)
  => Settings n a
  -- ^ The SCT settings.
  -> String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check.
  -> Program pty n a
  -- ^ The computation to test.
  -> n Bool
dejafuWithSettings settings name test =
  dejafusWithSettings settings [(name, test)]

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
-- @since 2.1.0.0
dejafus :: (MonadDejaFu n, MonadIO n, Show b)
  => [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check.
  -> Program pty n a
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
-- @since 2.1.0.0
dejafusWay :: (MonadDejaFu n, MonadIO n, Show b)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @IORef@ operations.
  -> [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check.
  -> Program pty n a
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
-- @since 2.1.0.0
dejafusWithSettings :: (MonadDejaFu n, MonadIO n, Show b)
  => Settings n a
  -- ^ The SCT settings.
  -> [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check.
  -> Program pty n a
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
  , _failures :: [(Either Condition a, Trace)]
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
defaultFail :: [(Either Condition a, Trace)] -> Result a
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
-- @since 2.1.0.0
runTest :: MonadDejaFu n
  => ProPredicate a b
  -- ^ The predicate to check
  -> Program pty n a
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
-- @since 2.1.0.0
runTestWay :: MonadDejaFu n
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @IORef@ operations.
  -> ProPredicate a b
  -- ^ The predicate to check
  -> Program pty n a
  -- ^ The computation to test
  -> n (Result b)
runTestWay way = runTestWithSettings . fromWayAndMemType way

-- | Variant of 'runTest' which takes a settings record.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.  This may
-- affect which failing traces are reported, when there is a failure.
--
-- @since 2.1.0.0
runTestWithSettings :: MonadDejaFu n
  => Settings n a
  -- ^ The SCT settings.
  -> ProPredicate a b
  -- ^ The predicate to check
  -> Program pty n a
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
  { pdiscard :: Either Condition a -> Maybe Discard
  -- ^ Selectively discard results before computing the result.
  , peval :: [(Either Condition a, Trace)] -> Result b
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

-- | Check that a computation never produces a @Left@ value.
--
-- @since 1.9.1.0
successful :: Predicate a
successful = alwaysTrue (either (const False) (const True))

-- | Check that a computation never aborts.
--
-- Any result other than an abort, including other 'Condition's, is
-- allowed.
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
-- Any result other than an abort, including other 'Condition's, is
-- allowed.
--
-- @since 1.0.0.0
abortsSometimes :: Predicate a
abortsSometimes = somewhereTrue $ either (==Abort) (const False)

-- | Check that a computation never deadlocks.
--
-- Any result other than a deadlock, including other 'Condition's, is
-- allowed.
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
-- Any result other than a deadlock, including other 'Condition's, is
-- allowed.
--
-- @since 1.0.0.0
deadlocksSometimes :: Predicate a
deadlocksSometimes = somewhereTrue $ either isDeadlock (const False)

-- | Check that a computation never fails with an uncaught exception.
--
-- Any result other than an uncaught exception, including other
-- 'Condition's, is allowed.
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
-- Any result other than an uncaught exception, including other
-- 'Condition's, is allowed.
--
-- @since 1.0.0.0
exceptionsSometimes :: Predicate a
exceptionsSometimes = somewhereTrue $ either isUncaughtException (const False)

-- | Check that a computation always gives the same, @Right@, result.
--
-- > alwaysSame = alwaysSameBy (==)
--
-- @since 1.10.0.0
alwaysSame :: Eq a => Predicate a
alwaysSame = alwaysSameBy (==)

-- | Check that a computation always gives the same (according to the
-- provided function), @Right@, result.
--
-- > alwaysSameOn = alwaysSameBy ((==) `on` f)
--
-- @since 1.10.0.0
alwaysSameOn :: Eq b => (a -> b) -> Predicate a
alwaysSameOn f = alwaysSameBy ((==) `on` f)

-- | Check that the result of a computation is always the same, using
-- some transformation on results.
--
-- @since 1.10.0.0
alwaysSameBy :: (a -> a -> Bool) -> Predicate a
alwaysSameBy f = ProPredicate
  { pdiscard = const Nothing
  , peval = \xs ->
      let (failures, successes) = partition (isLeft . fst) xs
          simpleSuccesses = simplestsBy (f `on` efromRight) successes
          simpleFailures  = simplestsBy ((==) `on` efromLeft) failures
      in case (simpleFailures, simpleSuccesses) of
        ([], []) -> defaultPass
        ([], [_]) -> defaultPass
        (_, _) -> defaultFail (simpleFailures ++ simpleSuccesses)
  }

-- | Check that a computation never fails, and gives multiple distinct
-- @Right@ results.
--
-- > notAlwaysSame = notAlwaysSameBy (==)
--
-- @since 1.10.0.0
notAlwaysSame :: Eq a => Predicate a
notAlwaysSame = notAlwaysSameBy (==)

-- | Check that a computation never fails, and gives multiple distinct
-- (according to the provided function) @Right@ results.
--
-- > notAlwaysSameOn = notAlwaysSameBy ((==) `on` f)
--
-- @since 1.10.0.0
notAlwaysSameOn :: Eq b => (a -> b) -> Predicate a
notAlwaysSameOn f = notAlwaysSameBy ((==) `on` f)

-- | Check that a computation never fails, and gives multiple distinct
-- @Right@ results, by applying a transformation on results.
--
-- This inverts the condition, so (eg) @notAlwaysSameBy (==)@ will
-- pass if there are unequal results.
--
-- @since 1.10.0.0
notAlwaysSameBy :: (a -> a -> Bool) -> Predicate a
notAlwaysSameBy f = ProPredicate
    { pdiscard = const Nothing
    , peval = \xs ->
        let (failures, successes) = partition (isLeft . fst) xs
            simpleFailures = simplestsBy ((==) `on` efromLeft) failures
        in case successes of
          [x] -> defaultFail (x : simpleFailures)
          _  ->
            let res = go successes (defaultFail [])
            in case failures of
              [] -> res
              _ -> res { _failures = simpleFailures ++ _failures res, _pass = False }
    }
  where
    y1 .*. y2 = not (on f (efromRight . fst) y1 y2)

    go [y1,y2] res
      | y1 .*. y2 = res { _pass = True }
      | otherwise = res { _failures = y1 : y2 : _failures res }
    go (y1:y2:ys) res
      | y1 .*. y2 = go (y2:ys) res { _pass = True }
      | otherwise = go (y2:ys) res { _failures = y1 : y2 : _failures res }
    go _ res = res

-- | Check that a @Maybe@-producing function always returns 'Nothing'.
--
-- @since 1.0.0.0
alwaysNothing :: (Either Condition a -> Maybe (Either Condition b)) -> ProPredicate a b
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
alwaysTrue :: (Either Condition a -> Bool) -> Predicate a
alwaysTrue p = alwaysNothing (\efa -> if p efa then Nothing else Just efa)

-- | Check that a @Maybe@-producing function returns 'Nothing' at
-- least once.
--
-- @since 1.0.0.0
somewhereNothing :: (Either Condition a -> Maybe (Either Condition b)) -> ProPredicate a b
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
somewhereTrue :: (Either Condition a -> Bool) -> Predicate a
somewhereTrue p = somewhereNothing (\efa -> if p efa then Nothing else Just efa)

-- | Predicate for when there is a known set of results where every
-- result must be exhibited at least once.
--
-- @since 1.0.0.0
gives :: (Eq a, Show a) => [Either Condition a] -> Predicate a
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

-- | Variant of 'gives' that doesn't allow for @Left@ results.
--
-- > gives' = gives . map Right
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
      let output = map (\(r, t) -> putStrLn . indent $ either showCondition show r ++ " " ++ showTrace t) $ take 5 failures
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

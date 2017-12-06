{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Test.DejaFu
Copyright   : (c) 2016 Michael Walker
License     : MIT
Maintainer  : Michael Walker <mike@barrucadu.co.uk>
Stability   : experimental
Portability : LambdaCase, MultiParamTypeClasses, TupleSections

dejafu is a library for unit-testing concurrent Haskell programs,
written using the [concurrency](https://hackage.haskell.org/package/concurrency)
package's 'MonadConc' typeclass.

__A first test:__ This is a simple concurrent program which forks two
threads and each races to write to the same @MVar@:

> example = do
>   var <- newEmptyMVar
>   fork (putMVar var "hello")
>   fork (putMVar var "world")
>   readMVar var

We can test it with dejafu like so:

> > autocheck example
> [pass] Never Deadlocks
> [pass] No Exceptions
> [fail] Consistent Result
>         "hello" S0----S1--S0--
>
>         "world" S0----S2--S0--

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

Finally, there is one failure which can arise through improper use of
dejafu:

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
few tricks available.

 * The 'Way', which controls how schedules are explored.

 * The 'MemType', which controls how reads and writes to @CRef@s (or
   @IORef@s) behave.

 * The 'Discard' function, which saves memory by throwing away
   uninteresting results during exploration.

-}

  , autocheckWay
  , dejafuWay
  , dejafusWay
  , dejafuDiscard

  -- *** Defaults

  , defaultWay
  , defaultMemType
  , defaultDiscarder

  -- *** Exploration

  , Way
  , systematically
  , randomly
  , uniformly
  , swarmy

  -- **** Schedule bounding

  {- |

Schedule bounding is used by the 'systematically' approach to limit
the search-space, which in general will be huge.

There are three types of bounds used:

 * The 'PreemptionBound', which bounds the number of pre-emptive
   context switches.  Empirical evidence suggests @2@ is a good value
   for this, if you have a small test case.

 * The 'FairBound', which bounds the difference between how many times
   threads can yield.  This is necessary to test certain kinds of
   potentially non-terminating behaviour, such as spinlocks.

 * The 'LengthBound', which bounds how long a test case can run, in
   terms of scheduling decisions.  This is necessary to test certain
   kinds of potentially non-terminating behaviour, such as livelocks.

Schedule bounding is not used by the non-systematic exploration
behaviours.

-}

  , Bounds(..)
  , defaultBounds
  , noBounds
  , PreemptionBound(..)
  , defaultPreemptionBound
  , FairBound(..)
  , defaultFairBound
  , LengthBound(..)
  , defaultLengthBound

  -- *** Memory model

  {- |

When executed on a multi-core processor some @CRef@ / @IORef@ programs
can exhibit \"relaxed memory\" behaviours, where the apparent
behaviour of the program is not a simple interleaving of the actions
of each thread.

__Example:__ This is a simple program which creates two @CRef@s
containing @False@, and forks two threads.  Each thread writes @True@
to one of the @CRef@s and reads the other.  The value that each thread
reads is communicated back through an @MVar@:

> relaxed = do
>   r1 <- newCRef False
>   r2 <- newCRef False
>   x <- spawn $ writeCRef r1 True >> readCRef r2
>   y <- spawn $ writeCRef r2 True >> readCRef r1
>   (,) <$> readMVar x <*> readMVar y

We see something surprising if we ask for the results:

> > autocheck relaxed
> [pass] Never Deadlocks
> [pass] No Exceptions
> [fail] Consistent Result
>         (False,True) S0---------S1----S0--S2----S0--
>
>         (False,False) S0---------S1--P2----S1--S0---
>
>         (True,False) S0---------S2----S1----S0---
>
>         (True,True) S0---------S1-C-S2----S1---S0---

It's possible for both threads to read the value @False@, even though
each writes @True@ to the other @CRef@ before reading.  This is
because processors are free to re-order reads and writes to
independent memory addresses in the name of performance.

Execution traces for relaxed memory computations can include \"C\"
actions, as above, which show where @CRef@ writes were explicitly
/committed/, and made visible to other threads.

However, modelling this behaviour can require more executions.  If you
do not care about the relaxed-memory behaviour of your program, use
the 'SequentialConsistency' model.

-}

  , MemType(..)

  -- *** Reducing memory usage

  {- |

Sometimes we know that a result is uninteresting and cannot affect the
result of a test, in which case there is no point in keeping it
around.  Execution traces can be large, so any opportunity to get rid
of them early is possibly a great saving of memory.

A discard function, which has type @Either Failure a -> Maybe
Discard@, can selectively discard results or execution traces before
the schedule exploration finishes, allowing them to be garbage
collected sooner.

__Note:__ This is only relevant if you are producing your own
predicates.  The predicates and helper functions provided by this
module do discard results and traces wherever possible.

-}

  , Discard(..)

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

> sig e = Sig
>  { initialise = maybe newEmptyMVar newMVar
>  , observe    = \v _ -> tryReadMVar v
>  , interfere  = \v _ -> putMVar v 42
>  , expression = void . e
>  }

This is a signature for operations over @Num n => MVar n@ values where
there are multiple producers.  The initialisation function takes a
@Maybe n@ and constructs an @MVar n@, empty if it gets @Nothing@; the
observation function reads the @MVar@; and the interference function
puts a new value in.

Given this signature, we can check if @readMVar@ is the same as a
@takeMVar@ followed by a @putMVar@:

> > check $ sig readMVar === sig (\v -> takeMVar v >>= putMVar v)
> *** Failure: (seed Just 0)
>     left:  [(Nothing,Just 0)]
>     right: [(Nothing,Just 0),(Just Deadlock,Just 42)]

The two expressions are not equivalent, and we get a counterexample:
if the @MVar@ is nonempty, then the left expression (@readMVar@) will
preserve the value, but the right expression (@\v -> takeMVar v >>=
putMVar v@) may cause it to change.  This is because of the concurrent
interference we have provided: the left term never empties a full
@MVar@, but the Right term does.

-}

  , module Test.DejaFu.Refinement
  ) where

import           Control.Arrow            (first)
import           Control.DeepSeq          (NFData(..))
import           Control.Monad            (unless, when)
import           Control.Monad.Conc.Class (MonadConc)
import           Control.Monad.IO.Class   (MonadIO(..))
import           Control.Monad.Ref        (MonadRef)
import           Data.Function            (on)
import           Data.List                (intercalate, intersperse)
import           Data.Maybe               (catMaybes, isNothing, mapMaybe)
import           Data.Profunctor          (Profunctor(..))

import           Test.DejaFu.Conc
import           Test.DejaFu.Defaults
import           Test.DejaFu.Refinement
import           Test.DejaFu.SCT
import           Test.DejaFu.Types
import           Test.DejaFu.Utils


-------------------------------------------------------------------------------
-- DejaFu

-- | Automatically test a computation.
--
-- In particular, look for deadlocks, uncaught exceptions, and
-- multiple return values.  Returns @True@ if all tests pass
--
-- > > autocheck example
-- > [pass] Never Deadlocks
-- > [pass] No Exceptions
-- > [fail] Consistent Result
-- >         "hello" S0----S1--S0--
-- >
-- >         "world" S0----S2--S0--
--
-- @since 1.0.0.0
autocheck :: (MonadConc n, MonadIO n, MonadRef r n, Eq a, Show a)
  => ConcT r n a
  -- ^ The computation to test.
  -> n Bool
autocheck = autocheckWay defaultWay defaultMemType

-- | Variant of 'autocheck' which takes a way to run the program and a
-- memory model.
--
-- > > autocheckWay defaultWay defaultMemType relaxed
-- > [pass] Never Deadlocks
-- > [pass] No Exceptions
-- > [fail] Consistent Result
-- >         (False,True) S0---------S1----S0--S2----S0--
-- >
-- >         (False,False) S0---------S1--P2----S1--S0---
-- >
-- >         (True,False) S0---------S2----S1----S0---
-- >
-- >         (True,True) S0---------S1-C-S2----S1---S0---
-- >
-- > > autocheckWay defaultWay SequentialConsistency relaxed
-- > [pass] Never Deadlocks
-- > [pass] No Exceptions
-- > [fail] Consistent Result
-- >         (False,True) S0---------S1----S0--S2----S0--
-- >
-- >         (True,True) S0---------S1-P2----S1---S0---
-- >
-- >         (True,False) S0---------S2----S1----S0---
--
-- @since 1.0.0.0
autocheckWay :: (MonadConc n, MonadIO n, MonadRef r n, Eq a, Show a)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> ConcT r n a
  -- ^ The computation to test.
  -> n Bool
autocheckWay way memtype = dejafusWay way memtype autocheckCases

-- | Predicates for the various autocheck functions.
autocheckCases :: Eq a => [(String, Predicate a)]
autocheckCases =
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
-- > > dejafu "Test Name" alwaysSame example
-- > [fail] Test Name
-- >         "hello" S0----S1--S0--
-- >
-- >         "world" S0----S2--S0--
--
-- @since 1.0.0.0
dejafu :: (MonadConc n, MonadIO n, MonadRef r n, Show b)
  => String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check.
  -> ConcT r n a
  -- ^ The computation to test.
  -> n Bool
dejafu = dejafuWay defaultWay defaultMemType

-- | Variant of 'dejafu' which takes a way to run the program and a
-- memory model.
--
-- > > import System.Random
-- >
-- > > dejafuWay (randomly (mkStdGen 0) 100) defaultMemType "Randomly!" alwaysSame example
-- > [fail] Randomly!
-- >         "hello" S0----S1--S0--
-- >
-- >         "world" S0----S2--S0--
-- >
-- > > dejafuWay (randomly (mkStdGen 1) 100) defaultMemType "Randomly!" alwaysSame example
-- > [fail] Randomly!
-- >         "hello" S0----S1--S0--
-- >
-- >         "world" S0----S2--S1-S0--
--
-- @since 1.0.0.0
dejafuWay :: (MonadConc n, MonadIO n, MonadRef r n, Show b)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> String
  -- ^ The name of the test.
  -> ProPredicate a b
  -- ^ The predicate to check.
  -> ConcT r n a
  -- ^ The computation to test.
  -> n Bool
dejafuWay = dejafuDiscard (const Nothing)

-- | Variant of 'dejafuWay' which can selectively discard results.
--
-- > > dejafuDiscard (\_ -> Just DiscardTrace) defaultWay defaultMemType "Discarding" alwaysSame example
-- > [fail] Discarding
-- >         "hello" <trace discarded>
-- >
-- >         "world" <trace discarded>
--
-- @since 1.0.0.0
dejafuDiscard :: (MonadConc n, MonadIO n, MonadRef r n, Show b)
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
  -> ConcT r n a
  -- ^ The computation to test.
  -> n Bool
dejafuDiscard discard way memtype name test conc = do
  let discarder = strengthenDiscard discard (pdiscard test)
  traces <- runSCTDiscard discarder way memtype conc
  liftIO $ doTest name (peval test traces)

-- | Variant of 'dejafu' which takes a collection of predicates to
-- test, returning 'True' if all pass.
--
-- > > dejafus [("A", alwaysSame), ("B", deadlocksNever)] example
-- > [fail] A
-- >         "hello" S0----S1--S0--
-- >
-- >         "world" S0----S2--S0--
-- > [pass] B
--
-- @since 1.0.0.0
dejafus :: (MonadConc n, MonadIO n, MonadRef r n, Show b)
  => [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check.
  -> ConcT r n a
  -- ^ The computation to test.
  -> n Bool
dejafus = dejafusWay defaultWay defaultMemType

-- | Variant of 'dejafus' which takes a way to run the program and a
-- memory model.
--
-- > > dejafusWay defaultWay SequentialConsistency [("A", alwaysSame), ("B", exceptionsNever)] relaxed
-- > [fail] A
-- >         (False,True) S0---------S1----S0--S2----S0--
-- >
-- >         (True,True) S0---------S1-P2----S1---S0---
-- >
-- >         (True,False) S0---------S2----S1----S0---
-- > [pass] B
--
-- @since 1.0.0.0
dejafusWay :: (MonadConc n, MonadIO n, MonadRef r n, Show b)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> [(String, ProPredicate a b)]
  -- ^ The list of predicates (with names) to check.
  -> ConcT r n a
  -- ^ The computation to test.
  -> n Bool
dejafusWay way memtype tests conc = do
    traces  <- runSCTDiscard discarder way memtype conc
    results <- mapM (\(name, test) -> liftIO . doTest name $ check test traces) tests
    pure (and results)
  where
    discarder = foldr
      (weakenDiscard . pdiscard . snd)
      (const (Just DiscardResultAndTrace))
      tests

    -- for evaluating each individual predicate, we only want the
    -- results/traces it would not discard, but the traces set may
    -- include more than this if the different predicates have
    -- different discard functions, so we do another pass of
    -- discarding.
    check p = peval p . mapMaybe go where
      go r@(efa, _) = case pdiscard p efa of
        Just DiscardResultAndTrace -> Nothing
        Just DiscardTrace -> Just (efa, [])
        Nothing -> Just r

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
runTest :: (MonadConc n, MonadRef r n)
  => ProPredicate a b
  -- ^ The predicate to check
  -> ConcT r n a
  -- ^ The computation to test
  -> n (Result b)
runTest = runTestWay defaultWay defaultMemType

-- | Variant of 'runTest' which takes a way to run the program and a
-- memory model.
--
-- The exact executions tried, and the order in which results are
-- found, is unspecified and may change between releases.  This may
-- affect which failing traces are reported, when there is a failure.
--
-- @since 1.0.0.0
runTestWay :: (MonadConc n, MonadRef r n)
  => Way
  -- ^ How to run the concurrent program.
  -> MemType
  -- ^ The memory model to use for non-synchronised @CRef@ operations.
  -> ProPredicate a b
  -- ^ The predicate to check
  -> ConcT r n a
  -- ^ The computation to test
  -> n (Result b)
runTestWay way memtype p conc =
  peval p <$> runSCTDiscard (pdiscard p) way memtype conc


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
  if _pass result
  then
    -- Display a pass message.
    putStrLn ("\27[32m[pass]\27[0m " ++ name)
  else do
    -- Display a failure message, and the first 5 (simplified) failed traces
    putStrLn ("\27[31m[fail]\27[0m " ++ name)

    unless (null $ _failureMsg result) $
      putStrLn $ _failureMsg result

    let failures = _failures result
    let output = map (\(r, t) -> putStrLn . indent $ either showFail show r ++ " " ++ showTrace t) $ take 5 failures
    sequence_ $ intersperse (putStrLn "") output
    when (moreThan 5 failures) $
      putStrLn (indent "...")

  pure (_pass result)

-- | Check if a list is longer than some value, without needing to
-- compute the entire length.
moreThan :: Int -> [a] -> Bool
moreThan n [] = n < 0
moreThan 0 _ = True
moreThan n (_:rest) = moreThan (n-1) rest

-- | Indent every line of a string.
indent :: String -> String
indent = intercalate "\n" . map ('\t':) . lines

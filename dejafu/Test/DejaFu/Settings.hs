{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Test.DejaFu.Settings
-- Copyright   : (c) 2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : RankNTypes
--
-- Configuration for the SCT functions.
module Test.DejaFu.Settings
  ( -- * SCT configuration
    Settings
  , defaultSettings
  , fromWayAndMemType

  -- ** The @Way@
  , Way
  , defaultWay
  , lway
  , systematically
  , randomly
  , uniformly

  -- *** Schedule bounds

  -- | Schedule bounding is used by the 'systematically' approach to
  -- limit the search-space, which in general will be huge.
  --
  -- There are three types of bound:
  --
  --  * The 'PreemptionBound', which bounds the number of pre-emptive
  --    context switches.  Empirical evidence suggests @2@ is a good
  --    value for this, if you have a small test case.
  --
  --  * The 'FairBound', which bounds the difference between how many
  --    times threads can yield.  This is necessary to test certain
  --    kinds of potentially non-terminating behaviour, such as
  --    spinlocks.
  --
  --  * The 'LengthBound', which bounds how long a test case can run,
  --    in terms of scheduling decisions.  This is necessary to test
  --    certain kinds of potentially non-terminating behaviour, such
  --    as livelocks.
  --
  -- Schedule bounding is not used by the non-systematic exploration
  -- behaviours.

  , Bounds(..)
  , PreemptionBound(..)
  , FairBound(..)
  , LengthBound(..)
  , defaultBounds
  , defaultPreemptionBound
  , defaultFairBound
  , defaultLengthBound
  , noBounds

  -- ** The @MemType@

  -- | When executed on a multi-core processor some @CRef@ / @IORef@
  -- programs can exhibit \"relaxed memory\" behaviours, where the
  -- apparent behaviour of the program is not a simple interleaving of
  -- the actions of each thread.
  --
  -- __Example:__ This is a simple program which creates two @CRef@s
  -- containing @False@, and forks two threads.  Each thread writes
  -- @True@ to one of the @CRef@s and reads the other.  The value that
  -- each thread reads is communicated back through an @MVar@:
  --
  -- > >>> :{
  -- > let relaxed = do
  -- >       r1 <- newCRef False
  -- >       r2 <- newCRef False
  -- >       x <- spawn $ writeCRef r1 True >> readCRef r2
  -- >       y <- spawn $ writeCRef r2 True >> readCRef r1
  -- >       (,) <$> readMVar x <*> readMVar y
  -- > :}
  --
  -- We see something surprising if we ask for the results:
  --
  -- > >>> autocheck relaxed
  -- > [pass] Never Deadlocks
  -- > [pass] No Exceptions
  -- > [fail] Consistent Result
  -- >     (False,True) S0---------S1----S0--S2----S0--
  -- >
  -- >     (False,False) S0---------S1--P2----S1--S0---
  -- >
  -- >     (True,False) S0---------S2----S1----S0---
  -- >
  -- >     (True,True) S0---------S1-C-S2----S1---S0---
  -- > False
  --
  -- It's possible for both threads to read the value @False@, even
  -- though each writes @True@ to the other @CRef@ before reading.
  -- This is because processors are free to re-order reads and writes
  -- to independent memory addresses in the name of performance.
  --
  -- Execution traces for relaxed memory computations can include
  -- \"C\" actions, as above, which show where @CRef@ writes were
  -- explicitly /committed/, and made visible to other threads.
  --
  -- However, modelling this behaviour can require more executions.
  -- If you do not care about the relaxed-memory behaviour of your
  -- program, use the 'SequentialConsistency' model.

  , MemType(..)
  , defaultMemType
  , lmemtype

  -- ** Discard functions

  -- | Sometimes we know that a result is uninteresting and cannot
  -- affect the result of a test, in which case there is no point in
  -- keeping it around.  Execution traces can be large, so any
  -- opportunity to get rid of them early is possibly a great saving
  -- of memory.
  --
  -- A discard function, which has type @Either Failure a -> Maybe
  -- Discard@, can selectively discard results or execution traces
  -- before the schedule exploration finishes, allowing them to be
  -- garbage collected sooner.
  --
  -- __Note:__ The predicates and helper functions in Test.DejaFu come
  -- with discard functions built in, to discard results and traces
  -- wherever possible.

  , Discard(..)
  , ldiscard

  -- ** Early exit

  -- | Sometimes we don't want to wait for all executions to be
  -- explored, we just want to stop as soon as a particular result is
  -- found.  An early-exit predicate, which has type @Either Failure a
  -- -> Bool@, can opt to halt execution when such a result is found.
  --
  -- All results found up to, and including, the one which terminates
  -- the exploration are reported.
  --
  -- __Usage in combination with a discard function:__ A discard
  -- function can be used in combination with early-exit.  As usual,
  -- results or traces will be discarded as appropriate.  If a single
  -- result causes the early-exit function to return @True@ and the
  -- discard function to return @Just DiscardResultAndTrace@, the
  -- exploration will end early, but the result will not be included
  -- in the output.

  , learlyExit

  -- ** Representative traces

  -- | There may be many different execution traces which give rise to
  -- the same result, but some traces can be more complex than others.
  --
  -- By supplying an equality predicate on results, all but the
  -- simplest trace for each distinct result can be thrown away.
  --
  -- __Slippage:__ Just comparing results can lead to different errors
  -- which happen to have the same result comparing as equal.  For
  -- example, all deadlocks have the same result (@Left Deadlock@),
  -- but may have different causes.  See issue @#241@.

  , lequality

  -- ** Shrinking

  -- | There may be many ways to reveal the same bug, and dejafu is
  -- not guaranteed to find the simplest way first.  This is
  -- particularly problematic with random testing, where the schedules
  -- generated tend to involve a lot of context switching.  Shrinking
  -- produces simpler traces, which still have the same essential
  -- behaviour.
  --
  -- __Performance:__ Shrinking can be expensive, as it involves
  -- running the program again for each distinct result.  This is why
  -- shrinking is disabled by default.  If you want to use shrinking,
  -- it is /highly/ recommended to also use 'lequality', to reduce the
  -- number of traces to shrink.

  , lshrink

  -- ** Debug output

  -- | You can opt to receive debugging messages by setting debugging
  -- print and show functions.  Enabling debugging doesn't change any
  -- behaviour, it just causes messages to be printed.  These options
  -- are most likely not useful for anyone not developing dejafu.

  , ldebugShow
  , ldebugPrint

  -- * Lens helpers
  , get
  , set

  -- * Deprecated
  , swarmy
  ) where

import           Control.Applicative   (Const(..))
import           Data.Functor.Identity (Identity(..))
import           System.Random         (RandomGen)

import           Test.DejaFu.Internal  (Settings(..), Way(..))
import           Test.DejaFu.Types

-------------------------------------------------------------------------------
-- SCT configuration

-- | Default SCT settings: just combine all the other defaults.
--
-- @since 1.2.0.0
defaultSettings :: Applicative n => Settings n a
defaultSettings = fromWayAndMemType defaultWay defaultMemType

-- | Construct a 'Settings' record from a 'Way' and a 'MemType'.
--
-- All other settings take on their default values.
--
-- @since 1.2.0.0
fromWayAndMemType :: Applicative n => Way -> MemType -> Settings n a
fromWayAndMemType way memtype = Settings
  { _way = way
  , _memtype = memtype
  , _discard = Nothing
  , _debugShow = Nothing
  , _debugPrint = Nothing
  , _earlyExit = Nothing
  , _equality = Nothing
  , _shrink = False
  }

-------------------------------------------------------------------------------
-- The @Way@

-- | A default way to execute concurrent programs: systematically
-- using 'defaultBounds'.
--
-- @since 0.6.0.0
defaultWay :: Way
defaultWay = systematically defaultBounds

-- | A lens into the 'Way'.
--
-- @since 1.2.0.0
lway :: Lens' (Settings n a) Way
lway afb s = (\b -> s {_way = b}) <$> afb (_way s)

-- | Systematically execute a program, trying all distinct executions
-- within the bounds.
--
-- @since 0.7.0.0
systematically
  :: Bounds
  -- ^ The bounds to constrain the exploration.
  -> Way
systematically = Systematic

-- | Randomly execute a program, exploring a fixed number of
-- executions.
--
-- Threads are scheduled by a weighted random selection, where weights
-- are assigned randomly on thread creation.
--
-- This is not guaranteed to find all distinct results (unlike
-- 'systematically').
--
-- @since 0.7.0.0
randomly :: RandomGen g
  => g
  -- ^ The random generator to drive the scheduling.
  -> Int
  -- ^ The number of executions to try.
  -> Way
randomly g lim = Weighted g lim 1

-- | Randomly execute a program, exploring a fixed number of
-- executions.
--
-- Threads are scheduled by a uniform random selection.
--
-- This is not guaranteed to find all distinct results (unlike
-- 'systematically').
--
-- @since 0.7.0.0
uniformly :: RandomGen g
  => g
  -- ^ The random generator to drive the scheduling.
  -> Int
  -- ^ The number of executions to try.
  -> Way
uniformly = Uniform

-- | Randomly execute a program, exploring a fixed number of
-- executions.
--
-- Threads are scheduled by a weighted random selection, where weights
-- are assigned randomly on thread creation.
--
-- This is not guaranteed to find all distinct results (unlike
-- 'systematically').
--
-- @since 0.7.0.0
swarmy :: RandomGen g
  => g
  -- ^ The random generator to drive the scheduling.
  -> Int
  -- ^ The number of executions to try.
  -> Int
  -- ^ The number of executions to use the thread weights for.
  -> Way
-- when this is removed, simplify the Weighted logic to not do re-use
swarmy = Weighted
{-# DEPRECATED swarmy "Use randomly instead.  If you have a case where swarmy works better, please comment on issue #237." #-}

-------------------------------------------------------------------------------
-- Schedule bounds

-- | All bounds enabled, using their default values.
--
-- @since 0.2.0.0
defaultBounds :: Bounds
defaultBounds = Bounds
  { boundPreemp = Just defaultPreemptionBound
  , boundFair   = Just defaultFairBound
  , boundLength = Just defaultLengthBound
  }

-- | A sensible default preemption bound: 2.
--
-- See /Concurrency Testing Using Schedule Bounding: an Empirical Study/,
-- P. Thomson, A. F. Donaldson, A. Betts for justification.
--
-- @since 0.2.0.0
defaultPreemptionBound :: PreemptionBound
defaultPreemptionBound = 2

-- | A sensible default fair bound: 5.
--
-- This comes from playing around myself, but there is probably a
-- better default.
--
-- @since 0.2.0.0
defaultFairBound :: FairBound
defaultFairBound = 5

-- | A sensible default length bound: 250.
--
-- Based on the assumption that anything which executes for much
-- longer (or even this long) will take ages to test.
--
-- @since 0.2.0.0
defaultLengthBound :: LengthBound
defaultLengthBound = 250

-- | No bounds enabled. This forces the scheduler to just use
-- partial-order reduction and sleep sets to prune the search
-- space. This will /ONLY/ work if your computation always terminates!
--
-- @since 0.3.0.0
noBounds :: Bounds
noBounds = Bounds
  { boundPreemp = Nothing
  , boundFair   = Nothing
  , boundLength = Nothing
  }

-------------------------------------------------------------------------------
-- The @MemType@

-- | The default memory model: @TotalStoreOrder@
--
-- @since 0.2.0.0
defaultMemType :: MemType
defaultMemType = TotalStoreOrder

-- | A lens into the 'MemType'.
--
-- @since 1.2.0.0
lmemtype :: Lens' (Settings n a) MemType
lmemtype afb s = (\b -> s {_memtype = b}) <$> afb (_memtype s)

-------------------------------------------------------------------------------
-- Discard functions

-- | A lens into the discard function.
--
-- @since 1.2.0.0
ldiscard :: Lens' (Settings n a) (Maybe (Either Failure a -> Maybe Discard))
ldiscard afb s = (\b -> s {_discard = b}) <$> afb (_discard s)

-------------------------------------------------------------------------------
-- Early exit

-- | A lens into the early-exit predicate.
--
-- @since 1.2.0.0
learlyExit :: Lens' (Settings n a) (Maybe (Either Failure a -> Bool))
learlyExit afb s = (\b -> s {_earlyExit = b}) <$> afb (_earlyExit s)

-------------------------------------------------------------------------------
-- Representative traces

-- | A lens into the equality predicate.
--
-- @since unreleased
lequality :: Lens' (Settings n a) (Maybe (a -> a -> Bool))
lequality afb s = (\b -> s {_equality = b}) <$> afb (_equality s)

-------------------------------------------------------------------------------
-- Shrinking

-- | A lens into the shrink flag.
--
-- @since unreleased
lshrink :: Lens' (Settings n a) Bool
lshrink afb s = (\b -> s {_shrink = b}) <$> afb (_shrink s)

-------------------------------------------------------------------------------
-- Debug output

-- | A lens into the debug 'show' function.
--
-- @since 1.2.0.0
ldebugShow :: Lens' (Settings n a) (Maybe (a -> String))
ldebugShow afb s = (\b -> s {_debugShow = b}) <$> afb (_debugShow s)

-- | A lens into the debug 'print' function.
--
-- @since 1.2.0.0
ldebugPrint :: Lens' (Settings n a) (Maybe (String -> n ()))
ldebugPrint afb s = (\b -> s {_debugPrint = b}) <$> afb (_debugPrint s)

-------------------------------------------------------------------------------
-- Lens helpers

-- lens type synonyms, unexported
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- | Get a value from a lens.
--
-- @since 1.2.0.0
get :: Lens' s a -> s -> a
get lens = getConst . lens Const

-- | Set a value in a lens.
--
-- @since 1.2.0.0
set :: Lens' s a -> a -> s -> s
set lens a = runIdentity . lens (\_ -> Identity a)

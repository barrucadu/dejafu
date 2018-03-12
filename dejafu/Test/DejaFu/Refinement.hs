{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Test.DejaFu.Refinement
-- Copyright   : (c) 2017--2018 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : FlexibleContexts, FlexibleInstances, GADTs, MultiWayIf, TupleSections, TypeFamilies
--
-- Properties about the side-effects of concurrent functions on some
-- shared state.
--
-- Consider this statement about @MVar@s: \"using @readMVar@ is better
-- than @takeMVar@ followed by @putMVar@ because the former is atomic
-- but the latter is not.\"
--
-- This module can test properties like that:
--
-- >>> import Control.Monad (void)
-- >>> :{
-- let sig e = Sig
--       { initialise = maybe newEmptyMVar newMVar
--       , observe    = \v _ -> tryReadMVar v
--       , interfere  = \v _ -> putMVar v 42
--       , expression = void . e
--       }
-- :}
--
-- >>> check $ sig readMVar === sig (\v -> takeMVar v >>= putMVar v)
-- *** Failure: (seed Just 0)
--     left:  [(Nothing,Just 0)]
--     right: [(Nothing,Just 0),(Just Deadlock,Just 42)]
-- False
--
-- The two expressions are not equivalent, and we get given the
-- counterexample!
--
-- There are quite a few things going on here, so let's unpack this:
--
-- (1) Properties are specified in terms of an __initialisation__
--     function, an __observation__ function, an __interference__
--     function, and the expression of interest.
--
-- (2) The initialisation function ('initialise') says how to
--     construct some __state__ value from a __seed__ value, which is
--     supplied by 'check'.  In this case the seed is of type @Maybe
--     a@ and the state @MVar ConcIO a@.
--
-- (3) The observation ('observe') function says how to take the state
--     and the seed, and produce some value which will be used to
--     compare the expressions.  In this case the observation value is
--     of type @Maybe a@.
--
-- (4) The interference ('interfere') function says what sort of
--     concurrent interference can happen.  In this case we just try
--     to set the @MVar@ to its original value.
--
-- The 'check' function takes a property, consisting of two signatures
-- and a way to compare them, evaluates all the results of each
-- signature, and then compares them in the appropriate way.
--
-- See the sections later in the documentation for what
-- \"refinement\", \"strict refinement\", and \"equivalence\" mean
-- exactly.
module Test.DejaFu.Refinement
  ( -- * Defining properties
    Sig(..)
  , RefinementProperty
  , expectFailure

  -- ** A refines B

  -- | Refinement (or \"weak refinement\") means that all of the
  -- results of the left are also results of the right.  If you think
  -- in terms of sets of results, refinement is subset.
  , refines, (=>=)

  -- ** A strictly refines B

  -- | Strict refinement means that the left refines the right, but
  -- the right does not refine the left.  If you think in terms of
  -- sets of results, strict refinement is proper subset.
  , strictlyRefines, (->-)

  -- ** A is equivalent to B

  -- | Equivalence means that the left and right refine each other.
  -- If you think in terms of sets of results, equivalence is
  -- equality.
  , equivalentTo, (===)

  -- * Testing properties
  , FailedProperty(..)
  , Testable(O,X)
  , check
  , check'
  , checkFor
  , counterExamples

  -- * Re-exports
  , Listable(..)
  ) where

import           Control.Arrow            (first)
import           Control.Monad.Conc.Class (fork)
import           Data.Maybe               (isNothing)
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Test.LeanCheck           (Listable(..), concatMapT, mapT)

import           Test.DejaFu.Conc         (ConcIO, Failure, subconcurrency)
import           Test.DejaFu.SCT          (runSCT)
import           Test.DejaFu.Settings     (defaultMemType, defaultWay)

-- $setup
-- >>> import Control.Concurrent.Classy hiding (check)

-------------------------------------------------------------------------------
-- Specifying properties

-- | What to check.
data How = Weak | Equiv | Strict deriving Eq

-- | A property which can be given to 'check'.
--
-- @since 0.7.0.0
data RefinementProperty o x where
  RP  :: Ord o => How -> Sig s1 o x -> Sig s2 o x -> RefinementProperty o x
  Neg :: RefinementProperty o x -> RefinementProperty o x

-- | A concurrent function and some information about how to execute
-- it and observe its effect.
--
-- * @s@ is the state type (@MVar ConcIO a@ in the example)
-- * @o@ is the observation type (@Maybe a@ in the example)
-- * @x@ is the seed type (@Maybe a@ in the example)
--
-- @since 0.7.0.0
data Sig s o x = Sig
  { initialise :: x -> ConcIO s
  -- ^ Create a new instance of the state variable.
  , observe :: s -> x -> ConcIO o
  -- ^ The observation to make.
  , interfere :: s -> x -> ConcIO ()
  -- ^ Set the state value. This doesn't need to be atomic, or even
  -- guaranteed to work, its purpose is to cause interference.
  , expression :: s -> ConcIO ()
  -- ^ The expression to evaluate.
  }

-- | Indicates that the property is supposed to fail.
expectFailure :: RefinementProperty o x -> RefinementProperty o x
expectFailure = Neg

-- | Observational refinement.
--
-- True iff the result-set of the left expression is a subset (not
-- necessarily proper) of the result-set of the right expression.
--
-- The two signatures can have different state types, this lets you
-- compare the behaviour of different data structures.  The
-- observation and seed types must match, however.
--
-- @since 0.7.0.0
refines :: Ord o => Sig s1 o x -> Sig s2 o x -> RefinementProperty o x
refines = RP Weak

-- | Infix synonym for 'refines'.
--
-- You might think this should be '=<=', so it looks kind of like a
-- funny subset operator, with @A =<= B@ meaning \"the result-set of A
-- is a subset of the result-set of B\".  Unfortunately you would be
-- wrong.  The operator used in the literature for refinement has the
-- open end pointing at the LESS general term and the closed end at
-- the MORE general term.  It is read as \"is refined by\", not
-- \"refines\".  So for consistency with the literature, the open end
-- of @=>=@ points at the less general term, and the closed end at the
-- more general term, to give the same argument order as 'refines'.
--
-- @since 0.7.0.0
(=>=) :: Ord o => Sig s1 o x -> Sig s2 o x -> RefinementProperty o x
(=>=) = refines

-- | Observational equivalence.
--
-- True iff the result-set of the left expression is equal to the
-- result-set of the right expression.
--
-- The two signatures can have different state types, this lets you
-- compare the behaviour of different data structures.  The
-- observation and seed types must match, however.
--
-- @since 0.7.0.0
equivalentTo :: Ord o => Sig s1 o x -> Sig s2 o x -> RefinementProperty o x
equivalentTo = RP Equiv

-- | Infix synonym for 'equivalentTo'.
--
-- @since 0.7.0.0
(===) :: Ord o => Sig s1 o x -> Sig s2 o x -> RefinementProperty o x
(===) = equivalentTo

-- | Strict observational refinement.
--
-- True iff the result-set of the left expression is a proper subset
-- of the result-set of the right expression.
--
-- The two signatures can have different state types, this lets you
-- compare the behaviour of different data structures.  The
-- observation and seed types must match, however.
--
-- @since 0.7.0.0
strictlyRefines :: Ord o => Sig s1 o x -> Sig s2 o x -> RefinementProperty o x
strictlyRefines = RP Strict

-- | Infix synonym for 'strictlyRefines'
--
-- @since 0.7.0.0
(->-) :: Ord o => Sig s1 o x -> Sig s2 o x -> RefinementProperty o x
(->-) = strictlyRefines


-------------------------------------------------------------------------------
-- Property testing

-- | Things which can be tested.
--
-- @since 0.7.0.0
class Testable a where
  -- | The observation value type.  This is used to compare the
  -- results.
  type O a :: *

  -- | The seed value type.  This is used to construct the concurrent
  -- states.
  type X a :: *

  rpropTiers :: a -> [[([String], RefinementProperty (O a) (X a))]]

instance Testable (RefinementProperty o x) where
  type O (RefinementProperty o x) = o
  type X (RefinementProperty o x) = x

  rpropTiers p = [[([], p)]]

instance (Listable a, Show a, Testable b) => Testable (a -> b) where
  type O (a -> b) = O b
  type X (a -> b) = X b

  rpropTiers p = concatMapT resultiersFor tiers where
    resultiersFor x = first (show x:) `mapT` rpropTiers (p x)

-- | A counter example is a seed value and a list of variable
-- assignments.
--
-- @since 0.7.0.0
data FailedProperty o x
  = CounterExample
    { failingSeed  :: x
    -- ^ The seed for this set of executions.
    , failingArgs  :: [String]
    -- ^ The values of free variables, as strings.
    , leftResults  :: Set (Maybe Failure, o)
    -- ^ The set of results of the left signature.
    , rightResults :: Set (Maybe Failure, o)
    -- ^ The set of results of the right signature.
    }
  | NoExpectedFailure
  deriving Show

-- | Check a refinement property with a variety of seed values and
-- variable assignments.
--
-- @since 0.7.0.0
check :: (Testable p, Listable (X p), Show (X p), Show (O p))
  => p
  -- ^ The property to check.
  -> IO Bool
check p = do
  ce <- check' p
  putStrLn $ case ce of
    Just NoExpectedFailure -> "*** Failure: passed, but expected failure."
    Just c -> init $ unlines
      [ "*** Failure: " ++
        (if null (failingArgs c) then "" else unwords (failingArgs c) ++ " ") ++
        "(seed " ++ show (failingSeed c) ++ ")"
      , "    left:  " ++ show (S.toList $ leftResults  c)
      , "    right: " ++ show (S.toList $ rightResults c)
      ]
    Nothing -> "+++ OK"
  pure (isNothing ce)

-- | A version of 'check' that doesn't print, and returns the
-- counterexample.
--
-- @since 0.7.0.0
check' :: (Testable p, Listable (X p))
  => p
  -- ^ The property to check.
  -> IO (Maybe (FailedProperty (O p) (X p)))
check' = checkFor 10 100

-- | Like 'check', but take a number of cases to try, also returns the
-- counter example found rather than printing it.
--
-- If multiple counterexamples exist, this will be faster than
-- @listToMaybe@ composed with @counterExamples@.
--
-- @since 0.7.0.0
checkFor :: (Testable p, Listable (X p))
  => Int
  -- ^ Number of seed values per variable-assignment.
  -> Int
  -- ^ Number of variable assignments.
  -> p
  -- ^ The property to check.
  -> IO (Maybe (FailedProperty (O p) (X p)))
checkFor sn vn p =  do
    let seeds = take sn $ concat tiers
    let cases = take vn $ concat (rpropTiers p)
    go seeds cases
  where
    go seeds ((vs, p'):rest) = do
      r <- checkWithSeeds seeds p'
      case r of
        Just cf -> pure (Just (cf vs))
        Nothing -> go seeds rest
    go _ [] = pure Nothing

-- | Find all counterexamples up to a limit.
--
-- @since 0.7.0.0
counterExamples :: (Testable p, Listable (X p))
  => Int
  -- ^ Number of seed values per variable-assignment.
  -> Int
  -- ^ Number of variable assignments
  -> p
  -- ^ The property to check.
  -> IO [FailedProperty (O p) (X p)]
counterExamples sn vn p = do
  let seeds = take sn $ concat tiers
  let cases = take vn $ concat (rpropTiers p)
  rs <- mapM (\(vs, p') -> (vs,) <$> checkWithSeeds seeds p') cases
  pure [ cf vs | (vs, Just cf) <- rs ]


-------------------------------------------------------------------------------
-- Internal

-- | Three-valued sum, used in checking strict refinement.
data F x = Failing x | Unknown | Refuted

-- | Check a refinement property with given seed values.  Returns the
-- counterexample if the property is false.
checkWithSeeds
  :: [x]
  -- ^ Seed values to use.
  -> RefinementProperty o x
  -- ^ The property to check.
  -> IO (Maybe ([String] -> FailedProperty o x))
checkWithSeeds seeds (RP how l r) = case how of
    Weak   -> go1 S.isSubsetOf seeds
    Equiv  -> go1 (==)         seeds
    Strict -> go2 Unknown      seeds
  where
    -- weak and equiv need every set of pairwise result-sets to match
    -- some predicate.
    go1 f (x:xs) = do
      lrs <- evalSigWithSeed l x
      rrs <- evalSigWithSeed r x
      if lrs `f` rrs
        then go1 f xs
        else pure (Just $ toCE x lrs rrs)
    go1 _ [] = pure Nothing

    -- strict fails if (a) any left result-set is not a subset of the
    -- corresponding right result-set, or (b) every left result-set is
    -- equal to the corresponding right result-set
    go2 eq (x:xs) = do
      lrs <- evalSigWithSeed l x
      rrs <- evalSigWithSeed r x
      let ce = toCE x lrs rrs
      if | lrs == rrs             -> go2 (case eq of Unknown -> Failing ce; _ -> eq) xs
         | lrs `S.isSubsetOf` rrs -> go2 Refuted xs
         | otherwise              -> pure (Just ce)
    go2 (Failing cf) [] = pure (Just cf)
    go2 _ [] = pure Nothing

    toCE x lrs rrs args = CounterExample
      { failingSeed  = x
      , failingArgs  = args
      , leftResults  = lrs
      , rightResults = rrs
      }
checkWithSeeds seeds (Neg rp) = do
  r <- checkWithSeeds seeds rp
  pure $ case r of
    Just _ -> Nothing
    Nothing -> Just (const NoExpectedFailure)

-- | Evaluate a signature with a given seed value
evalSigWithSeed :: Ord o
  => Sig s o x
  -> x
  -> IO (Set (Maybe Failure, o))
evalSigWithSeed sig x = do
  results <- runSCT defaultWay defaultMemType $ do
    s <- initialise sig x
    r <- subconcurrency $ do
      _ <- fork (interfere sig s x)
      _ <- expression sig s
      pure ()
    o <- observe sig s x
    pure (either Just (const Nothing) r, o)
  pure . S.fromList $ map (\(Right a, _) -> a) results

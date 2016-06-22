{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Typeclass laws for @Concurrently@ from the async package.
module Examples.ClassLaws where

import Control.Applicative
import Control.Exception (SomeException)
import Control.Monad ((>=>), ap, liftM, forever)
import Control.Monad.Catch (onException)
import Control.Monad.Conc.Class
import Data.Maybe (isJust)
import Data.Set (Set, fromList)
import Test.DejaFu (Failure(..), defaultBounds, defaultMemType)
import Test.DejaFu.Conc (ConcST)
import Test.DejaFu.SCT (sctBound)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), expectFailure, monomorphic)
import Test.QuickCheck.Function (Fun, apply)

import Examples.ClassLaws.Impredicative

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid(..))
#endif

-- Tests at bottom of file due to Template Haskell silliness.

--------------------------------------------------------------------------------

-- | A value of type @Concurrently m a@ is a @MonadConc@ operation
-- that can be composed with other @Concurrently@ values, using the
-- @Applicative@ and @Alternative@ instances.
--
-- Calling @runConcurrently@ on a value of type @Concurrently m a@
-- will execute the @MonadConc@ operations it contains concurrently,
-- before delivering the result of type @a@.
newtype Concurrently m a = Concurrently { runConcurrently :: m a }

type CST t = Concurrently (ConcST t)

--------------------------------------------------------------------------------
-- Functor

instance MonadConc m => Functor (Concurrently m) where
  fmap f (Concurrently a) = Concurrently $ f <$> a

-- fmap id a = a
prop_functor_id :: Ord a => CST t a -> Bool
prop_functor_id ca = ca `eq` (fmap id ca)

-- fmap f . fmap g = fmap (f . g)
prop_functor_comp :: Ord c => CST t a -> Fun a b -> Fun b c -> Bool
prop_functor_comp ca (apply -> f) (apply -> g) = (g . f <$> ca) `eq` (g <$> (f <$> ca))

--------------------------------------------------------------------------------
-- Applicative

instance MonadConc m => Applicative (Concurrently m) where
  pure = Concurrently . pure

  Concurrently fs <*> Concurrently as = Concurrently $ (\(f, a) -> f a) <$> concurrently fs as

-- pure id <*> a = a
prop_applicative_id :: Ord a => CST t a -> Bool
prop_applicative_id ca = ca `eq` (pure id <*> ca)

-- pure f <*> pure x = pure (f x)
prop_applicative_homo :: Ord b => a -> Fun a b -> Bool
prop_applicative_homo a (apply -> f) = (pure $ f a) `eq` (pure f <*> pure a)

-- u <*> pure y = pure ($ y) <*> u
prop_applicative_inter :: Ord b => CST t (Fun a b) -> a -> Bool
prop_applicative_inter u y = (u' <*> pure y) `eq` (pure ($ y) <*> u') where
  u' = apply <$> u

-- u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
prop_applicative_comp :: Ord c => CST t (Fun b c) -> CST t (Fun a b) -> CST t a -> Bool
prop_applicative_comp u v w = (u' <*> (v' <*> w)) `eq` (pure (.) <*> u' <*> v' <*> w) where
  u' = apply <$> u
  v' = apply <$> v

-- f <$> x = pure f <*> x
prop_applicative_fmap :: Ord b => Fun a b -> CST t a -> Bool
prop_applicative_fmap (apply -> f) a = (f <$> a) `eq` (pure f <*> a)

--------------------------------------------------------------------------------
-- Monad

instance MonadConc m => Monad (Concurrently m) where
  return = pure

  Concurrently a >>= f = Concurrently $ a >>= runConcurrently . f

-- return >=> f = f
prop_monad_left_id :: Ord b => Fun a (CST t b) -> a -> Bool
prop_monad_left_id (apply -> f) = f `eqf` (return >=> f)

-- f >=> return = f
prop_monad_right_id :: Ord b => Fun a (CST t b) -> a -> Bool
prop_monad_right_id (apply -> f) = f `eqf` (f >=> return)

-- (f >=> g) >=> h = f >=> (g >=> h)
prop_monad_assoc :: Ord d => Fun a (CST t b) -> Fun b (CST t c) -> Fun c (CST t d) -> a -> Bool
prop_monad_assoc (apply -> f) (apply -> g) (apply -> h) = ((f >=> g) >=> h) `eqf` (f >=> (g >=> h))

-- f <$> a = f `liftM` a
prop_monad_fmap :: Ord b => Fun a b -> CST t a -> Bool
prop_monad_fmap (apply -> f) a = (f <$> a) `eq` (f `liftM` a)

-- return = pure
prop_monad_pure :: Ord a => a -> Bool
prop_monad_pure = pure `eqf` return

-- (<*>) = ap
prop_monad_ap :: Ord b => Fun a b -> a -> Bool
prop_monad_ap (apply -> f) a = (pure f <*> pure a) `eq` (return f `ap` return a)

-- (<*>) = ap, side-effect-testing version
prop_monad_ap' :: forall a b. Ord b => Fun a b -> Fun a b -> a -> Bool
prop_monad_ap' (apply -> f) (apply -> g) a = go (<*>) `eq'` go ap where
  go :: (CST t (a -> b) -> CST t a -> CST t b) -> ConcST t b
  go combine = do
    var <- newEmptyMVar
    let cf = do { res <- tryTakeMVar var; pure $ if isJust res then f else g }
    let ca = do { putMVar var (); pure a }
    runConcurrently $ Concurrently cf `combine` Concurrently ca

--------------------------------------------------------------------------------
-- Alternative

instance MonadConc m => Alternative (Concurrently m) where
  empty = Concurrently $ forever yield

  Concurrently as <|> Concurrently bs =
    Concurrently $ either id id <$> race as bs

-- x <|> (y <|> z) = (x <|> y) <|> z
prop_alternative_assoc :: Ord a => CST t a -> CST t a -> CST t a -> Bool
prop_alternative_assoc x y z = (x <|> (y <|> z)) `eq` ((x <|> y) <|> z)

-- x = x <|> empty
prop_alternative_right_id :: Ord a => CST t a -> Bool
prop_alternative_right_id x = x `eq` (x <|> empty)

-- x = empty <|> x
prop_alternative_left_id :: Ord a => CST t a -> Bool
prop_alternative_left_id x = x `eq` (empty <|> x)

--------------------------------------------------------------------------------
-- Stuff for testing

instance Show (Concurrently m a) where
  show _ = "<concurrently>"

instance (Arbitrary a, Applicative m) => Arbitrary (Concurrently m a) where
  arbitrary = Concurrently . pure <$> arbitrary

instance Monoid Integer where
  mempty  = 0
  mappend = (+)

eq :: Ord a => CST t a -> CST t a -> Bool
eq left right = runConcurrently left `eq'` runConcurrently right

eq' :: forall t a. Ord a => ConcST t a -> ConcST t a -> Bool
eq' left right = runST' $ do
  leftTraces  <- sctBound defaultMemType defaultBounds left
  rightTraces <- sctBound defaultMemType defaultBounds right
  let toSet = fromList . map fst
  pure (toSet leftTraces == toSet rightTraces)

eqf :: Ord b => (a -> CST t b) -> (a -> CST t b) -> a -> Bool
eqf left right a = left a `eq` right a

--------------------------------------------------------------------------------
-- Stuff copied from async

concurrently :: MonadConc m => m a -> m b -> m (a, b)
concurrently left right = concurrently' left right (collect []) where
  collect [Left a, Right b] _ = return (a, b)
  collect [Right b, Left a] _ = return (a, b)
  collect xs m = do
    e <- takeMVar m
    case e of
      Left ex -> throw ex
      Right r -> collect (r:xs) m

concurrently' :: MonadConc m => m a -> m b
  -> (MVar m (Either SomeException (Either a b)) -> m r)
  -> m r
concurrently' left right collect = do
  done <- newEmptyMVar
  mask $ \restore -> do
    lid <- fork $ restore (left >>= putMVar done . Right . Left)
          `catch` (putMVar done . Left)

    rid <- fork $ restore (right >>= putMVar done . Right . Right)
          `catch` (putMVar done . Left)

    -- See: https://github.com/simonmar/async/issues/27
    let stop = killThread rid >> killThread lid

    r <- restore (collect done) `onException` stop

    stop

    return r

race :: MonadConc m => m a -> m b -> m (Either a b)
race left right = concurrently' left right collect where
  collect m = do
    e <- takeMVar m
    case e of
      Left ex -> throw ex
      Right r -> return r

--------------------------------------------------------------------------------

return []

-- QuickChecking the Applicative composition and Alternative
-- associativity laws is really slow to run every time, sadly. I have
-- done all I can think of for now to cut down the number of
-- executions tried, they give rise to something on the order of 10000
-- or so executions (100 sets of random inputs, 2 computations per
-- test, 40 to 50 schedules for each computation)
--
-- I expect a large portion of it is due to the exception handling,
-- which is admittedly rather heavy-handed right now. There's got to
-- be some better way than just having all exceptions be dependent
-- with everything ever, except Stop.

tests :: [Test]
tests =
  [ testGroup "Functor Laws"
   [ testProperty "identity"    $(monomorphic 'prop_functor_id)
   , testProperty "composition" $(monomorphic 'prop_functor_comp)
   ]
  , testGroup "Applicative Laws"
    [ testProperty "identity" $(monomorphic 'prop_applicative_id)
    , testProperty "homomorphism" $(monomorphic 'prop_applicative_homo)
    , testProperty "interchange"  $(monomorphic 'prop_applicative_inter)
    , testProperty "composition"  $(monomorphic 'prop_applicative_comp)
    , testProperty "fmap" $(monomorphic 'prop_applicative_fmap)
    ]
  , testGroup "Monad Laws"
    [ testProperty "left identity"  $(monomorphic 'prop_monad_left_id)
    , testProperty "right identity" $(monomorphic 'prop_monad_right_id)
    , testProperty "associativity"  $(monomorphic 'prop_monad_assoc)
    , testProperty "fmap" $(monomorphic 'prop_monad_fmap)
    , testProperty "pure" $(monomorphic 'prop_monad_pure)
    , testProperty "ap"   $(monomorphic 'prop_monad_ap)
    , testProperty "ap (side effects)" $ expectFailure $(monomorphic 'prop_monad_ap')
    ]
  , testGroup "Alternative Laws"
    [ testProperty "left identity"  $(monomorphic 'prop_alternative_left_id)
    , testProperty "right identity" $(monomorphic 'prop_alternative_right_id)
    --, testProperty "associativity"  $(monomorphic 'prop_alternative_assoc)
    ]
  ]

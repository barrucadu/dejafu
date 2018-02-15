{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Typeclass laws for @Concurrently@ from the async package.
module Examples.ClassLaws where

import           Control.Applicative
import           Control.Exception        (SomeException)
import           Control.Monad            (ap, forever, liftM, (>=>))
import           Control.Monad.Catch      (onException)
import           Control.Monad.Conc.Class
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Maybe               (isJust)
import           Data.Set                 (fromList)
import qualified Hedgehog                 as H
import           Test.DejaFu              (defaultBounds, defaultMemType)
import           Test.DejaFu.Conc         (ConcIO)
import           Test.DejaFu.SCT          (sctBound)
import qualified Test.Tasty.Hedgehog      as H

import           Common

tests :: [TestTree]
tests =
  [ testGroup "Functor" functorProps
  , testGroup "Applicative" applicativeProps
  , testGroup "Monad" monadProps
  , testGroup "Alternative" alternativeProps
  ]

--------------------------------------------------------------------------------

-- | A value of type @Concurrently m a@ is a @MonadConc@ operation
-- that can be composed with other @Concurrently@ values, using the
-- @Applicative@ and @Alternative@ instances.
--
-- Calling @runConcurrently@ on a value of type @Concurrently m a@
-- will execute the @MonadConc@ operations it contains concurrently,
-- before delivering the result of type @a@.
newtype Concurrently m a = Concurrently { runConcurrently :: m a }

type C = Concurrently ConcIO

--------------------------------------------------------------------------------
-- Functor

instance MonadConc m => Functor (Concurrently m) where
  fmap f (Concurrently a) = Concurrently $ f <$> a

functorProps :: [TestTree]
functorProps = toTestList
  [ H.testProperty "fmap id a = a" . H.property $ do
      (cval -> ca) <- H.forAll genA
      H.assert =<< ca `eq` fmap id ca

  , H.testProperty "fmap f . fmap g = fmap (f . g)" . H.property $ do
      (cval -> ca) <- H.forAll genA
      (fun -> f) <- H.forAll genFun
      (fun -> g) <- H.forAll genFun
      H.assert =<< (g . f <$> ca) `eq` (g <$> (f <$> ca))
  ]

--------------------------------------------------------------------------------
-- Applicative

instance MonadConc m => Applicative (Concurrently m) where
  pure = Concurrently . pure

  Concurrently fs <*> Concurrently as = Concurrently $ (\(f, a) -> f a) <$> concurrently fs as

applicativeProps :: [TestTree]
applicativeProps =
  [ H.testProperty "pure id <*> a = a" . H.property $ do
      (cval -> ca) <- H.forAll genA
      H.assert =<< ca `eq` (pure id <*> ca)

  , H.testProperty "pure f <*> pure x = pure (f x)" . H.property $ do
      (fun -> f) <- H.forAll genFun
      a <- H.forAll genA
      H.assert =<< pure (f a) `eq` (pure f <*> pure a)

  , H.testProperty "u <*> pure y = pure ($ y) <*> u" . H.property $ do
      (cfun -> u) <- H.forAll genFun
      y <- H.forAll genA
      H.assert =<< (u <*> pure y) `eq` (pure ($ y) <*> u)

  , testGroup "u <*> (v <*> w) = pure (.) <*> u <*> v <*> w"
    [ H.testProperty "Without races" . H.property $ do
        (cfun -> u) <- H.forAll genFun
        (cfun -> v) <- H.forAll genFun
        (cval -> w) <- H.forAll genA
        H.assert =<< (u <*> (v <*> w)) `eq` (pure (.) <*> u <*> v <*> w)

    -- todo: H.testProperty "With races" ...
    ]

  , H.testProperty "f <$> x = pure f <*> x" . H.property $ do
      (fun -> f) <- H.forAll genFun
      (cval -> a) <- H.forAll genA
      H.assert =<< (f <$> a) `eq` (pure f <*> a)
  ]

--------------------------------------------------------------------------------
-- Monad

instance MonadConc m => Monad (Concurrently m) where
  return = pure

  Concurrently a >>= f = Concurrently $ a >>= runConcurrently . f

monadProps :: [TestTree]
monadProps =
  [ H.testProperty "return >=> f = f" . H.property $ do
      (func -> f) <- H.forAll genFun
      a <- H.forAll genA
      H.assert =<< f a `eq` (return >=> f) a

  , H.testProperty "f >=> return = f" . H.property $ do
      (func -> f) <- H.forAll genFun
      a <- H.forAll genA
      H.assert =<< f a `eq` (f >=> return) a

  , H.testProperty "(f >=> g) >=> h = f >=> (g >=> h)" . H.property $ do
      (func -> f) <- H.forAll genFun
      (func -> g) <- H.forAll genFun
      (func -> h) <- H.forAll genFun
      a <- H.forAll genA
      H.assert =<< ((f >=> g) >=> h) a `eq` (f >=> (g >=> h)) a

  , H.testProperty "f <$> a = f `liftM` a" . H.property $ do
      (fun -> f) <- H.forAll genFun
      (cval -> a) <- H.forAll genA
      H.assert =<< (f <$> a) `eq` (f `liftM` a)

  , H.testProperty "return = pure" . H.property $ do
      a <- H.forAll genA
      H.assert =<< pure a `eq` return a

  , testGroup "(<*>) = ap"
    [ H.testProperty "Without races" . H.property $ do
        (fun -> f) <- H.forAll genFun
        a <- H.forAll genA
        H.assert =<< (pure f <*> pure a) `eq` (return f `ap` return a)

    , expectFail . H.testProperty "With races" . H.property $ do
        (fun -> f1) <- H.forAll genFun
        (fun -> f2) <- H.forAll genFun
        a <- H.forAll genA
        let go combine = do
              var <- newEmptyMVar
              let cf = do { res <- tryTakeMVar var; pure $ if isJust res then f1 else f2 }
              let ca = do { putMVar var (); pure a }
              runConcurrently $ Concurrently cf `combine` Concurrently ca
        H.assert =<< go (<*>) `eq'` go ap
    ]
  ]

--------------------------------------------------------------------------------
-- Alternative

instance MonadConc m => Alternative (Concurrently m) where
  empty = Concurrently $ forever yield

  Concurrently as <|> Concurrently bs =
    Concurrently $ either id id <$> race as bs

alternativeProps :: [TestTree]
alternativeProps =
  [ testGroup "x <|> (y <|> z) = (x <|> y) <|> z"
    [ H.testProperty "Without races" . H.property $ do
        (cval -> x) <- H.forAll genA
        (cval -> y) <- H.forAll genA
        (cval -> z) <- H.forAll genA
        H.assert =<< (x <|> (y <|> z)) `eq` ((x <|> y) <|> z)

    -- todo: H.testProperty "With races" ...
    ]

  , H.testProperty "x = x <|> empty" . H.property $ do
      (cval -> x) <- H.forAll genA
      H.assert =<< x `eq` (x <|> empty)

  , H.testProperty "x = empty <|> x" . H.property $ do
      (cval -> x) <- H.forAll genA
      H.assert =<< x `eq` (empty <|> x)
  ]

--------------------------------------------------------------------------------
-- Stuff for testing

eq :: (MonadIO m, Ord a) => C a -> C a -> m Bool
eq left right = runConcurrently left `eq'` runConcurrently right

eq' :: (MonadIO m, Ord a) => ConcIO a -> ConcIO a -> m Bool
eq' left right = liftIO $ do
  leftTraces  <- sctBound defaultMemType defaultBounds left
  rightTraces <- sctBound defaultMemType defaultBounds right
  let toSet = fromList . map fst
  pure (toSet leftTraces == toSet rightTraces)

--------------------------------------------------------------------------------
-- Stuff copied from async

concurrently :: MonadConc m => m a -> m b -> m (a, b)
concurrently left right = concurrently' left right (collect []) where
  collect [Left a, Right b] _ = pure (a, b)
  collect [Right b, Left a] _ = pure (a, b)
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

    pure r

race :: MonadConc m => m a -> m b -> m (Either a b)
race left right = concurrently' left right collect where
  collect m = do
    e <- takeMVar m
    case e of
      Left ex -> throw ex
      Right r -> pure r

-------------------------------------------------------------------------------
-- Hedgehog generators

genA :: H.Gen Int
genA = genSmallInt

genFun :: H.Gen (Function Int Int)
genFun = genFunction genA genA

-- for viewpatterns
fun :: Ord a => Function a b -> a -> b
fun = applyFunction

cfun :: Ord a => Function a b -> C (a -> b)
cfun = pure . applyFunction

func :: Ord a => Function a b -> a -> C b
func = fmap pure . applyFunction

cval :: a -> C a
cval = pure

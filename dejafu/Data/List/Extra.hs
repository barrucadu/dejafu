{-# LANGUAGE CPP #-}
-- | Extra list functions and list-like types.
module Data.List.Extra where

import Control.DeepSeq (NFData(..))
import Data.Traversable (fmapDefault, foldMapDefault)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
#else
-- Why does this give a redundancy warning? It's necessary in order to
-- define the toList function in the Foldable instance for NonEmpty!
import Data.Foldable (toList)
#endif

-- * Regular lists

-- | Check if a list has more than some number of elements.
moreThan :: [a] -> Int -> Bool
moreThan [] n = n < 0
moreThan _ 0  = True
moreThan (_:xs) n = moreThan xs (n-1)

-- * Non-empty lists

-- This gets exposed to users of the library, so it has a bunch of
-- classes which aren't actually used in the rest of the code to make
-- it more friendly to further use.

-- | The type of non-empty lists.
data NonEmpty a = a :| [a] deriving (Eq, Ord, Read, Show)

instance Functor NonEmpty where
  fmap = fmapDefault

instance Foldable NonEmpty where
  foldMap = foldMapDefault

#if __GLASGOW_HASKELL__ >= 710
  -- toList isn't in Foldable until GHC 7.10
  toList (a :| as) = a : as
#endif

instance Traversable NonEmpty where
  traverse f (a:|as) = (:|) <$> f a <*> traverse f as

instance NFData a => NFData (NonEmpty a) where
  rnf (x:|xs) = rnf (x, xs)

-- | Convert a 'NonEmpty' to a regular non-empty list.
toList :: NonEmpty a -> [a]
toList (a :| as) = a : as

-- | Convert a regular non-empty list to a 'NonEmpty'. This is
-- necessarily partial.
unsafeToNonEmpty :: [a] -> NonEmpty a
unsafeToNonEmpty (a:as) = a :| as
unsafeToNonEmpty [] = error "Cannot convert [] to NonEmpty!"

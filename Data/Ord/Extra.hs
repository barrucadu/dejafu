-- | Extra ordering functions and types.
module Data.Ord.Extra where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)

-- | A newtype for tuples which only checks the first element for 'Eq'
-- and 'Ord'.
newtype First a b = First { unFirst :: (a, b) }
  deriving (Read, Show)

instance Eq a => Eq (First a b) where
  (First (a1,_)) == (First (a2,_)) = a1 == a2
  (First (a1,_)) /= (First (a2,_)) = a1 /= a2

instance Ord a => Ord (First a b) where
  compare (First (a1,_)) (First (a2,_)) = compare a1 a2
  (First (a1,_)) <= (First (a2,_))       = a1 <= a2

instance Functor (First a) where
  fmap = fmapDefault

instance Foldable (First a) where
  foldMap = foldMapDefault

instance Traversable (First a) where
  traverse f (First (a, b)) = (\b -> First (a, b)) <$> f b

instance (NFData a, NFData b) => NFData (First a b) where
  rnf = rnf . unFirst

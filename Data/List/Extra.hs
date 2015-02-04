-- | Extra list functions and list-like types.
module Data.List.Extra where

import Control.DeepSeq (NFData(..))
import Data.List (foldl')

-- * Regular lists

-- | Split a list at an index and transform the two halves.
splitAtF :: ([a] -> b) -> ([a] -> c) -> Int -> [a] -> (b, c)
splitAtF f g i xs = let (l, r) = splitAt i xs in (f l, g r)

-- | Check if a list has more than some number of elements.
moreThan :: [a] -> Int -> Bool
moreThan [] n = n < 0
moreThan _ 0  = True
moreThan (_:xs) n = moreThan xs (n-1)

-- | For all sets of mutually comparable elements (hence the partial
-- ordering), remove all non-minimal ones.
sortNubBy :: (a -> a -> Maybe Ordering) -> [a] -> [a]
sortNubBy cmp = foldl' (flip insert) [] where
  insert x xs
    | any (\a -> a `cmp` x == Just LT) xs = xs
    | otherwise = x : filter (\a -> a `cmp` x /= Just GT) xs

-- * Non-empty lists

-- This gets exposed to users of the library, so it has a bunch of
-- classes which aren't actually used in the rest of the code to make
-- it more friendly to further use.

-- | The type of non-empty lists.
data NonEmpty a = a :| [a] deriving (Eq, Ord, Read, Show)

instance Functor NonEmpty where
  fmap f (a :| as) = f a :| map f as

instance NFData a => NFData (NonEmpty a) where
  rnf (x:|xs) = rnf (x, xs)

-- | Convert a 'NonEmpty' to a regular non-empty list.
toList :: NonEmpty a -> [a]
toList (a :| as) = a : as

-- * Tagged streams

-- | Data type representing a chunky, tagged, stream of data.
data Stream t a = Stream (t, NonEmpty a) (Stream t a) | Empty t

-- | Prepend a value onto a lazy stream.
(+|) :: (t, [a]) -> Stream t a -> Stream t a
(_, [])   +| l = l
(t, x:xs) +| l = Stream (t, x:|xs) l

infixr +|

-- | Extra list functions and list-like types.
module Data.List.Extra where

import Control.DeepSeq (NFData(..))
import Data.List (groupBy)

-- * Regular lists

-- | Split a list at an index and transform the two halves.
splitAtF :: ([a] -> b) -> ([a] -> c) -> Int -> [a] -> (b, c)
splitAtF f g i xs = let (l, r) = splitAt i xs in (f l, g r)

-- | Get the longest common prefix of a bunch of lists.
commonPrefix :: Eq a => [[a]] -> [a]
commonPrefix [] = []
commonPrefix ls = foldl1 commonPrefix2 ls where
  commonPrefix2 [] _ = []
  commonPrefix2 _ [] = []
  commonPrefix2 (x:xs) (y:ys)
    | x == y     = x : commonPrefix2 xs ys
    | otherwise = []

-- | Get the longest common suffix of a bunch of lists.
commonSuffix :: Eq a => [[a]] -> [a]
commonSuffix = reverse . commonPrefix . map reverse

-- | Like 'nubBy', but only compare adjacent elements.
nubishBy :: (a -> a -> Bool) -> [a] -> [a]
nubishBy eq = nubish' Nothing where
  nubish' _ [] = []
  nubish' Nothing (x:xs) = x : nubish' (Just x) xs
  nubish' e'@(Just e) (x:xs)
    | e `eq` x = nubish' e' xs
    | otherwise = x : nubish' (Just x) xs

-- | Check if a list has more than some number of elements.
moreThan :: [a] -> Int -> Bool
moreThan [] n = n < 0
moreThan _ 0  = True
moreThan (_:xs) n = moreThan xs (n-1)

-- | Like 'groupBy', but also handle things which are separated by a
-- couple of elements.
groupByIsh :: (a -> a -> Bool) -> [a] -> [[a]]
groupByIsh f = merge Nothing . merge Nothing . merge Nothing . groupBy f where
  merge Nothing (xs:ys:rest) = merge (Just (xs, ys)) rest
  merge Nothing groups = groups

  merge (Just (xs,ys)) (zs:zss)
    | head xs `f` head zs = merge (Just (xs ++ zs, ys)) zss
    | head ys `f` head zs = merge (Just (xs, ys ++ zs)) zss
    | otherwise = xs : merge (Just (ys, zs)) zss

  merge (Just (xs, ys)) zs = xs : ys : zs

-- * Non-empty lists
-- | This gets exposed to users of the library, so it has a bunch of
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

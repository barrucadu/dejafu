-- | Extra list functions and list-like types.
module Data.List.Extra where

import Control.DeepSeq (NFData(..))

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

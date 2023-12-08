module AoC.Common (
  (!?),
  windows,
  pairs,
  listTup2,
  listTup3,
  listTup4,
) where

import Data.List (tails)

-- Safe, strict, list index
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x : _) !? 0 = Just x
(x : xs) !? n = x `seq` (xs !? (n - 1))

-- | Sliding windows over a list.
windows :: Int -> [a] -> [[a]]
windows n = foldr (zipWith (:)) (repeat []) . take n . tails

-- | Sliding pairs over a list.
pairs :: [a] -> [(a, a)]
pairs (x : y : z) = (x, y) : pairs (y : z)
pairs _ = []

-- | Safely convert a list to a 2-tuple of the elements.
listTup2 :: [a] -> Maybe (a, a)
listTup2 [a, b] = Just (a, b)
listTup2 _ = Nothing

-- | Safely convert a list to a 3-tuple of the elements.
listTup3 :: [a] -> Maybe (a, a, a)
listTup3 [a, b, c] = Just (a, b, c)
listTup3 _ = Nothing

-- | Safely convert a list to a 4-tuple of the elements.
listTup4 :: [a] -> Maybe (a, a, a, a)
listTup4 [a, b, c, d] = Just (a, b, c, d)
listTup4 _ = Nothing

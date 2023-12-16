module AoC.Challenge.Day12
  ( day12a,
    day12b,
  )
where

import AoC.Common (listTup2)
import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Array (Array)
import qualified Data.Array as A
import Data.Bifunctor (bimap)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

parse :: String -> Either String [(Array Int Char, Array Int Int)]
parse =
  traverse
    ( fmap
        ( bimap
            (\l -> A.listArray (0, length l - 1) l)
            ((\l -> A.listArray (0, length l - 1) l) . fmap read . splitOn ",")
        )
        . maybeToEither "more than two entries on line"
        . listTup2
        . words
    )
    . lines

calcMap :: Array Int Char -> Array Int Int -> Map (Int, Int, Int) Int
calcMap row groups =
  m
  where
    (rMin, rMax) = A.bounds row
    (gMin, gMax) = A.bounds groups

    m :: Map (Int, Int, Int) Int
    m =
      M.mapWithKey (\k _ -> calc k) $
        M.fromList
          ( [ ((r, g, n), ())
              | r <- [rMin .. rMax],
                g <- [gMin .. gMax],
                n <- [0 .. (groups A.! g)]
            ]
              ++ [((r, gMax + 1, 0), ()) | r <- [rMin .. rMax]]
          )

    get :: (Int, Int, Int) -> Int
    get (r, g, n)
      | r == rMax + 1 =
          if (gMax + 1 == g && n == 0)
            || (gMax == g && n == groups A.! g)
            then 1
            else 0
      | otherwise =
          fromMaybe 0 (M.lookup (r, g, n) m)

    calc :: (Int, Int, Int) -> Int
    calc (r, g, n) =
      let brokenCount
            | g > gMax = 0
            | otherwise = get (r + 1, g, n + 1)
          operationalCount
            | n == 0 = get (r + 1, g, 0)
            | g <= gMax && n == groups A.! g = get (r + 1, g + 1, 0)
            | otherwise = 0
       in case row A.! r of
            '.' -> operationalCount
            '#' -> brokenCount
            '?' -> operationalCount + brokenCount
            _ -> error "Invalid condition"

solveA :: [(Array Int Char, Array Int Int)] -> Int
solveA =
  sum . fmap ((M.! (0, 0, 0)) . uncurry calcMap)

day12a :: Solution [(Array Int Char, Array Int Int)] Int
day12a = Solution {sParse = parse, sShow = show, sSolve = Right . solveA}

solveB :: [(Array Int Char, Array Int Int)] -> Int
solveB =
  sum
    . fmap
      ( (M.! (0, 0, 0))
          . uncurry calcMap
          . bimap mapRow mapGroup
      )
  where
    mapRow :: Array Int Char -> Array Int Char
    mapRow a =
      let (rMin, rMax) = A.bounds a
          l = A.elems a
       in A.listArray
            (rMin, (5 * (rMax - rMin + 1)) + 3)
            (intercalate "?" (replicate 5 l))

    mapGroup :: Array Int Int -> Array Int Int
    mapGroup a =
      let (rMin, rMax) = A.bounds a
          l = A.elems a
       in A.listArray
            (rMin, 5 * (rMax - rMin + 1) - 1)
            (concat $ replicate 5 l)

day12b :: Solution [(Array Int Char, Array Int Int)] Int
day12b = Solution {sParse = parse, sShow = show, sSolve = Right . solveB}

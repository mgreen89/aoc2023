module AoC.Challenge.Day21
  ( day21a,
    day21b,
  )
where

import AoC.Common.Point (boundingBox', cardinalNeighbs)
import AoC.Solution
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

type Point = V2 Int

parse :: String -> (Set Point, Point)
parse s =
  let gardenList =
        filter (\(_, c) -> c == '.' || c == 'S')
          . concat
          . zipWith (\y -> zipWith (\x -> (V2 x y,)) [0 ..]) [0 ..]
          . lines
          $ s
   in ( S.fromList . fmap fst $ gardenList,
        fst . head . filter (\(_, c) -> c == 'S') $ gardenList
      )

gardensAfterSteps :: (Point -> Bool) -> Point -> Int -> Int
gardensAfterSteps isGarden start steps =
  S.size
    . (!! steps)
    . iterate go
    $ S.singleton start
  where
    go :: Set Point -> Set Point
    go = S.filter isGarden . S.fromList . concatMap cardinalNeighbs . S.toList

solveA :: Int -> (Set Point, Point) -> Int
solveA n (gs, s) = gardensAfterSteps (`S.member` gs) s n

day21a :: Solution (Set Point, Point) Int
day21a = Solution {sParse = Right . parse, sShow = show, sSolve = Right . solveA 64}

{-
Another day where looking at the input seems to be required.
In the actual input there's a direct path to the edge moving N, S, E and W
from the start, which is in the center of a 131x131 grid.

Total number of steps is 26501365, which is 202300 * 131 + 65.

Given the above, the fastest way to the centre of each adjacent grid is
131 steps directly north, south, east and west, and to go to diagonally
adjacent grids just move at 90 degrees from the previous direction to give
the minimal manhattan distance.

So in 131 steps we can make it to the centre of cardinally adjacent grids,
and in 262 steps we can make it to the centre of diagonally adjacent grids.
This means we're going to get a massive diamond of grids we can reach
the center of.

This is going to be a quadratically increasing sequence.
Start from 65 steps, then 131 + 65, then (131 * 2) + 65, etc...
Calculate the first few terms and figure out the 202301st element.

The first few terms are [3955,35214,97607,191134,315795,471590]

This gives the quadratic sequence: 15567x^2 + 15692x + 3955

The 202300th entry in the sequence is 637087163925555.
-}

solveB :: (Set Point, Point) -> Int
solveB (gs, s) =
  const 637087163925555
  $ [ gardensAfterSteps isGarden s n | i <- [0..2], let n = 65 + (i * 131)]
  where
    (V2 xLo yLo, V2 xHi yHi) = fromJust . boundingBox' . S.toList $ gs
    (xLen, yLen) = (xHi - xLo + 1, yHi - yLo + 1)

    isGarden :: Point -> Bool
    isGarden (V2 x y) = S.member (V2 (x `mod` xLen) (y `mod` yLen)) gs

day21b :: Solution (Set Point, Point) Int
day21b = Solution {sParse = Right . parse, sShow = show, sSolve = Right . solveB}

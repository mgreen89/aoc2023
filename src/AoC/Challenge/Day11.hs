module AoC.Challenge.Day11
  ( day11a,
    day11b,
  )
where

import AoC.Common.Point (manhattan)
import AoC.Solution
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.List (tails)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

type Point = V2 Int

parse :: String -> Set Point
parse =
  S.fromList
    . mapMaybe (\(p, c) -> p <$ guard (c == '#'))
    . concat
    . zipWith
      (\y -> zipWith (\x -> (V2 x y,)) [0 ..])
      [0 ..]
    . lines

expand :: Int -> Set Point -> Set Point
expand n ps =
  let (V2 xMax yMax) = S.foldl' (liftA2 max) (V2 0 0) ps
      pList = S.toList ps
      blankXs = filter (\i -> all (\(V2 x _) -> x /= i) pList) [0 .. xMax]
      blankYs = filter (\i -> all (\(V2 _ y) -> y /= i) pList) [0 .. yMax]

      go :: Point -> Point
      go (V2 x y) =
        V2
          (x + (n - 1) * length (takeWhile (< x) blankXs))
          (y + (n - 1) * length (takeWhile (< y) blankYs))
   in S.map go ps

solve :: Int -> Set Point -> Int
solve n ps =
  sum [ manhattan p q | p : qs <- tails (S.toList (expand n ps)), q <- qs ]

day11a :: Solution (Set Point) Int
day11a = Solution {sParse = Right . parse, sShow = show, sSolve = Right . solve 2}

day11b :: Solution (Set Point) Int
day11b = Solution {sParse = Right . parse, sShow = show, sSolve = Right . solve 1000000}

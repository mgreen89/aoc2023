{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day11
  ( day11a,
  )
where

-- , day11b

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

expand :: Set Point -> Set Point
expand ps =
  let (V2 xMax yMax) = S.foldl' (liftA2 max) (V2 0 0) ps
      pList = S.toList ps
      blankXs = mapMaybe (\i -> i <$ guard (all (\(V2 x _) -> x /= i) pList)) [0 .. xMax]
      blankYs = mapMaybe (\i -> i <$ guard (all (\(V2 _ y) -> y /= i) pList)) [0 .. yMax]

      go :: Point -> Point
      go (V2 x y) = V2 (x + length (takeWhile (< x) blankXs)) (y + length (takeWhile (< y) blankYs))
   in S.map go ps

solveA :: Set Point -> Int
solveA ps =
  let allPairs :: [a] -> [(a, a)]
      allPairs = (concatMap pairsFromFirst . tails)
        where
          pairsFromFirst :: [a] -> [(a, a)]
          pairsFromFirst (a : b : xs) = (a, b) : pairsFromFirst (a : xs)
          pairsFromFirst _ = []
   in sum [manhattan p q | (p, q) <- allPairs (S.toList (expand ps))]

day11a :: Solution (Set Point) Int
day11a = Solution {sParse = Right . parse, sShow = show, sSolve = Right . solveA}

day11b :: Solution _ _
day11b = Solution {sParse = Right, sShow = show, sSolve = Right}

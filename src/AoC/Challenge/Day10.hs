{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day10
  ( day10a,
  )
where

-- , day10b

import AoC.Common.Graph (explore)
import AoC.Solution
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Debug.Trace
import Linear (V2 (..))

type Point = V2 Int

-- Parse to a sparse map of neighbours.
parse :: String -> (Point, Map Point [Point])
parse s =
  (head . M.keys . M.filter (== 'S') $ charMap, neighbMap)
  where
    neighbMap :: Map Point [Point]
    neighbMap = M.mapWithKey (getNeighbs charMap) charMap

    charMap :: Map Point Char
    charMap =
      M.filter (/= '.')
        $ M.fromList
          . concat
          . zipWith
            (\y -> zipWith (\x -> (V2 x y,)) [0 ..])
            [0 ..]
          . lines
        $ s

    bounds :: (Point, Point)
    bounds = (V2 0 0, maximum $ M.keys charMap)

    inBounds :: Point -> Bool
    inBounds p = p >= fst bounds && p <= snd bounds

    getNeighbs :: Map Point Char -> Point -> Char -> [Point]
    getNeighbs m p c =
      filter inBounds [p + d | d <- deltas]
      where
        deltas :: [Point]
        deltas = case c of
          '|' -> [V2 0 1, V2 0 (-1)]
          '-' -> [V2 1 0, V2 (-1) 0]
          'L' -> [V2 1 0, V2 0 (-1)]
          'J' -> [V2 (-1) 0, V2 0 (-1)]
          '7' -> [V2 (-1) 0, V2 0 1]
          'F' -> [V2 1 0, V2 0 1]
          'S' -> getStartNeighbs
          _ -> fail "Invalid input"

        getStartNeighbs :: [Point]
        getStartNeighbs =
          mapMaybe
            filterStartNeighbs
            [ (V2 1 0, "-7J"),
              (V2 (-1) 0, "-FL"),
              (V2 0 1, "|LJ"),
              (V2 0 (-1), "|7F")
            ]

        filterStartNeighbs :: (Point, [Char]) -> Maybe Point
        filterStartNeighbs (d, a) = do
          n <- m M.!? (p + d)
          guard (n `elem` a)
          pure d

solveA :: (Point, Map Point [Point]) -> Int
solveA (s, m) =
  maximum . M.elems . explore (M.fromList . (`zip` repeat 1) . (m M.!)) $ s

day10a :: Solution (Point, Map Point [Point]) Int
day10a =
  Solution
    { sParse = Right . parse,
      sShow = show,
      sSolve = Right . solveA
    }

day10b :: Solution _ _
day10b = Solution {sParse = Right, sShow = show, sSolve = Right}

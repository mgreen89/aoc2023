module AoC.Challenge.Day10
  ( day10a,
    day10b,
  )
where

import AoC.Solution
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
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

getLoop :: Point -> Map Point [Point] -> [Point]
getLoop s m =
  go s (head $ m M.! s) [s]
  where
    go prev curr path
      | curr == s = s : path
      | otherwise =
          let next = head . filter (/= prev) $ m M.! curr
           in go curr next (curr : path)

solveA :: (Point, Map Point [Point]) -> Int
solveA =
  (`div` 2) . length . uncurry getLoop

day10a :: Solution (Point, Map Point [Point]) Int
day10a =
  Solution
    { sParse = Right . parse,
      sShow = show,
      sSolve = Right . solveA
    }

-- Use the shoelace theorem to get the area of the polygon with the
-- given vertices.
shoeLace :: [Point] -> Int
shoeLace =
  abs . (`div` 2) . go
  where
    go :: [Point] -> Int
    go (V2 x1 y1 : p2@(V2 x2 y2) : xs) = (x1 * y2) - (x2 * y1) + go (p2 : xs)
    go _ = 0

solveB :: (Point, Map Point [Point]) -> Int
solveB (s, m) =
  let pipePoints = getLoop s m
   in -- Use Pick's theorem
      -- Area = (interior points) + (boundary points / 2) - 1
      --
      -- Want the interior points.
      shoeLace pipePoints - (length pipePoints `div` 2) + 1

day10b :: Solution (Point, Map Point [Point]) Int
day10b =
  Solution
    { sParse = Right . parse,
      sShow = show,
      sSolve = Right . solveB
    }

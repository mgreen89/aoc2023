module AoC.Challenge.Day18
  ( day18a,
    day18b,
  )
where

import AoC.Common (listTup2)
import AoC.Common.Point (Dir (..), dirPoint, shoeLace)
import AoC.Solution
import AoC.Util (maybeToEither)
import Control.Monad ((<=<))
import Data.Bitraversable (bitraverse)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Linear (V2 (..), (*^))
import Safe (headMay)
import Text.Read (readEither)

type Point = V2 Int

-- Use a combination of the shoelace theorem and Pick's theorem again.
-- Shoelace to find area, and then Pick's to find the sum of the internal
-- and boundary points.
--
-- A = i + b/2 - 1  ==>  i = A - b/2 + 1
solve :: [(Dir, Int)] -> Int
solve ds =
  internal + boundary
  where
    (boundaryPoints, boundary) = foldl' build (V2 0 0 NE.:| [], 0) $ ds
    area = shoeLace (NE.toList boundaryPoints)
    internal = area - (boundary `div` 2) + 1

    build :: (NonEmpty Point, Int) -> (Dir, Int) -> (NonEmpty Point, Int)
    build (ps@(p NE.:| _), tl) (d, l) = ((p + l *^ dirPoint d) NE.<| ps, tl + l)

day18a :: Solution [(Dir, Int)] Int
day18a =
  Solution
    { sParse =
        traverse (bitraverse readEither readEither)
          <=< maybeToEither "invalid line"
            . traverse
              ( listTup2
                  . take 2
                  . words
              )
            . lines,
      sShow = show,
      sSolve = Right . solve
    }

parseB :: String -> Either String [(Dir, Int)]
parseB =
  traverse parseInstruction
    <=< maybeToEither "invalid line"
      . traverse (headMay . drop 2 . words)
      . lines
  where
    parseInstruction :: String -> Either String (Dir, Int)
    parseInstruction
      ['(', '#', a, b, c, d, e, f, ')'] = do
        dir <- getDir f
        n <- readEither ['0', 'x', a, b, c, d, e]
        pure (dir, n)
    parseInstruction _ = Left "Invalid line"

    getDir :: Char -> Either String Dir
    getDir '0' = Right R
    getDir '1' = Right D
    getDir '2' = Right L
    getDir '3' = Right U
    getDir _ = Left "Invalid direction"

day18b :: Solution [(Dir, Int)] Int
day18b = Solution {sParse = parseB, sShow = show, sSolve = Right . solve}

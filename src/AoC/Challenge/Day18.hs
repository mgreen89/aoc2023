{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day18
  ( day18a,
  )
where

-- , day18b

import AoC.Common (listTup2)
import AoC.Common.Point (Dir (..), dirPoint, shoeLace)
import AoC.Solution
import AoC.Util (maybeToEither)
import Control.Monad ((<=<))
import Data.Bitraversable (bitraverse)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Debug.Trace
import Linear (V2 (..), (*^))
import Text.Read (readEither)

type Point = V2 Int

-- Use a combination of the shoelace theorem and Pick's theorem again.
-- Shoelace to find area, and then Pick's to find the sum of the internal
-- and boundary points.
--
-- A = i + b/2 - 1  ==>  i = A - b/2 + 1
solveA :: [(Dir, Int)] -> Int
solveA ds =
  internal + boundary
  where
    boundaryPoints = NE.toList . foldl' build (V2 0 0 NE.:| []) $ ds
    area = shoeLace boundaryPoints
    boundary = length boundaryPoints - 1
    internal = area - (boundary `div` 2) + 1

    build :: NonEmpty Point -> (Dir, Int) -> NonEmpty Point
    build ps (d, l) = go l d ps

    go :: Int -> Dir -> NonEmpty Point -> NonEmpty Point
    go 0 _ ps = ps
    go n d ps@(p NE.:| _) = go (n - 1) d ((p + dirPoint d) NE.<| ps)

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
      sSolve = Right . solveA
    }

day18b :: Solution _ _
day18b = Solution {sParse = Right, sShow = show, sSolve = Right}

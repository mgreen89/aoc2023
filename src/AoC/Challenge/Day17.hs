module AoC.Challenge.Day17
  ( day17a,
    day17b,
  )
where

import AoC.Common.Graph (aStar)
import AoC.Common.Point (Dir (..), dirPoint, manhattan, parse2dMap)
import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Linear (V2 (..))

type Point = V2 Int

solve :: Int -> Int -> Map Point Int -> Either String Int
solve minStep maxStep m =
  fmap fst
    . maybeToEither "aStar failed"
    . aStar (manhattan finish . fst) getNeighbs (start, Nothing)
    $ ((== finish) . fst)
  where
    start = V2 0 0
    finish = maximum $ M.keys m

    spanDir :: Point -> Dir -> [(Point, Int)]
    spanDir p d =
      NE.drop minStep
        . NE.reverse
        . go 1
        $ ((p, 0) NE.:| [])
      where
        go :: Int -> NonEmpty (Point, Int) -> NonEmpty (Point, Int)
        go i a@((lastP, lastC) NE.:| _)
          | i > maxStep = a
          | otherwise =
              let next = (lastP + dirPoint d)
               in case M.lookup next m of
                    Nothing -> a
                    Just c -> go (i + 1) ((next, lastC + c) NE.<| a)

    getNeighbs :: (Point, Maybe Dir) -> Map (Point, Maybe Dir) Int
    getNeighbs (p, md) =
      let possDirections =
            -- If there are no parents this is the first node, so allow
            -- right and down (start at top left)
            case md of
              Nothing ->
                -- Start, only move R and D.
                [R, D]
              Just d ->
                -- Change direction, and don't allow backtracking.
                [d <> L, d <> R]
       in M.fromList
            . concatMap (\d' -> first (,Just d') <$> spanDir p d')
            $ possDirections

day17a :: Solution (Map Point Int) Int
day17a = Solution {sParse = parse2dMap, sShow = show, sSolve = solve 1 3}

day17b :: Solution (Map Point Int) Int
day17b = Solution {sParse = parse2dMap, sShow = show, sSolve = solve 4 10}

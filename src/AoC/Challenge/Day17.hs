module AoC.Challenge.Day17
  ( day17a,
    day17b,
  )
where

import AoC.Common.Graph (aStarWithPath)
import AoC.Common.Point (Dir (..), dirPoint, dirRot, manhattan, parse2dMap)
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
    . aStarWithPath (manhattan finish . fst) getNeighbs (start, U)
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

    getNeighbs :: [(Point, Dir)] -> (Point, Dir) -> Map (Point, Dir) Int
    getNeighbs parents (p, d) =
      let possDirections =
            -- If there are no parents this is the first node, so allow
            -- right and down (start at top left)
            if null parents
              then -- Only move R and D.
                [R, D]
              else -- Change direction, and don't allow backtracking.
                [dirRot L d, dirRot R d]
       in M.fromList
            . concatMap (\d' -> first (,d') <$> spanDir p d')
            $ possDirections

day17a :: Solution (Map Point Int) Int
day17a = Solution {sParse = parse2dMap, sShow = show, sSolve = solve 1 3}

day17b :: Solution (Map Point Int) Int
day17b = Solution {sParse = parse2dMap, sShow = show, sSolve = solve 4 10}

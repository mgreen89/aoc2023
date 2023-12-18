{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day17
  ( day17a,
  )
where

-- , day17b

import AoC.Common.Graph (aStarWithPath)
import AoC.Common.Point (Dir (..), dirPoint, dirRot, manhattan, parse2dMap)
import AoC.Solution
import AoC.Util (maybeToEither)
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, maybeToList)
import Linear (V2 (..))

type Point = V2 Int

solveA :: Map Point Int -> Either String Int
solveA m =
  fmap fst
    . maybeToEither "aStar failed"
    . aStarWithPath (manhattan finish . fst) getNeighbs (start, U)
    $ ((== finish) . fst)
  where
    start = V2 0 0
    finish = maximum $ M.keys m

    getNeighbs :: [(Point, Dir)] -> (Point, Dir) -> Map (Point, Dir) Int
    getNeighbs parents (p, d) =
      let possDirections =
            -- If there are no parents this is the first node, so allow
            -- right and down (start at top left)
            if null parents
              then -- Only move R and D.
                [R, D]
              else -- Change direction, and don't allow backtracking.
                filter (/= d) . filter (/= dirRot D d) $ [R, D, L, U]

          getNexts :: Dir -> [((Point, Dir), Int)]
          getNexts d' =
            tail . reverse . go 1 $ [((p, d), 0)]
            where
              dirMove = dirPoint d'
              go :: Int -> [((Point, Dir), Int)] -> [((Point, Dir), Int)]
              go n as@(((p', _), c) : _)
                | n > 3 = as
                | otherwise =
                    case M.lookup (p' + dirMove) m of
                      Nothing -> as
                      Just inc -> go (n + 1) (((p' + dirMove, d'), c + inc) : as)
              go _ _ = error "failed"
       in M.fromList . concatMap getNexts $ possDirections

day17a :: Solution (Map Point Int) Int
day17a = Solution {sParse = parse2dMap, sShow = show, sSolve = solveA}

day17b :: Solution _ _
day17b = Solution {sParse = Right, sShow = show, sSolve = Right}

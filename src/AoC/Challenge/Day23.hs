{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day23
  ( day23a,
  )
where

-- day23a
-- , day23b

import AoC.Common.Point (Dir (..), boundingBox', cardinalNeighbs, dirPoint)
import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, mapMaybe)
import GHC.Generics (Generic)
import Linear (V2 (..))

type Point = V2 Int

data Trail
  = Path
  | Steep Dir
  deriving (Eq, Generic, NFData, Ord, Show)

parse :: String -> Map Point Trail
parse =
  M.fromList
    . mapMaybe toTrail
    . concat
    . zipWith
      (\y -> zipWith (\x -> (V2 x y,)) [0 ..])
      [0 ..]
    . lines
  where
    toTrail :: (Point, Char) -> Maybe (Point, Trail)
    toTrail (p, c) =
      case c of
        '.' -> Just (p, Path)
        '>' -> Just (p, Steep R)
        'v' -> Just (p, Steep D)
        '<' -> Just (p, Steep L)
        '^' -> Just (p, Steep U)
        _ -> Nothing

type PT = (Point, Trail)

dfs :: PT -> PT -> (PT -> Map PT Int) -> Maybe Int
dfs start end getNeighbs =
  (M.!? end) . go M.empty start $ 0
  where
    go :: Map PT Int -> PT -> Int -> Map PT Int
    go seen pt c =
      M.unionsWith max . M.mapWithKey (go' c seen) $ getNeighbs pt

    go' :: Int -> Map PT Int -> PT -> Int -> Map PT Int
    go' c m pt nc =
      case M.lookup pt m of
        Nothing -> go (M.insert pt (c + nc) m) pt (c + nc)
        Just _ -> m -- loop!

solveA :: Map Point Trail -> Int
solveA m =
  fromJust . dfs start end $ getNeighbs
  where
    bb@(V2 xLo yLo, V2 xHi yHi) = fromJust . boundingBox' . M.keys $ m
    start = head . M.toList . M.filterWithKey (\(V2 _ y) _ -> y == yLo) $ m
    end = head . M.toList . M.filterWithKey (\(V2 _ y) _ -> y == yHi) $ m

    getNeighbs :: (Point, Trail) -> Map (Point, Trail) Int
    getNeighbs (p, t) =
      let candidates = case t of
            Path -> cardinalNeighbs p
            Steep d -> [p + dirPoint d]
       in M.fromList
            . fmap (,1)
            . mapMaybe (\c -> (c,) <$> M.lookup c m)
            $ candidates

day23a :: Solution (Map Point Trail) Int
day23a = Solution {sParse = Right . parse, sShow = show, sSolve = Right . solveA}

day23b :: Solution _ _
day23b = Solution {sParse = Right, sShow = show, sSolve = Right}

{-# LANGUAGE ScopedTypeVariables #-}

module AoC.Challenge.Day23
  ( day23a,
    day23b,
  )
where

import AoC.Common.Point (Dir (..), boundingBox', cardinalNeighbs, dirPoint)
import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
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

dfs :: forall a p. (Num a, Ord a, Ord p) => p -> p -> (p -> Map p a) -> Maybe a
dfs start end getNeighbs =
  (M.!? end) . go M.empty start $ 0
  where
    go :: Map p a -> p -> a -> Map p a
    go seen p c =
      M.unionsWith max . M.mapWithKey (go' c seen) $ getNeighbs p

    go' :: a -> Map p a -> p -> a -> Map p a
    go' c m p nc =
      case M.lookup p m of
        Nothing -> go (M.insert p (c + nc) m) p (c + nc)
        Just _ -> m -- loop!

solveA :: Map Point Trail -> Int
solveA m =
  fromJust . dfs start end $ getNeighbs
  where
    (V2 _ yLo, V2 _ yHi) = fromJust . boundingBox' . M.keys $ m
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

-- Pre-compute a map that contains only nodes for:
--  - The start and end nodes.
--  - Any node with a choice of neighbours (i.e. any node with 3 or 4 valid
--    neighbours).
--
-- As the trails are largely 1 tile wide, and are often long and squiggly,
-- this should result in a much faster search.
generateGraph :: Map Point Trail -> Map Point [(Point, Int)]
generateGraph inp =
  M.foldlWithKey' go M.empty inp
  where
    (V2 _ yLo, V2 _ yHi) = fromJust . boundingBox' . M.keys $ inp

    junctions :: Set Point
    junctions = S.filter ((> 2) . length . filter (`M.member` inp) . cardinalNeighbs) (M.keysSet inp)

    go :: Map Point [(Point, Int)] -> Point -> Trail -> Map Point [(Point, Int)]
    go m p@(V2 _ y) _
      | y == yLo || y == yHi || S.member p junctions = M.insert p (genNode p) m
      | otherwise = m

    genNode :: Point -> [(Point, Int)]
    genNode p =
      fmap (getNextJunction p 0)
        . filter (`M.member` inp)
        $ cardinalNeighbs p

    getNextJunction :: Point -> Int -> Point -> (Point, Int)
    getNextJunction l c p@(V2 _ y)
      | y == yLo || y == yHi = (p, c + 1)
      | otherwise =
          let candidates =
                filter (/= l)
                  . filter (`M.member` inp)
                  $ cardinalNeighbs p
           in if length candidates > 1
                then (p, c + 1)
                else getNextJunction p (c + 1) (head candidates)

solveB :: Map Point Trail -> Int
solveB m =
  fromJust . dfs start end $ getNeighbs
  where
    (V2 _ yLo, V2 _ yHi) = fromJust . boundingBox' . M.keys $ m
    start = fst . head . M.toList . M.filterWithKey (\(V2 _ y) _ -> y == yLo) $ m
    end = fst . head . M.toList . M.filterWithKey (\(V2 _ y) _ -> y == yHi) $ m
    graph = generateGraph m

    getNeighbs :: Point -> Map Point Int
    getNeighbs p = M.fromList $ fromMaybe [] (M.lookup p graph)

day23b :: Solution (Map Point Trail) Int
day23b = Solution {sParse = Right . parse, sShow = show, sSolve = Right . solveB}

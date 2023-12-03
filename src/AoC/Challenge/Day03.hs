module AoC.Challenge.Day03
  ( day03a,
    day03b,
  )
where

import AoC.Common.Point (allNeighbs)
import AoC.Solution
import Data.Char (digitToInt, isDigit)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Linear (V2 (..))

type PartNo = (Int, V2 Int, Int)

type Symbol = (Char, V2 Int)

type Input = ([PartNo], [Symbol])

data Chunk
  = Part Int Int
  | Symbol Char
  | Blank

chunk :: String -> [[Chunk]]
chunk =
  fmap (foldl go []) . lines
  where
    go :: [Chunk] -> Char -> [Chunk]
    go a c
      | isDigit c = case a of
          (Part i l) : x -> Part ((i * 10) + digitToInt c) (l + 1) : x
          y -> Part (digitToInt c) 1 : y
      | c == '.' = Blank : a
      | otherwise = Symbol c : a

parse :: String -> Input
parse =
  foldr go ([], []) . zip [0 ..] . chunk
  where
    go :: (Int, [Chunk]) -> Input -> Input
    go (y, cs) (ps, ss) =
      snd . foldr go' (0, (ps, ss)) $ cs
      where
        go' :: Chunk -> (Int, Input) -> (Int, Input)
        go' ch (x, (ps', cs')) = case ch of
          Part i l -> (x + l, ((i, V2 x y, l) : ps', cs'))
          Symbol c -> (x + 1, (ps', (c, V2 x y) : cs'))
          Blank -> (x + 1, (ps', cs'))

partA :: Input -> Int
partA (ps, ss) =
  sum . fmap fst . M.elems . M.filterWithKey nextToSymbol $ partNos
  where
    partNos :: M.Map (V2 Int) (Int, Int)
    partNos = M.fromList . fmap (\(i, s, l) -> (s, (i, l))) $ ps

    symbols :: M.Map (V2 Int) Char
    symbols = M.fromList . fmap (\(c, p) -> (p, c)) $ ss

    nextToSymbol :: V2 Int -> (Int, Int) -> Bool
    nextToSymbol (V2 x y) (_, l) =
      any (any (`M.member` symbols) . allNeighbs) $
        [V2 (x + dx) y | dx <- [0 .. (l - 1)]]

day03a :: Solution Input Int
day03a = Solution {sParse = Right . parse, sShow = show, sSolve = Right . partA}

partB :: Input -> Int
partB (ps, ss) =
  sum . fmap (S.foldr (*) 1) . M.elems . M.filter ((== 2) . S.size) . M.mapWithKey getPartNos . M.filter (== '*') $ symbols
  where
    partNos :: M.Map (V2 Int) Int
    partNos = M.unions . fmap (\(i, V2 x y, l) -> M.fromList [(V2 (x + dx) y, i) | dx <- [0 .. (l - 1)]]) $ ps

    symbols :: M.Map (V2 Int) Char
    symbols = M.fromList . fmap (\(c, p) -> (p, c)) $ ss

    getPartNos :: V2 Int -> Char -> IntSet
    getPartNos p '*' = S.fromList . mapMaybe (`M.lookup` partNos) . allNeighbs $ p
    getPartNos _ _ = S.empty

day03b :: Solution Input Int
day03b = Solution {sParse = Right . parse, sShow = show, sSolve = Right . partB}

module AoC.Challenge.Day03
  ( day03a,
    day03b,
  )
where

import AoC.Common.Point (allNeighbs)
import AoC.Solution
import Data.Char (digitToInt, isDigit)
import Data.Foldable (foldl')
import qualified Data.IntSet as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Linear (V2 (..))

type Input = (Map (V2 Int) Int, Map (V2 Int) Char)

data Chunk
  = Part Int Int
  | Symbol Char
  | Blank
  deriving (Show)

chunk :: String -> [[Chunk]]
chunk =
  fmap (reverse . foldl' go []) . lines
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
  foldl' parseLine (M.empty, M.empty) . zip [0 ..] . chunk
  where
    parseLine :: Input -> (Int, [Chunk]) -> Input
    parseLine (ps, ss) (y, cs) =
      snd . foldl' (parseChunk y) (0, (ps, ss)) $ cs

    parseChunk :: Int -> (Int, Input) -> Chunk -> (Int, Input)
    parseChunk y (x, (ps, cs)) ch = case ch of
      Part i l ->
        ( x + l,
          ( M.union
              ps
              (M.fromList [(V2 (x + dx) y, i) | dx <- [0 .. (l - 1)]]),
            cs
          )
        )
      Symbol c -> (x + 1, (ps, M.insert (V2 x y) c cs))
      Blank -> (x + 1, (ps, cs))

partA :: Input -> Int
partA (parts, syms) =
  sum
    . S.toList
    . S.unions
    . fmap (S.fromList . mapMaybe (`M.lookup` parts) . allNeighbs)
    . M.keys
    $ syms

day03a :: Solution Input Int
day03a = Solution {sParse = Right . parse, sShow = show, sSolve = Right . partA}

partB :: Input -> Int
partB (parts, syms) =
  sum
    . fmap (product . S.toList)
    . filter ((== 2) . S.size)
    . fmap (S.fromList . mapMaybe (`M.lookup` parts) . allNeighbs)
    . M.keys
    . M.filter (== '*')
    $ syms

day03b :: Solution Input Int
day03b = Solution {sParse = Right . parse, sShow = show, sSolve = Right . partB}

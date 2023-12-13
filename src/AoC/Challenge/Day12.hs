{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day12
  ( day12a,
  )
where

-- , day12b

import AoC.Common (listTup2)
import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Array (Array)
import qualified Data.Array as A
import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M

parse :: String -> Either String [(Array Int Char, Array Int Int)]
parse =
  traverse
    ( fmap
        ( bimap
            (\l -> A.listArray (0, length l - 1) l)
            ((\l -> A.listArray (0, length l - 1) l) . fmap read . splitOn ",")
        )
        . maybeToEither "more than two entries on line"
        . listTup2
        . words
    )
    . lines

calc :: Array Int Char -> Array Int Int -> Int -> Int -> Int -> Int
calc row groups iRow iGroup nSoFar
  | iRow == snd (A.bounds row) + 1 =
      if (snd (A.bounds groups) + 1 == iGroup && nSoFar == 0)
        || (snd (A.bounds groups) == iGroup && nSoFar == groups A.! iGroup)
        then 1
        else 0
  | otherwise =
      let brokenCount = calc row groups (iRow + 1) iGroup (nSoFar + 1)
          operationalCount
            | nSoFar == 0 = calc row groups (iRow + 1) iGroup 0
            | iGroup <= snd (A.bounds groups) && nSoFar == groups A.! iGroup =
                calc row groups (iRow + 1) (iGroup + 1) 0
            | otherwise = 0
       in case row A.! iRow of
            '.' -> operationalCount
            '#' -> brokenCount
            '?' -> operationalCount + brokenCount
            _ -> error "Invalid condition"

solveA :: [(Array Int Char, Array Int Int)] -> Int
solveA =
  sum . fmap (\(a, b) -> calc a b 0 0 0)

day12a :: Solution [(Array Int Char, Array Int Int)] Int
day12a = Solution {sParse = parse, sShow = show, sSolve = Right . solveA}

day12b :: Solution _ _
day12b = Solution {sParse = Right, sShow = show, sSolve = Right}

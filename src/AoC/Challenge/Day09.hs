{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day09
  ( day09a,
  )
where

-- , day09b

import AoC.Solution
import Data.Foldable (foldl')
import Text.Read (readEither)

parse :: String -> Either String [[Int]]
parse = traverse (traverse readEither . words) . lines

stepBack :: [Int] -> Int
stepBack =
  sum . go []
  where
    go :: [Int] -> [Int] -> [Int]
    go a (x : xs) =
      go (x : a) (zipWith (-) (x : xs) xs)
    go a [] = a

day09a :: Solution [[Int]] Int
day09a =
  Solution
    { sParse = parse,
      sShow = show,
      sSolve = Right . sum . fmap (stepBack . reverse)
    }

day09b :: Solution _ _
day09b = Solution {sParse = Right, sShow = show, sSolve = Right}

module AoC.Challenge.Day09
  ( day09a,
    day09b,
  )
where

import AoC.Solution
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

day09b :: Solution [[Int]] Int
day09b =
  Solution
    { sParse = parse,
      sShow = show,
      sSolve = Right . sum . fmap stepBack
    }

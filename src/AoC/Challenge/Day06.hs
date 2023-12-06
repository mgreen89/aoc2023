{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day06
  ( day06a,
  )
where

-- , day06b

import AoC.Common (listTup2)
import AoC.Solution
import AoC.Util (maybeToEither)
import Text.Read (readMaybe)

parse :: String -> Maybe [(Int, Int)]
parse =
  fmap (uncurry zip)
    . (>>= listTup2)
    . traverse (traverse readMaybe . drop 1 . words)
    . take 2
    . lines

getWinners :: Int -> Int -> Int
getWinners t d =
  sum [if (t - x) * x > d then 1 else 0 | x <- [1 .. (t - 1)]]

day06a :: Solution [(Int, Int)] Int
day06a =
  Solution
    { sParse = maybeToEither "parse error" . parse,
      sShow = show,
      sSolve = Right . product . fmap (uncurry getWinners)
    }

day06b :: Solution _ _
day06b = Solution {sParse = Right, sShow = show, sSolve = Right}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day15
  ( day15a,
  )
where

-- , day15b

import AoC.Solution
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.List.Split (splitOn)

hash :: String -> Int
hash = foldl' go 0
  where
    go a = (`mod` 256) . (* 17) . (+ a) . ord

day15a :: Solution [String] Int
day15a =
  Solution
    { sParse = Right . splitOn ",",
      sShow = show,
      sSolve = Right . sum . fmap hash
    }

day15b :: Solution _ _
day15b = Solution {sParse = Right, sShow = show, sSolve = Right}

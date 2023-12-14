{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day14
  ( day14a,
  )
where

-- , day14b

import AoC.Solution
import Data.Foldable (foldl')
import Data.List (transpose)

data State = State
  { pos :: Int,
    posLast :: Int,
    nCurr :: Int,
    nTot :: Int,
    acc :: Int
  }

solveA :: [String] -> Int
solveA = sum . fmap handleLine
  where
    handleLine :: String -> Int
    handleLine =
      calc . roll . foldl' go (State 0 0 0 0 0)

    calc :: State -> Int
    calc s = (s.nTot * (s.pos - 1)) - s.acc

    go :: State -> Char -> State
    go s '#' = roll s
    go s 'O' = State (s.pos + 1) s.posLast (s.nCurr + 1) s.nTot s.acc
    go s '.' = State (s.pos + 1) s.posLast s.nCurr s.nTot s.acc
    go _ _ = error "Invalid input"

    roll :: State -> State
    roll s =
      if s.nCurr > 0
        then State (s.pos + 1) (s.pos + 1) 0 (s.nCurr + s.nTot) (s.acc + sum (take s.nCurr [s.posLast ..]))
        else State (s.pos + 1) (s.pos + 1) 0 s.nTot s.acc

day14a :: Solution [String] Int
day14a = Solution {sParse = Right . transpose . lines, sShow = show, sSolve = Right . solveA}

day14b :: Solution _ _
day14b = Solution {sParse = Right, sShow = show, sSolve = Right}

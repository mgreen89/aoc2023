{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day21 (
  day21a,
  )
where

-- , day21b

import AoC.Solution
import AoC.Common.Point (cardinalNeighbs)
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2(..))
import Data.Maybe (fromJust)

type Point = V2 Int

parse :: String -> (Set Point, Point)
parse s =
  let 
    gardenList =
      filter (\(_, c) -> c == '.' || c == 'S')
      . concat
      . zipWith (\y -> zipWith (\x -> (V2 x y,)) [0 ..]) [0 ..]
      . lines
      $ s
  in
  (S.fromList . fmap fst $ gardenList,
   fst . head . filter (\(_, c) -> c == 'S') $ gardenList
   )

solveA :: Int -> (Set Point, Point) -> Int
solveA n (gs, s) =
  S.size
  . (!! n)
  . iterate go
  $ (S.singleton s)
  where
    go :: Set Point -> Set Point
    go = S.intersection gs . S.fromList . concatMap (cardinalNeighbs) . S.toList
    
day21a :: Solution (Set Point, Point) Int
day21a = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solveA 64 }

day21b :: Solution _ _
day21b = Solution{sParse = Right, sShow = show, sSolve = Right}

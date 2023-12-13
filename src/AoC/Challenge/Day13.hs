{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day13
  ( day13a,
  )
where

-- , day13b

import AoC.Solution
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Foldable (foldl')
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

type Point = V2 Int

parse :: String -> [Set Point]
parse =
  fmap go . splitOn "\n\n"
  where
    go =
      S.fromList
        . mapMaybe (\(p, c) -> if c == '#' then Just p else Nothing)
        . concat
        . zipWith (\y -> zipWith (\x -> (V2 x y,)) [0 ..]) [0 ..]
        . lines

firstJust :: [Maybe a] -> Maybe a
firstJust (Just x : _) = Just x
firstJust (Nothing : rest) = firstJust rest
firstJust [] = Nothing

findReflections :: Set Point -> Int
findReflections ps =
  let x = firstJust [(* 100) <$> check yMax rows, check xMax cols]
   in case x of
        Just f -> f
        Nothing -> error "failed!!"
  where
    (V2 xMax yMax) = S.foldl' (liftA2 max) 0 ps

    cols :: [Int]
    cols =
      [ construct ys
        | i <- [0 .. xMax],
          let ys = (\(V2 _ y) -> y) <$> S.toList (S.filter (\(V2 x _) -> x == i) ps)
      ]

    rows :: [Int]
    rows =
      [ construct xs
        | i <- [0 .. yMax],
          let xs = (\(V2 x _) -> x) <$> S.toList (S.filter (\(V2 _ y) -> y == i) ps)
      ]

    construct :: (Foldable f) => f Int -> Int
    construct = foldl' (\a q -> a + 2 ^ q) 0

    check :: Int -> [Int] -> Maybe Int
    check cMax cs =
      firstJust [go i | i <- [1 .. cMax]]
      where
        go a =
          let (lt, gt) = splitAt a cs
           in a <$ guard (and (zipWith (==) (reverse lt) gt))

day13a :: Solution [Set Point] Int
day13a =
  Solution
    { sParse = Right . parse,
      sShow = show,
      sSolve = Right . sum . fmap findReflections
    }

day13b :: Solution _ _
day13b = Solution {sParse = Right, sShow = show, sSolve = Right}

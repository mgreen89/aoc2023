module AoC.Challenge.Day13
  ( day13a,
    day13b,
  )
where

import AoC.Solution
import AoC.Util (maybeToEither)
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Foldable (foldl')
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
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

findReflection :: Set Point -> [Int]
findReflection ps =
  ((* 100) <$> check yMax rows) <> check xMax cols
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

    check :: Int -> [Int] -> [Int]
    check cMax cs =
      catMaybes [go i | i <- [1 .. cMax]]
      where
        go a =
          let (lt, gt) = splitAt a cs
           in a <$ guard (and (zipWith (==) (reverse lt) gt))

day13a :: Solution [Set Point] Int
day13a =
  Solution
    { sParse = Right . parse,
      sShow = show,
      sSolve = fmap sum . traverse (maybeToEither "didn't find reflection" . listToMaybe . findReflection)
    }

solveB :: [Set Point] -> Int
solveB =
  sum . fmap go
  where
    go :: Set Point -> Int
    go ps =
      let (V2 xMax yMax) = S.foldl' (liftA2 max) 0 ps
          initSol = head $ findReflection ps
       in head . filter (/= initSol) . concat $
            [ findReflection ps'
              | x <- [0 .. xMax],
                y <- [0 .. yMax],
                let smudge = V2 x y,
                let ps' =
                      if S.member smudge ps
                        then S.delete smudge ps
                        else S.insert smudge ps
            ]

day13b :: Solution [Set Point] Int
day13b = Solution {sParse = Right . parse, sShow = show, sSolve = Right . solveB}

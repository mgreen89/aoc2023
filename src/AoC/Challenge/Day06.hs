module AoC.Challenge.Day06
  ( day06a,
    day06b,
  )
where

import AoC.Common (listTup2)
import AoC.Solution
import AoC.Util (maybeToEither)
import Safe (headMay)
import Text.Read (readMaybe)

parseA :: String -> Maybe [(Int, Int)]
parseA =
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
    { sParse = maybeToEither "parse error" . parseA,
      sShow = show,
      sSolve = Right . product . fmap (uncurry getWinners)
    }

parseB :: String -> Maybe (Int, Int)
parseB =
  (>>= listTup2) . traverse (readMaybe . concat . drop 1 . words) . take 2 . lines

{- Time taken, where T is total time and x is time spent charging is,
   x(T - x).

   Differentiating this gives T - 2x, so the inflection point is where
   x = T/2, which gives the max distance.

   So, starting from 1 second charging, find the first point where you win.
   If you call this w, the # of winners is len(range(w, T-w)) == T-2w
   (since it's symmetrical about T/2).
-}
getWinnersEfficient :: Int -> Int -> Maybe Int
getWinnersEfficient t d =
  fmap ((\w -> t - (2 * w) + 1) . fst)
    . headMay
    . dropWhile (\(_, s) -> s < d)
    . fmap (\x -> (x, x * (t - x)))
    $ [1 ..]

day06b :: Solution (Int, Int) Int
day06b =
  Solution
    { sParse = maybeToEither "parse error" . parseB,
      sShow = show,
      sSolve = maybeToEither "no winners!" . uncurry getWinnersEfficient
    }

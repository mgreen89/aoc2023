module AoC.Challenge.Day01
  ( day01a,
    day01b,
  )
where

import AoC.Solution
import AoC.Util (maybeToEither)
import Control.Monad (guard)
import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (listToMaybe, mapMaybe)
import Safe (headMay, lastMay)

getVal :: [Int] -> Maybe Int
getVal xs =
  (\x y -> x * 10 + y) <$> headMay xs <*> lastMay xs

digitToIntMay :: Char -> Maybe Int
digitToIntMay c
  | isDigit c = Just $ digitToInt c
  | otherwise = Nothing

digitLookup :: [(String, Int)]
digitLookup =
  [(show x, x) | x <- [0 .. 9]]
    ++ [ ("zero", 0),
         ("one", 1),
         ("two", 2),
         ("three", 3),
         ("four", 4),
         ("five", 5),
         ("six", 6),
         ("seven", 7),
         ("eight", 8),
         ("nine", 9)
       ]

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

strToIntMay :: String -> Maybe Int
strToIntMay inp =
  firstJust (\(s, v) -> v <$ guard (s `isPrefixOf` inp)) digitLookup

day01a :: Solution [String] Int
day01a =
  Solution
    { sParse = Right . lines,
      sShow = show,
      sSolve =
        maybeToEither "Invalid line"
          . fmap sum
          . traverse (getVal . mapMaybe digitToIntMay)
    }

day01b :: Solution [String] Int
day01b =
  Solution
    { sParse = Right . lines,
      sShow = show,
      sSolve =
        maybeToEither "Invalid line"
          . fmap sum
          . traverse (getVal . mapMaybe strToIntMay . tails)
    }

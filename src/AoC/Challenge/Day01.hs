{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day01 (
  day01a
  )
where

-- , day01b

import AoC.Solution
import Data.Char (isDigit, digitToInt)
import Data.Maybe (mapMaybe)
import Safe (headMay, lastMay)
import AoC.Util (maybeToEither)

getVal :: [Int] -> Maybe Int
getVal xs =
  (\x y -> x * 10 + y) <$> headMay xs <*> lastMay xs

digitToIntMay :: Char -> Maybe Int
digitToIntMay c
  | isDigit c = Just $ digitToInt c
  | otherwise = Nothing

partA :: [String] -> Maybe Int
partA =
  fmap sum . traverse (getVal . mapMaybe digitToIntMay)

day01a :: Solution [String] Int
day01a = Solution{sParse = Right . lines, sShow = show, sSolve = maybeToEither "Invalid line" . partA}

day01b :: Solution _ _
day01b = Solution{sParse = Right, sShow = show, sSolve = Right}

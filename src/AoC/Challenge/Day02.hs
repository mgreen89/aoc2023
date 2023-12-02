{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day02
  ( day02a,
  )
where

-- , day02b

import AoC.Solution
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Void (Void)
import Linear (V3 (..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

type Pull = V3 Int

type Game = (Int, [Pull])

parser :: MP.Parsec Void String [Game]
parser = do
  gameParser `MP.sepBy` MP.char '\n'
  where
    gameParser :: MP.Parsec Void String Game
    gameParser = do
      i <- MP.string "Game " *> MPL.decimal <* MP.char ':'
      ps <- MP.many pullParser
      pure (i, ps)

    pullParser :: MP.Parsec Void String Pull
    pullParser = do
      MP.char ' '
      cs <- colorCountParser `MP.sepBy` MP.string ", "
      MP.optional (MP.char ';')
      pure (sum cs)

    colorCountParser :: MP.Parsec Void String Pull
    colorCountParser = do
      n <- MPL.decimal <* MP.char ' '
      c <- colorParser
      pure (c n)

    colorParser :: MP.Parsec Void String (Int -> Pull)
    colorParser = do
      MP.choice
        [ MP.string "red" $> (\x -> V3 x 0 0),
          MP.string "green" $> (\y -> V3 0 y 0),
          MP.string "blue" $> (\z -> V3 0 0 z)
        ]

parseInput :: String -> Either String [Game]
parseInput =
  first MP.errorBundlePretty . MP.parse parser "day02"

isPossible :: Pull -> Bool
isPossible (V3 r g b) = r <= 12 && g <= 13 && b <= 14

partA :: [Game] -> Int
partA =
  sum . map fst . filter (\(_, ps) -> all isPossible ps)

day02a :: Solution _ _
day02a = Solution {sParse = parseInput, sShow = show, sSolve = Right . partA}

day02b :: Solution _ _
day02b = Solution {sParse = Right, sShow = show, sSolve = Right}

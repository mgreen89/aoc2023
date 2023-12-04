{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day04
  ( day04a,
  )
where

-- , day04b

import AoC.Solution
import Data.Bifunctor (first)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

type Card = (Int, IntSet, IntSet)

parser :: MP.Parsec Void String [Card]
parser = do
  MP.many cardParser
  where
    cardParser :: MP.Parsec Void String Card
    cardParser = do
      cardNo <- MP.string "Card" *> MP.space1 *> MPL.decimal <* MP.string ":"
      MP.space
      winners <- MP.many (MPL.decimal <* MP.space)
      MP.string "|"
      MP.space
      have <- MP.many (MPL.decimal <* MP.space)
      pure (cardNo, S.fromList winners, S.fromList have)

parse :: String -> Either String [Card]
parse =
  first MP.errorBundlePretty . MP.parse parser "day04"

day04a :: Solution [Card] Int
day04a =
  Solution
    { sParse = parse,
      sShow = show,
      sSolve =
        Right
          . sum
          . fmap
            ( (\x -> if x == 0 then 0 else 2 ^ (x - 1))
                . S.size
                . \(_, w, h) -> S.intersection w h
            )
    }

day04b :: Solution _ _
day04b = Solution {sParse = Right, sShow = show, sSolve = Right}

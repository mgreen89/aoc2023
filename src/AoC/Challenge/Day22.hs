{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day22
  ( day22a,
  )
where

-- , day22b

import AoC.Solution
import Data.Bifunctor (first)
import Data.Void (Void)
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V3 (..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

type Point = V3 Int

type Brick = (Point, Point)

bricksParser :: MP.Parsec Void String [Brick]
bricksParser = MP.sepBy brickParser MP.space1
  where
    brickParser :: MP.Parsec Void String Brick
    brickParser = do
      p1 <- pointParser
      MP.char '~'
      p2 <- pointParser
      pure (p1, p2)

    pointParser :: MP.Parsec Void String Point
    pointParser = do
      x <- MPL.decimal <* MP.char ','
      y <- MPL.decimal <* MP.char ','
      z <- MPL.decimal
      pure $ V3 x y z

parse :: String -> Either String [Brick]
parse =
  first MP.errorBundlePretty . MP.parse bricksParser "day22"

-- | Repeat a function until you get the same result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x
  | x == y = x
  | otherwise = fixedPoint f y
  where
    y = f x

solveA :: [Brick] -> Int
solveA bs =
  
  where
    fallen :: Set Brick
    fallen = fixedPoint go (S.fromList bs)

    go :: Set Brick -> Set Brick
    go 

day22a :: Solution [Brick] Int
day22a = Solution {sParse = parse, sShow = show, sSolve = Right}

day22b :: Solution _ _
day22b = Solution {sParse = Right, sShow = show, sSolve = Right}

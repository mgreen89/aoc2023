{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day05
  ( day05a,
  )
where

-- , day05b

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

data Input = Input {seeds :: [Int], maps :: Map String (String, [(Int, Int, Int)])}
  deriving (Show, Generic, NFData)

parser :: MP.Parsec Void String Input
parser = do
  seeds <- MP.string "seeds: " *> MP.many (MPL.decimal <* MP.space)
  maps <- MP.many mapParser
  pure $ Input seeds (M.fromList maps)
  where
    mapParser :: MP.Parsec Void String (String, (String, [(Int, Int, Int)]))
    mapParser = do
      src <- MP.manyTill MP.lowerChar (MP.char '-') <* MP.string "to-"
      dst <- MP.many MP.lowerChar <* MP.space <* MP.string "map:\n"
      ranges <- MP.many rangeParser
      pure (src, (dst, ranges))

    rangeParser :: MP.Parsec Void String (Int, Int, Int)
    rangeParser = do
      dstStart <- MPL.decimal <* MP.space
      srcStart <- MPL.decimal <* MP.space
      rangeLen <- MPL.decimal <* MP.space
      pure (dstStart, srcStart, rangeLen)

parseInput :: String -> Either String Input
parseInput = first MP.errorBundlePretty . MP.parse parser "day05"

tryRange :: (Int, Int, Int) -> Int -> Maybe Int
tryRange (dstStart, srcStart, rangeLen) i
  | i >= srcStart && i <= srcStart + rangeLen = Just $ dstStart - srcStart + i
  | otherwise = Nothing

getDstVal :: [(Int, Int, Int)] -> Int -> Int
getDstVal ranges i =
  fromMaybe i . listToMaybe . mapMaybe (`tryRange` i) $ ranges

goToLoc :: Map String (String, [(Int, Int, Int)]) -> String -> Int -> Int
goToLoc _ "location" val = val
goToLoc maps src srcVal =
  let (dst, dstRangeMaps) = maps M.! src
   in goToLoc maps dst (getDstVal dstRangeMaps srcVal)

solveA :: Input -> Int
solveA (Input seeds maps) =
  minimum . fmap (goToLoc maps "seed") $ seeds

day05a :: Solution Input Int
day05a = Solution {sParse = parseInput, sShow = show, sSolve = Right . solveA}

day05b :: Solution _ _
day05b = Solution {sParse = Right, sShow = show, sSolve = Right}

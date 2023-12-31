{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day22
  ( day22a,
  )
where

-- , day22b

import AoC.Solution
import Control.Lens ((&), (.~), (^.))
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Linear (V2 (..), V3 (..), _z)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

type Block = (V3 Int, V3 Int)

blockParser :: MP.Parsec Void String Block
blockParser = do
  x1 <- MPL.decimal <* MP.char ','
  y1 <- MPL.decimal <* MP.char ','
  z1 <- MPL.decimal <* MP.char '~'
  x2 <- MPL.decimal <* MP.char ','
  y2 <- MPL.decimal <* MP.char ','
  z2 <- MPL.decimal
  pure (V3 x1 y1 z1, V3 x2 y2 z2)

parse :: String -> Either String [Block]
parse =
  first MP.errorBundlePretty . MP.parse (MP.sepBy blockParser MP.space) "day22"

getXYs :: Block -> [V2 Int]
getXYs (V3 x1 y1 _, V3 x2 y2 _)
  | x1 /= x2 = [V2 x y1 | x <- [x1 .. x2]]
  | y1 /= y2 = [V2 x1 y | y <- [y1 .. y2]]
  | otherwise = [V2 x1 y1]

-- Settle the blocks, and return a map of settle block to blocks that support it.
settle :: [Block] -> Map Block [Block]
settle =
  snd . foldl' go (M.empty, M.empty) . sortOn ((^. _z) . fst)
  where
    go ::
      (Map (V2 Int) (Int, Block), Map Block [Block]) ->
      Block ->
      (Map (V2 Int) (Int, Block), Map Block [Block])
    go (highMap, supportMap) b@(b1@(V3 _ _ z1), b2@(V3 _ _ z2)) =
      let xys = getXYs b
          possSupports = nub . fmap (fromMaybe (0, (0, 0)) . (highMap M.!?)) $ xys
          z' = (+ 1) . maximum . fmap fst $ possSupports
          supports = nub . fmap snd $ filter ((== (z' - 1)) . fst) possSupports
          b' = (b1 & _z .~ z', b2 & _z .~ (z' + z2 - z1))
          newHeights = M.fromList . fmap (,(z' + z2 - z1, b')) $ xys
       in (M.union newHeights highMap, M.insert b' supports supportMap)

solveA :: [Block] -> Int
solveA bs =
  length . filter id . fmap couldDisintegrate . M.keys $ settled
  where
    settled :: Map Block [Block]
    settled = settle bs

    couldDisintegrate :: Block -> Bool
    couldDisintegrate b = not . or . M.elems . M.map (== [b]) $ settled

day22a :: Solution [Block] Int
day22a = Solution {sParse = parse, sShow = show, sSolve = Right . solveA}

day22b :: Solution _ _
day22b = Solution {sParse = Right, sShow = show, sSolve = Right}

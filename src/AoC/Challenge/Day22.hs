{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
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
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
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

getXYZs :: Block -> [V3 Int]
getXYZs (V3 x1 y1 z1, V3 x2 y2 z2)
  | x1 /= x2 = [V3 x y1 z1 | x <- [x1 .. x2]]
  | y1 /= y2 = [V3 x1 y z1 | y <- [y1 .. y2]]
  | z1 /= z2 = [V3 x1 y1 z | z <- [z1 .. z2]]
  | otherwise = [V3 x1 y1 z1]

settle :: [Block] -> ([Block], Int)
settle =
  snd . foldl' go (M.empty, ([], 0)) . sortOn ((^. _z) . fst)
  where
    go :: (Map (V2 Int) Int, ([Block], Int)) -> Block -> (Map (V2 Int) Int, ([Block], Int))
    go (m, (bs, a)) b@(b1@(V3 _ _ z1), b2@(V3 _ _ z2)) =
      let xys = getXYs b
          z' = (+ 1) . maximum . fmap (fromMaybe 0 . (m M.!?)) $ xys
          new = M.fromList . fmap (,z' + z2 - z1) $ xys
          b' = (b1 & _z .~ z', b2 & _z .~ (z' + z2 - z1))
       in (M.union new m, (b' : bs, if b == b' then a else a + 1))

solveA :: [Block] -> Int
solveA bs =
  length . filter id . fmap couldDisintegrate $ settled
  where
    settled :: [Block]
    settled = sortOn ((^. _z) . fst) . fst . settle $ bs

    xyzToBlock :: Map (V3 Int) Block
    xyzToBlock = M.fromList . concatMap (\b -> [(xyz, b) | xyz <- getXYZs b]) $ settled

    getSupports :: Block -> [Block]
    getSupports b =
      let getSupport :: V3 Int -> Maybe Block
          getSupport t@(V3 _ _ z)
            | z == 1 = Just (V3 0 0 0, V3 0 0 0) -- Dummy "ground" block
            | otherwise =
                (\x -> if x == b then Nothing else Just x)
                  =<< M.lookup (t - V3 0 0 1) xyzToBlock
       in mapMaybe getSupport . getXYZs $ b

    couldDisintegrate :: Block -> Bool
    couldDisintegrate b = all (any (/= b) . getSupports) settled

day22a :: Solution [Block] Int
day22a = Solution {sParse = parse, sShow = show, sSolve = Right . solveA}

day22b :: Solution _ _
day22b = Solution {sParse = Right, sShow = show, sSolve = Right}

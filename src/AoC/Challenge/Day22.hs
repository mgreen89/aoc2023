module AoC.Challenge.Day22
  ( day22a,
    day22b,
  )
where

import AoC.Solution
import Control.Lens ((&), (.~), (^.))
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
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

-- Settle the blocks, and return:
--  - a map of settled block to blocks that support it.
--  - the number of bricks that fell.
--
-- The 'ground' is represented as a single block at the origin.
settle :: [Block] -> (Map Block [Block], Int)
settle =
  snd . foldl' go (M.empty, (M.empty, 0)) . sortOn ((^. _z) . fst)
  where
    go ::
      (Map (V2 Int) (Int, Block), (Map Block [Block], Int)) ->
      Block ->
      (Map (V2 Int) (Int, Block), (Map Block [Block], Int))
    go (highMap, (supportMap, count)) b@(b1@(V3 _ _ z1), b2@(V3 _ _ z2)) =
      let xys = getXYs b
          possSupports = nub . fmap (fromMaybe (0, (0, 0)) . (highMap M.!?)) $ xys
          z' = (+ 1) . maximum . fmap fst $ possSupports
          supports = nub . fmap snd $ filter ((== (z' - 1)) . fst) possSupports
          b' = (b1 & _z .~ z', b2 & _z .~ (z' + z2 - z1))
          newHeights = M.fromList . fmap (,(z' + z2 - z1, b')) $ xys
       in ( M.union newHeights highMap,
            (M.insert b' supports supportMap, if b' == b then count else count + 1)
          )

solveA :: [Block] -> Int
solveA bs =
  length . filter id . fmap couldDisintegrate . M.keys $ settled
  where
    settled :: Map Block [Block]
    settled = fst $ settle bs

    couldDisintegrate :: Block -> Bool
    couldDisintegrate b = not . or . M.elems . M.map (== [b]) $ settled

day22a :: Solution [Block] Int
day22a = Solution {sParse = parse, sShow = show, sSolve = Right . solveA}

solveB :: [Block] -> Int
solveB bs =
  sum . fmap fallCount $ M.keys settled
  where
    settled :: Map Block [Block]
    settled = fst $ settle bs

    blockToSupported :: Map Block [Block]
    blockToSupported =
      M.foldlWithKey' go M.empty settled
      where
        go a b = M.unionWith (++) a . M.fromList . fmap (,[b])

    fallCount :: Block -> Int
    fallCount b =
      S.size
        . S.delete b
        . getAllFalling (S.singleton b)
        . fromMaybe []
        $ blockToSupported M.!? b

    getAllFalling :: Set Block -> [Block] -> Set Block
    getAllFalling falling consider =
      let newlyFalling = filter (\b -> all (`S.member` falling) (settled M.! b)) consider
          falling' = S.union falling . S.fromList $ newlyFalling
       in if null newlyFalling
            then falling
            else
              getAllFalling falling'
                . concatMap (fromMaybe [] . (blockToSupported M.!?))
                $ newlyFalling

day22b :: Solution [Block] Int
day22b = Solution {sParse = parse, sShow = show, sSolve = Right . solveB}

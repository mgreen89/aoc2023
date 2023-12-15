module AoC.Challenge.Day15 (
  day15a,
  day15b,
  )
where

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Char (isAlpha, isDigit, ord)
import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)

hash :: String -> Int
hash = foldl' go 0
  where
  go a = (`mod` 256) . (* 17) . (+ a) . ord

day15a :: Solution [String] Int
day15a = Solution{sParse = Right . splitOn ",", sShow = show, sSolve = Right . sum . fmap hash }

data Op = Remove
        | Set Int
  deriving (Eq, Generic, NFData, Show) 

instance Read Op where
  readsPrec _ = \case
    '-' : rest -> [(Remove, rest)]
    '=' : d : rest -> if isDigit d then [(Set (read (d:[])), rest)] else []
    _ -> [] 

solveB :: [(String, Op)] -> Int
solveB = 
  focussingPower . snd . foldl' go (0, IM.empty)
  where
    go :: (Int, IntMap (Map String (Int, Int))) -> (String, Op) -> (Int, IntMap (Map String (Int, Int)))
    go (i, m) (l, o) =
      case o of
        Remove -> (i + 1, IM.adjust (M.delete l) (hash l) m)
        Set fl -> (i + 1, IM.insertWith (M.unionWith (\(_, fN) (iO, _) -> (iO, fN))) (hash l) (M.singleton l (i, fl))  m)

    focussingPower :: IntMap (Map String (Int, Int)) -> Int
    focussingPower =
      IM.foldlWithKey' sumBox 0
      where
      sumBox :: Int -> Int -> Map String (Int, Int) -> Int
      sumBox a i m =
        let
          boxLenses :: [Int]
          boxLenses = fmap snd . sortOn fst . M.elems $ m
        in a + (sum . fmap (\(n, f) -> (1 + i) * n * f) . zip [1..] $ boxLenses)
        

day15b :: Solution [(String, Op)] Int
day15b = Solution{sParse = Right . fmap (fmap read . span isAlpha) . splitOn ",", sShow = show, sSolve = Right . solveB }

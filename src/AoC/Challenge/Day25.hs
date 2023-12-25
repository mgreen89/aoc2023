module AoC.Challenge.Day25
  ( day25a,
  )
where

import AoC.Common (listTup2, pairs)
import AoC.Common.Graph (dijkstra)
import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Foldable (foldl')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (sortOn, unfoldr)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import qualified System.Random as R

parse :: String -> Either String (Map String (Set String))
parse =
  fmap (M.fromList . fmap (fmap (S.fromList . splitOn " ")))
    . traverse (maybeToEither "invalid line" . listTup2 . splitOn ": ")
    . lines

normalise :: Map String (Set String) -> Map String (Set String)
normalise =
  M.foldlWithKey' go M.empty
  where
    go :: Map String (Set String) -> String -> Set String -> Map String (Set String)
    go m a bs =
      M.unionsWith
        S.union
        ( m : M.singleton a bs : [M.singleton x (S.singleton a) | x <- S.toList bs]
        )

findConnectedSize :: Map String (Set String) -> String -> Int
findConnectedSize m =
  S.size . go S.empty
  where
    go :: Set String -> String -> Set String
    go a s
      | S.member s a = a
      | otherwise = foldl' go (S.insert s a) $ S.toList (m M.! s)

solveA :: Map String (Set String) -> Int
solveA m =
  let km :: IntMap String
      km = IM.fromList . zip [1 ..] . M.keys $ m

      getNeighbs :: String -> Map String Int
      getNeighbs = M.fromSet (const 1) . (m M.!)

      unwrap :: (R.RandomGen g) => (Int, g) -> Maybe ((Int, Int), (Int, g))
      unwrap (i, rg)
        | i >= 1000 = Nothing
        | otherwise =
            let (a, rg') = R.uniformR (1, IM.size km) rg
                (b, rg'') = R.uniformR (1, IM.size km) rg'
             in Just ((a, b), (i + 1, rg''))

      randList :: [(Int, Int)]
      randList = unfoldr unwrap (0, R.mkStdGen 14931837294)

      countMap :: Map (Set String) Int
      countMap =
        M.unionsWith (+)
          . fmap (M.fromList . map (\(a, b) -> (S.fromList [a, b], 1)) . pairs . snd)
          $ catMaybes
            [ dijkstra getNeighbs start (== end)
              | (a, b) <- randList,
                let start = km IM.! a,
                let end = km IM.! b
            ]

      topThree = fmap fst . take 3 . sortOn (negate . snd) $ M.toList countMap
      topThreePairs = fromJust . listTup2 . S.toList <$> topThree

      cutMap :: Map String (Set String)
      cutMap = foldl' doCut m topThreePairs

      doCut :: Map String (Set String) -> (String, String) -> Map String (Set String)
      doCut mp (a, b) =
        let news =
              M.fromList
                [ (a, S.delete b (mp M.! a)),
                  (b, S.delete a (mp M.! b))
                ]
         in M.union news mp

      (sideA, sideB) = head topThreePairs
   in findConnectedSize cutMap sideA * findConnectedSize cutMap sideB

day25a :: Solution (Map String (Set String)) Int
day25a =
  Solution
    { sParse = fmap normalise . parse,
      sShow = show,
      sSolve = Right . solveA
    }

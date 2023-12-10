module AoC.Challenge.Day08
  ( day08a,
    day08b,
  )
where

import AoC.Common (listTup4)
import AoC.Solution
import AoC.Util (maybeToEither)
import Control.DeepSeq (NFData)
import Data.Char (isAlphaNum)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Text.Read (readEither)

data Dir = L | R deriving (Enum, Eq, Generic, NFData, Ord, Read, Show)

parse :: String -> Either String ([Dir], Map String (String, String))
parse s =
  case lines s of
    is : _ : mp -> (,) <$> traverse (readEither . (: [])) is <*> (M.fromList <$> traverse getMapEntry mp)
    _ -> Left $ "Invalid input format"
  where
    getMapEntry :: String -> Either String (String, (String, String))
    getMapEntry l =
      fmap (\(a, _, b, c) -> (a, (b, c)))
        . maybeToEither ("Invalid map line " ++ l)
        . listTup4
        . fmap (filter isAlphaNum)
        . words
        $ l

start :: String
start = "AAA"

target :: String
target = "ZZZ"

step :: Map String (String, String) -> Dir -> String -> String
step m d l =
  ( case d of
      L -> fst
      R -> snd
  )
    $ m M.! l

solveA :: ([Dir], Map String (String, String)) -> Int
solveA (is, m) =
  go start 0 (cycle is)
  where
    go l c ds =
      let l' = step m (head ds) l
          c' = c + 1
       in if l' == target then c' else go l' c' (tail ds)

day08a :: Solution ([Dir], Map String (String, String)) Int
day08a = Solution {sParse = parse, sShow = show, sSolve = Right . solveA}

indexMod :: Seq a -> Int -> a
indexMod s i = s `Seq.index` (i `mod` Seq.length s)

solveB :: ([Dir], Map String (String, String)) -> Int
solveB (is, m) =
  -- By inspection of the lengths of paths, there are cycles.
  -- So just find the lowest common multiple.
  foldl' lcm 1
    . fmap length
    . M.elems
    . M.filterWithKey (\k _ -> last k == 'A')
    $ (`Seq.index` 0) <$> pathsTo__Z
  where
    initialize :: (String, String) -> Seq String
    initialize (l, r) =
      Seq.fromList $
        fmap
          ( \case
              L -> l
              R -> r
          )
          is

    -- The first path to the first __Z node for every starting node from
    -- every point in the instruction set.
    --
    -- Hurray for laziness!
    pathsTo__Z :: Map String (Seq [String])
    pathsTo__Z = fmap (Seq.mapWithIndex mapPath) (initialize <$> m)

    mapPath :: Int -> String -> [String]
    mapPath i x =
      x : (if last x == 'Z' then [] else (pathsTo__Z M.! x) `indexMod` (i + 1))

day08b :: Solution ([Dir], Map String (String, String)) Int
day08b = Solution {sParse = parse, sShow = show, sSolve = Right . solveB}

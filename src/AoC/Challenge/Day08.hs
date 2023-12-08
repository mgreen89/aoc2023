module AoC.Challenge.Day08 (
  day08a,
  day08b,
  )
where

import AoC.Common (listTup4)
import AoC.Solution
import AoC.Util (maybeToEither)
import Control.DeepSeq (NFData)
import Data.Char (isAlphaNum)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Text.Read (readEither)

data Dir = L | R deriving (Enum, Eq, Generic, NFData, Ord, Read, Show)

parse :: String -> Either String ([Dir], Map String (String, String))
parse s =
  case (lines s) of
    is : _ : mp -> (,) <$> (traverse (readEither . (: [])) is) <*> (M.fromList <$> traverse getMapEntry mp)
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
step m d l = (case d of
  L -> fst
  R -> snd) $ m M.! l

solveA :: ([Dir], Map String (String, String)) -> Int
solveA (is, m) =
  go start 0 (cycle is)
  where
    go l c ds =
      let
        l' = step m (head ds) l
        c' = c + 1
      in
      if l' == target then c' else go l' c' (tail ds)

day08a :: Solution ([Dir], Map String (String, String)) Int
day08a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA }

solveB :: ([Dir], Map String (String, String)) -> Int
solveB (is, m) =
  let
    starts = filter ((== 'A') . last) . M.keys $ m
  in
  go starts 0 (cycle is)
  where
    go :: [String] -> Int -> [Dir] -> Int
    go ls c ds =
      if all (\l -> last l == 'Z') ls
        then c 
        else go (fmap (step m (head ds)) ls) (c + 1) (tail ds) 

day08b :: Solution ([Dir], Map String (String, String)) Int
day08b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB }

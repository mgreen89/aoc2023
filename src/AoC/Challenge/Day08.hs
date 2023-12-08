{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day08 (
  day08a,
  )
where

-- , day08b

import AoC.Common (listTup4)
import AoC.Solution
import AoC.Util (maybeToEither)
import Control.DeepSeq (NFData)
import Data.Char (isAsciiUpper)
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
      . fmap (filter isAsciiUpper)
      . words
      $ l

start :: String
start = "AAA"

target :: String
target = "ZZZ"

solveA :: ([Dir], Map String (String, String)) -> Int
solveA (is, m) =
  go start 0 (cycle is)
  where
    go l c ds =
      let
        (l', c') = step (head ds) l c
      in
      if l' == target then c' else go l' c' (tail ds)

    step d l c =
      (dirOf (m M.! l), c + 1)
      where
        dirOf = case d of
                  L -> fst
                  R -> snd


day08a :: Solution ([Dir], Map String (String, String)) Int
day08a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA }

day08b :: Solution _ _
day08b = Solution{sParse = Right, sShow = show, sSolve = Right}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day07 (
  day07a,
  )
where

-- , day07b

import AoC.Common (listTup2)
import AoC.Solution
import AoC.Util (freqs, maybeToEither)
import Control.Monad ((<=<), join)
import Control.DeepSeq (NFData)
import Data.Bitraversable (bitraverse)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (Down(..))
import GHC.Generics (Generic)
import Text.Read (readMaybe)


import Data.Maybe (fromJust)

data Card
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Enum, Eq, Generic, NFData, Ord, Show)

charToCard :: Map Char Card
charToCard = M.fromList $ zip "23456789TJQKA" [Two .. Ace]

parse :: String -> Either String [([Card], Int)]
parse =
  maybeToEither "invalid input"
   . join
   . fmap (traverse (bitraverse (traverse (`M.lookup` charToCard)) (readMaybe)))
   . traverse (listTup2 . words) 
   . lines

day07a :: Solution [([Card], Int)] Int
day07a = Solution{sParse = parse, sShow = show,
                  sSolve = Right . sum . zipWith (\i (_, b) -> i * b) [1..] . sortOn (\(h, _) -> (sortOn Down . M.elems . freqs $ h, h))
                  }

day07b :: Solution _ _
day07b = Solution{sParse = Right, sShow = show, sSolve = Right}

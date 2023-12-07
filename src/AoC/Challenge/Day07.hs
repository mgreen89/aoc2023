module AoC.Challenge.Day07 (
  day07a,
  day07b,
  )
where

import AoC.Common (listTup2)
import AoC.Solution
import AoC.Util (freqs, maybeToEither)
import Control.Monad (join)
import Control.DeepSeq (NFData)
import Data.Bitraversable (bitraverse)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (Down(..))
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data Card
  = Joker
  | Two
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

charToCardA :: Map Char Card
charToCardA = M.fromList $ zip "23456789TJQKA" [Two .. Ace]

parse :: Map Char Card -> String -> Either String [([Card], Int)]
parse cToC =
  maybeToEither "invalid input"
   . join
   . fmap (traverse (bitraverse (traverse (`M.lookup` cToC)) (readMaybe)))
   . traverse (listTup2 . words) 
   . lines

solve :: Ord a => ([Card] -> a) -> [([Card], Int)] -> Int
solve handScore =
  sum
  . zipWith (\i (_, b) -> i * b) [1..]
  . sortOn (handScore . fst)

handScoreA :: [Card] -> ([Int], [Card])
handScoreA cs = (sortOn Down . M.elems . freqs $ cs, cs)

day07a :: Solution [([Card], Int)] Int
day07a = Solution{sParse = parse charToCardA, sShow = show, sSolve = Right . solve handScoreA }

charToCardB :: Map Char Card
charToCardB = M.insert 'J' Joker charToCardA

handScoreB :: [Card] -> ([Int], [Card])
handScoreB cs =
  -- Add the jokers to the top frequency.
  let
    fs = freqs cs
    numJokers = M.lookup Joker fs
    fs' = M.insert Joker 0 fs
    fsl = sortOn Down (M.elems fs')
    scores = case numJokers of
      Nothing -> fsl
      Just n -> case fsl of
        [] -> [n]
        x : xs -> (x + n) : xs
  in
  (scores, cs)

day07b :: Solution [([Card], Int)] Int
day07b = Solution{sParse = parse charToCardB, sShow = show, sSolve = Right . solve handScoreB }

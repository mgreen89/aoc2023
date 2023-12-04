module AoC.Challenge.Day04
  ( day04a,
    day04b,
  )
where

import AoC.Solution
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

type Card = (Int, (IntSet, IntSet))

type Cards = IntMap (IntSet, IntSet)

parser :: MP.Parsec Void String Cards
parser = do
  M.fromList <$> MP.many cardParser
  where
    cardParser :: MP.Parsec Void String Card
    cardParser = do
      cardNo <- MP.string "Card" *> MP.space1 *> MPL.decimal <* MP.string ":"
      MP.space
      winners <- MP.many (MPL.decimal <* MP.space)
      MP.string "|"
      MP.space
      have <- MP.many (MPL.decimal <* MP.space)
      pure (cardNo, (S.fromList winners, S.fromList have))

parse :: String -> Either String Cards
parse =
  first MP.errorBundlePretty . MP.parse parser "day04"

day04a :: Solution Cards Int
day04a =
  Solution
    { sParse = parse,
      sShow = show,
      sSolve =
        Right
          . sum
          . fmap
            ( (\x -> if x == 0 then 0 else 2 ^ (x - 1))
                . S.size
                . uncurry S.intersection
            )
          . M.elems
    }

partB :: Cards -> Int
partB cards =
  sum . M.elems . M.foldlWithKey' go initial $ cards
  where
    initial :: IntMap Int
    initial = M.fromList [(i, 1) | i <- M.keys cards]

    go :: IntMap Int -> Int -> (IntSet, IntSet) -> IntMap Int
    go counts card (winners, haves) =
      let winnerCount = S.size $ S.intersection winners haves
          cardCount = counts M.! card
          addCards = M.fromList [(c, cardCount) | c <- [card + 1 .. card + winnerCount]]
       in M.unionWith (+) counts addCards

day04b :: Solution Cards Int
day04b =
  Solution
    { sParse = parse,
      sShow = show,
      sSolve = Right . partB
    }

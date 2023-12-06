module AoC.Challenge.Day05
  ( day05a,
    day05b,
  )
where

import AoC.Common (listTup2)
import AoC.Solution
import AoC.Util (maybeToEither)
import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Interval (Interval)
import qualified Data.Interval as IV
import Data.IntervalMap.Strict (IntervalMap)
import qualified Data.IntervalMap.Strict as IVM
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IVS
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

data Input s = Input {seeds :: s, maps :: [IntervalMap Int Int]}
  deriving (Show, Generic, NFData)

type InputA = Input [Int]

type InputB = Input (IntervalSet Int)

fromRange :: (Ord a, Num a) => a -> a -> Interval a
fromRange start len =
  IV.Finite start IV.<=..< IV.Finite (start + len)

fromFinite :: IV.Extended a -> Maybe a
fromFinite (IV.Finite x) = Just x
fromFinite _ = Nothing

parser :: MP.Parsec Void String InputA
parser = do
  seeds <- MP.string "seeds: " *> MP.many (MPL.decimal <* MP.space)
  maps <- M.fromList <$> MP.many mapParser
  let mapList = go maps "seed" []
        where
          go ::
            Map String (String, IntervalMap Int Int) ->
            String ->
            [IntervalMap Int Int] ->
            [IntervalMap Int Int]
          go _ "location" a = reverse a
          go ms v a =
            let (next, m) = ms M.! v
             in go ms next (m : a)
  pure $ Input seeds mapList
  where
    mapParser :: MP.Parsec Void String (String, (String, IntervalMap Int Int))
    mapParser = do
      src <- MP.manyTill MP.lowerChar (MP.char '-') <* MP.string "to-"
      dst <- MP.many MP.lowerChar <* MP.space <* MP.string "map:\n"
      ranges <- MP.many rangeParser
      pure (src, (dst, IVM.fromList ranges))

    rangeParser :: MP.Parsec Void String (Interval Int, Int)
    rangeParser = do
      dstStart <- MPL.decimal <* MP.space
      srcStart <- MPL.decimal <* MP.space
      rangeLen <- MPL.decimal <* MP.space
      pure (fromRange srcStart rangeLen, dstStart - srcStart)

parseInput :: String -> Either String InputA
parseInput = first MP.errorBundlePretty . MP.parse parser "day05"

getDstVal :: IntervalMap Int Int -> Int -> Int
getDstVal ranges i = maybe i (+ i) (IVM.lookup i ranges)

solveA :: InputA -> Int
solveA (Input seeds maps) =
  minimum . foldl' (\vs r -> fmap (getDstVal r) vs) seeds $ maps

day05a :: Solution InputA Int
day05a = Solution {sParse = parseInput, sShow = show, sSolve = Right . solveA}

convertInput :: InputA -> Either String InputB
convertInput (Input seeds maps) =
  fmap ((`Input` maps) . IVS.fromList . fmap (uncurry fromRange))
    . maybeToEither "Seeds are not in pairs"
    . traverse listTup2
    . chunksOf 2
    $ seeds

intersectRanges :: IntervalMap Int Int -> IntervalSet Int -> IntervalSet Int
intersectRanges rangeMap xs =
  IVS.union unchanged changed
  where
    unchanged = xs `IVS.difference` IVM.keysSet rangeMap
    xsMap = IVM.fromList . fmap (,0) . IVS.toList $ xs
    changed =
      IVS.fromList
        . fmap (\(iv, offset) -> IV.mapMonotonic (+ offset) iv)
        . IVM.toList
        $ rangeMap `IVM.intersection` xsMap

solveB :: InputB -> Int
solveB (Input seeds maps) =
  fromJust . fromFinite . IV.lowerBound . (!! 0) . IVS.toAscList $
    foldl' (flip intersectRanges) seeds maps

day05b :: Solution InputB Int
day05b =
  Solution
    { sParse = parseInput >=> convertInput,
      sShow = show,
      sSolve = Right . solveB
    }

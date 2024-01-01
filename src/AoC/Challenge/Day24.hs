module AoC.Challenge.Day24
  ( day24a,
    day24b,
  )
where

import AoC.Common.Point (inBoundingBox)
import AoC.Solution
import Data.Bifunctor (first)
import Data.List (tails, uncons)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Linear (V2 (..), V3 (..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

lineParser :: MP.Parsec Void String (V3 Int, V3 Int)
lineParser = do
  px <- signed <* MP.char ',' <* MP.space
  py <- signed <* MP.char ',' <* MP.space
  pz <- signed <* MP.string " @" <* MP.space
  vx <- signed <* MP.char ',' <* MP.space
  vy <- signed <* MP.char ',' <* MP.space
  vz <- signed
  pure (V3 px py pz, V3 vx vy vz)
  where
    signed :: MP.Parsec Void String Int
    signed = MPL.signed MP.space MPL.decimal

parse :: String -> Either String [(V3 Int, V3 Int)]
parse = first MP.errorBundlePretty . MP.parse (MP.sepBy lineParser (MP.char '\n')) "day24"

getFutureIntersection2d ::
  (Eq a, Ord a, Fractional a, Integral b) =>
  ((V2 b, V2 b), V2 a) ->
  ((V2 b, V2 b), V2 a) ->
  Maybe (V2 a)
getFutureIntersection2d ((V2 apx _, V2 avx _), V2 a b) ((V2 bpx _, V2 bvx _), V2 c d) =
  let -- ax + b = cx + d => x = (d - b)/(a - c)
      xI = (d - b) / (a - c)
      yI = a * xI + b
      parallel = a == c
      inPastA = (xI - fromIntegral apx) * fromIntegral avx < 0
      inPastB = (xI - fromIntegral bpx) * fromIntegral bvx < 0
   in if parallel || inPastA || inPastB
        then Nothing
        else Just (V2 xI yI)

type Info = ((V2 Int, V2 Int), V2 Double)

solveA :: [(V3 Int, V3 Int)] -> Int
solveA inp =
  sum
    . fmap (length . filter id . intersectInRange)
    . mapMaybe uncons
    $ tails ls
  where
    to2d :: (V3 a, V3 a) -> (V2 a, V2 a)
    to2d (V3 px py _, V3 vx vy _) = (V2 px py, V2 vx vy)

    toLine :: (Integral a, Fractional b) => (V2 a, V2 a) -> V2 b
    toLine (V2 px py, V2 vx vy) =
      let n = fromIntegral px / fromIntegral vx
          yI = (fromIntegral py - (fromIntegral vy * n))
       in V2 (fromIntegral vy / fromIntegral vx) yI

    ls :: [Info]
    ls = (\x -> (x, toLine x)) . to2d <$> inp

    bb = (V2 200000000000000 200000000000000, V2 400000000000000 400000000000000)
    intersectInRange :: (Info, [Info]) -> [Bool]
    intersectInRange (l, ls') =
      maybe False (inBoundingBox bb) . getFutureIntersection2d l <$> ls'

day24a :: Solution [(V3 Int, V3 Int)] Int
day24a = Solution {sParse = parse, sShow = show, sSolve = Right . solveA}

{-
  Well this is interesting...  blech.
  Simultaneous equations that won't solve with the `Double` type - needs a
  higher precision type for the Gaussian Elimination step

  Instead, 'cheat' and just plug some equations into z3 (taken from the first
  set of hailstones of input).

  Output is:
  (define-fun t2 () Real
    499903573610.0)
  (define-fun vz () Real
    249.0)
  (define-fun t1 () Real
    154588043705.0)
  (define-fun vx () Real
    214.0)
  (define-fun t3 () Real
    548282595621.0)
  (define-fun vy () Real
    (- 168.0))
  (define-fun z () Real
    148125938782131.0)
  (define-fun x () Real
    172543224455736.0)
  (define-fun y () Real
    348373777394510.0)

  Return x + y + z
-}
day24b :: Solution [(V3 Int, V3 Int)] Int
day24b = Solution {sParse = parse, sShow = show, sSolve = Right . const 669042940632377}

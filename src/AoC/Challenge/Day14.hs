module AoC.Challenge.Day14
  ( day14a,
    day14b,
  )
where

import AoC.Solution
import Control.Applicative (liftA2)
import Data.Foldable (foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

type Point = V2 Int

-- Parse so (1,1) is in the bottom left!
-- Returns (cubed, rounded)
parse :: String -> (Set Point, Set Point)
parse s =
  ( S.fromList . fmap fst . filter (\(_, c) -> c == '#') $ points,
    S.fromList . fmap fst . filter (\(_, c) -> c == 'O') $ points
  )
  where
    points :: [(Point, Char)]
    points =
      concat
        . zipWith (\y -> zipWith (\x -> (V2 x y,)) [1 ..]) [1 ..]
        . reverse
        . lines
        $ s

-- This relies on there being at least one cube in each edge.
rollUp :: Set Point -> Set Point -> Set Point
rollUp cs rs =
  let (V2 xMax yMax) = S.foldl' (liftA2 max) 0 cs
      cols xs = [S.filter (\(V2 x _) -> x == i) xs | i <- [1 .. xMax]]

      cubeCols = cols cs
      roundCols = cols rs

      rollCol :: Set Point -> Set Point -> Set Point
      rollCol c r =
        foldl' go S.empty (S.toDescList r)
        where
          go a (V2 x y)
            | y == yMax = S.insert (V2 x y) a
            | S.member (V2 x (y + 1)) (S.union a c) = S.insert (V2 x y) a
            | otherwise = go a (V2 x (y + 1))
   in S.unions
        . fmap (uncurry rollCol)
        $ zip cubeCols roundCols

score :: Set Point -> Int
score = sum . fmap (\(V2 _ y) -> y) . S.elems

day14a :: Solution (Set Point, Set Point) Int
day14a = Solution {sParse = Right . parse, sShow = show, sSolve = Right . score . uncurry rollUp}

solveB :: Int -> (Set Point, Set Point) -> Int
solveB tgt (cs, rs) =
  fst $ mp IM.! (fstI + ((tgt - fstI) `mod` rptLen))
  where
    (V2 xMax _) = S.foldl' (liftA2 max) 0 cs
    (fstI, rptLen, mp) = build 1 rs IM.empty IM.empty

    -- Build up to a repeat. Return the first repeated value and the repeat length.
    build :: Int -> Set Point -> IntMap [Int] -> IntMap (Int, Set Point) -> (Int, Int, IntMap (Int, Set Point))
    build i r sm rm =
      let next = spinCycle cs r
          nextScore = score next
       in case IM.lookup nextScore sm of
            Nothing -> build (i + 1) next (IM.insertWith (++) nextScore [i] sm) (IM.insert i (nextScore, next) rm)
            Just is ->
              case filter (\j -> snd (rm IM.! j) == next) is of
                [q] -> (q, i - q, IM.insert i (nextScore, next) rm)
                [] -> build (i + 1) next (IM.insertWith (++) nextScore [i] sm) (IM.insert i (nextScore, next) rm)
                _ -> error "Matched too many!"

    spinCycle :: Set Point -> Set Point -> Set Point
    spinCycle c r = snd . go . go . go $ go (c, r)
      where
        go :: (Set Point, Set Point) -> (Set Point, Set Point)
        go (cs', rs') = (rotate cs', rotate (rollUp cs' rs'))

    rotate :: Set Point -> Set Point
    rotate = S.map (\(V2 x y) -> V2 y (xMax - x + 1))

day14b :: Solution (Set Point, Set Point) Int
day14b = Solution {sParse = Right . parse, sShow = show, sSolve = Right . solveB 1000000000}

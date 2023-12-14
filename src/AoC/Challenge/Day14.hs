module AoC.Challenge.Day14 (
  day14a,
  day14b,
  )
where

import AoC.Solution
import Control.Applicative (liftA2)
import Data.Foldable (foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2(..))

type Point = V2 Int

-- Parse so (1,1) is in the bottom left!
-- Returns (cubed, rounded)
parse :: String -> (Set Point, Set Point)
parse s =
  ( S.fromList . fmap fst . filter (\(_, c) -> c == '#') $ points
  , S.fromList . fmap fst . filter (\(_, c) -> c == 'O') $ points
  )
  where
    points :: [(Point, Char)]
    points = 
      concat
      . zipWith (\y -> zipWith (\x -> (V2 x y,)) [1..]) [1..]
      . reverse
      . lines
      $ s

rollUp :: Set Point -> Set Point -> Set Point
rollUp cs rs =
  let
    (V2 xMax yMax) = S.foldl' (liftA2 max) 0 rs
    cols xs = [ S.filter (\(V2 x _) -> x == i) xs | i <- [1..xMax] ]

    cubeCols = cols cs
    roundCols = cols rs

    rollCol :: Set Point -> Set Point -> Set Point
    rollCol c r =
      foldl' go S.empty (S.toDescList r)
      where
        go a (V2 x y)
          | y == yMax = S.insert (V2 x y) a
          | S.member (V2 x (y+1)) (S.union a c) = S.insert (V2 x y) a
          | otherwise = go a (V2 x (y+1))

  in
  S.unions
  . fmap (uncurry rollCol)
  $ zip cubeCols roundCols

score :: Set Point -> Int
score = sum . fmap (\(V2 _ y) -> y) . S.elems

day14a :: Solution (Set Point, Set Point) Int
day14a = Solution{sParse = Right . parse, sShow = show, sSolve = Right . score . uncurry rollUp }

spin :: Set Point -> Set Point -> Set Point
spin cs rs =
  rotate
  . rollUp (rotate (rotate (rotate cs)))
  . traceShowId
  . rotate
  . rollUp (rotate (rotate cs))
  . traceShowId
  . rotate
  . rollUp (rotate cs)
  . traceShowId
  . rotate
  $ rollUp cs rs

  where
    (V2 xMax _) = S.foldl' (liftA2 max) 0 rs

    rotate :: Set Point -> Set Point
    rotate = S.map (\(V2 x y) -> V2 y (xMax-x+1))

solveB :: Int -> (Set Point, Set Point) -> Int
solveB tgt (rs, cs) =
  fst $ mp IM.! ((tgt - fstI) `mod` rptLen)
  where
    (fstI, rptLen, mp) = traceShowId $ build 1 cs IM.empty (IM.singleton 0 (score rs, rs))

    -- Build up to a repeat. Return the first repeated value and the repeat length.
    build :: Int -> Set Point -> IntMap [Int] -> IntMap (Int, Set Point) -> (Int, Int, IntMap (Int, Set Point))
    build i c sm rm =
      let
        next = traceShowId $ spin c rs
        nextScore = traceShowId $ score next
      in
      case IM.lookup nextScore sm of
        Nothing -> build (i + 1) next (IM.insertWith (++) nextScore [i] sm) (IM.insert i (nextScore, next) rm)
        Just is ->
          case filter (\j -> snd (rm IM.! j) == next) is of
            [q] -> (q, i - q, IM.insert i (nextScore, next) rm)
            [] -> build (i + 1) next (IM.insertWith (++) nextScore [i] sm) (IM.insert i (nextScore, next) rm)
            _ -> error "Matched too many!"

day14b :: Solution (Set Point, Set Point) Int
day14b = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solveB 1000000000 }

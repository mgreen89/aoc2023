module AoC.Challenge.Day16
  ( day16a,
    day16b,
  )
where

import AoC.Common.Point (Dir (..), dirPoint, dirRot)
import AoC.Solution
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Linear (V2 (..))
import Text.Read (readMaybe)

type Point = V2 Int

data Elem
  = SplitV
  | SplitH
  | MirrorF
  | MirrorB
  deriving (Eq, Generic, NFData, Show)

instance Read Elem where
  readsPrec _ = \case
    '|' : rest -> [(SplitV, rest)]
    '-' : rest -> [(SplitH, rest)]
    '/' : rest -> [(MirrorF, rest)]
    '\\' : rest -> [(MirrorB, rest)]
    _ -> []

parse :: String -> Map Point Elem
parse =
  M.fromList
    . mapMaybe (traverse (readMaybe . (: [])))
    . concat
    . zipWith (\y -> zipWith (\x -> (V2 x y,)) [0 ..]) [0 ..]
    . lines

energise :: Map Point Elem -> (Point, Dir) -> Int
energise m (p, d) =
  S.size . S.map fst . snd $ fill ((p, d), S.empty)
  where
    (V2 xMin yMin) = foldl' (liftA2 min) 0 $ M.keys m
    (V2 xMax yMax) = foldl' (liftA2 max) 0 $ M.keys m

    fill :: ((Point, Dir), Set (Point, Dir)) -> ((Point, Dir), Set (Point, Dir))
    fill ((p@(V2 x y), d), s)
      | x < xMin || y < yMin = ((p, d), s)
      | x > xMax || y > yMax = ((p, d), s)
      | otherwise = step ((p, d), s)

    step :: ((Point, Dir), Set (Point, Dir)) -> ((Point, Dir), Set (Point, Dir))
    step ((p, d), s) =
      let straight = next d s
          left = next (dirRot L d) s
          right = next (dirRot R d) s
          both =
            let (_, s') = next (dirRot L d) s
             in next (dirRot R d) s'
          next d' s' =
            let nextPD = (p + dirPoint d', d')
                s'' = S.insert (p, d) s'
             in if S.member nextPD s'' then ((p, d), s'') else fill (nextPD, s'')
       in case M.lookup p m of
            Nothing -> straight
            Just e ->
              case d of
                R -> case e of
                  SplitV -> both
                  SplitH -> straight
                  MirrorF -> left
                  MirrorB -> right
                D -> case e of
                  SplitV -> straight
                  SplitH -> both
                  MirrorF -> right
                  MirrorB -> left
                L -> case e of
                  SplitV -> both
                  SplitH -> straight
                  MirrorF -> left
                  MirrorB -> right
                U -> case e of
                  SplitV -> straight
                  SplitH -> both
                  MirrorF -> right
                  MirrorB -> left

day16a :: Solution (Map Point Elem) Int
day16a =
  Solution
    { sParse = Right . parse,
      sShow = show,
      sSolve = Right . flip energise (V2 0 0, R)
    }

solveB :: Map Point Elem -> Int
solveB m =
  maximum $ fmap (energise m) possStarts
  where
    (V2 xMin yMin) = foldl' (liftA2 min) 0 $ M.keys m
    (V2 xMax yMax) = foldl' (liftA2 max) 0 $ M.keys m

    possStarts = [
      pd | x <- [xMin..xMax]
         , y <- [yMin..yMax]
         , rs <- (, R) <$> [V2 xMin y]
         , ds <- (, D) <$> [V2 x yMin]
         , ls <- (, L) <$> [V2 xMax y]
         , us <- (, U) <$> [V2 x yMax]
         , pd <- [rs, ds, ls, us]
      ]

day16b :: Solution (Map Point Elem) Int
day16b =
  Solution
    { sParse = Right . parse,
      sShow = show,
      sSolve = Right . solveB
    }

{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Common.Point
  ( cardinalNeighbs,
    allNeighbs,
    manhattan,
    boundingBox,
    boundingBox',
    inBoundingBox,
    parse2dMap,
    Dir (..),
    dirRot,
    dirPoint,
    shoeLace,
  )
where

import Control.Applicative
import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Semigroup
import Data.Semigroup.Foldable
import GHC.Generics (Generic)
import Linear (Additive, V2 (..), basis, negated)
import Text.Read (readEither)

-- Cardinal neighbour moves.
cardinalDiffs :: (Traversable t, Additive t, Num a) => [t a]
cardinalDiffs = fmap negated basis <> basis

-- | Get the cardinal neighbours (i.e. excluding diagonals) of a vector.
cardinalNeighbs ::
  (Traversable t, Additive t, Applicative t, Num a) =>
  t a ->
  [t a]
cardinalNeighbs p =
  [liftA2 (+) p delta | delta <- cardinalDiffs]

-- All neighbour moves.
allDiffs :: (Traversable t, Applicative t, Num a) => [t a]
allDiffs =
  -- Take the tail as the first delta is the zero vector.
  tail $ sequence (pure [0, -1, 1])

-- | Get all neighbours (including diagonals) of a vector.
allNeighbs :: (Traversable t, Applicative t, Num a) => t a -> [t a]
allNeighbs p =
  [liftA2 (+) p delta | delta <- allDiffs]

-- | Get the manhattan distance between two vectors.
manhattan :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
manhattan x y = sum . abs $ x - y

-- | Get the bounding box of a collection of points.
boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> (g a, g a)
boundingBox =
  (\(Ap mi, Ap ma) -> (getMin <$> mi, getMax <$> ma))
    . foldMap1 (\p -> (Ap (Min <$> p), Ap (Max <$> p)))

-- | `boundingBox` that safely works on generic (possible empty) foldables.
boundingBox' :: (Foldable f, Applicative g, Ord a) => f (g a) -> Maybe (g a, g a)
boundingBox' = fmap boundingBox . NE.nonEmpty . toList

-- | Check if a point is in a bounding box.
inBoundingBox :: (Applicative g, Foldable g, Ord a) => (g a, g a) -> g a -> Bool
inBoundingBox (bMin, bMax) p = and $ go <$> p <*> bMin <*> bMax
  where
    go cp cmin cmax = cp >= cmin && cp <= cmax

-- | Parse String data into a Map
parse2dMap :: (Read a) => String -> Either String (Map (V2 Int) a)
parse2dMap = fmap createMap . traverse (traverse (readEither . pure)) . lines
  where
    createMap :: [[a]] -> Map (V2 Int) a
    createMap =
      M.fromList
        . concat
        . zipWith
          (\y -> zipWith (\x -> (V2 x y,)) [0 ..])
          [0 ..]

-- | Direction
-- Up, Right, Left and Down.
data Dir = U | R | D | L deriving (Show, Eq, Ord, Enum, Generic, NFData, Read)

-- | Rotate a direction.
dirRot :: Dir -> Dir -> Dir
dirRot U = id
dirRot R = \case
  U -> R
  R -> D
  D -> L
  L -> U
dirRot D = \case
  U -> D
  R -> L
  D -> U
  L -> R
dirRot L = \case
  U -> L
  R -> U
  D -> R
  L -> D

-- | Convert a direction to a unit vector in the 2D plane.
-- N.B. that x increases to the right, y increases going down.
dirPoint :: Dir -> V2 Int
dirPoint = \case
  U -> V2 0 (-1)
  R -> V2 1 0
  D -> V2 0 1
  L -> V2 (-1) 0

instance Semigroup Dir where
  (<>) :: Dir -> Dir -> Dir
  (<>) = dirRot

  stimes :: (Integral b) => b -> Dir -> Dir
  stimes n d = case n `mod` 4 of
    1 -> d
    2 -> d <> d
    3 -> case d of
      U -> U
      R -> L
      D -> D
      L -> R
    _ -> U

instance Monoid Dir where
  mempty :: Dir
  mempty = U

-- Use the shoelace theorem to get the area of the polygon with the
-- given vertices.
shoeLace :: Integral a => [V2 a] -> a
shoeLace =
  abs . (`div` 2) . go 0
  where
    go :: Num a => a -> [V2 a] -> a
    go a (V2 x1 y1 : p2@(V2 x2 y2) : xs) = go (a + (x1 * y2) - (x2 * y1)) (p2 : xs)
    go a _ = a
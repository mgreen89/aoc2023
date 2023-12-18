{-# LANGUAGE ScopedTypeVariables #-}

module AoC.Common.Graph
  ( aStar,
    dijkstra,
    explore,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ

insertIfBetter :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
insertIfBetter k p x q = case PSQ.lookup k q of
  Nothing -> PSQ.insert k p x q
  Just (p', _)
    | p < p' -> PSQ.insert k p x q
    | otherwise -> q

--------------------------------------
-- Path Finding
--------------------------------------

data AStarState n a = AStarState
  { -- | Queue of node to cost and list of parents.
    open :: !(OrdPSQ n a (a, [n])),
    -- | Map of node to list of parents.
    closed :: !(Map n [n])
  }
  deriving (Show)

aStar ::
  forall a n.
  (Ord a, Num a, Ord n) =>
  -- | Heuristic
  (n -> a) ->
  -- | Neighbours and costs
  (n -> Map n a) ->
  -- | Start
  n ->
  -- | Destination
  (n -> Bool) ->
  -- | Total cost if successful, and path taken.
  Maybe (a, [n])
aStar heuristic getNs start isDest =
  present <$> go (AStarState (PSQ.singleton start 0 (0, [])) M.empty)
  where
    present :: (n, a, AStarState n a) -> (a, [n])
    present (finish, cost, s) =
      (cost, reverse (finish : s.closed M.! finish))

    go :: AStarState n a -> Maybe (n, a, AStarState n a)
    go s = do
      (node, _, (cost, parents), open') <- PSQ.minView s.open
      let closed' = M.insert node parents s.closed
          s' = AStarState {open = open', closed = closed'}
          parents' = node : parents
      if isDest node
        then pure (node, cost, s')
        else go (M.foldlWithKey' (handleNeighbour (cost, parents')) s' (getNs node))

    handleNeighbour :: (a, [n]) -> AStarState n a -> n -> a -> AStarState n a
    handleNeighbour (cost, parents) s n nCost
      | M.member n s.closed = s
      | otherwise =
          s
            { open =
                insertIfBetter
                  n
                  (cost + nCost + heuristic n)
                  (cost + nCost, parents)
                  s.open
            }

dijkstra ::
  forall a n.
  (Ord a, Num a, Ord n) =>
  -- | Neighbours and costs
  (n -> Map n a) ->
  -- | Start
  n ->
  -- | Destination
  (n -> Bool) ->
  -- | Total cost if successful
  Maybe (a, [n])
dijkstra = aStar (const 0)

-- | Fully explore a graph using a dijkstra-like algorithm.
-- Returns a map of distances from the start node for every found node.
explore ::
  forall a n.
  (Ord a, Num a, Ord n) =>
  -- | Neighbours and costs
  (n -> Map n a) ->
  -- | Start
  n ->
  -- | Total cost if successful
  Map n a
explore getNs start =
  go M.empty (PSQ.singleton start 0 0)
  where
    go :: Map n a -> OrdPSQ n a a -> Map n a
    go visited unvisited =
      case step (visited, unvisited) of
        Just (v, uv) -> go v uv
        Nothing -> visited

    step :: (Map n a, OrdPSQ n a a) -> Maybe (Map n a, OrdPSQ n a a)
    step (v, uv) = do
      (currP, _, currV, uv') <- PSQ.minView uv
      let v' = M.insert currP currV v
      pure (v', M.foldlWithKey' (handleNeighbour currV) uv' (getNs currP))
      where
        handleNeighbour :: a -> OrdPSQ n a a -> n -> a -> OrdPSQ n a a
        handleNeighbour currCost q n nCost
          | M.member n v = q
          | otherwise =
              insertIfBetter
                n
                (currCost + nCost)
                (currCost + nCost)
                q

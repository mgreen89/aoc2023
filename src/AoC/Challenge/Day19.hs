{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module AoC.Challenge.Day19
  ( day19a,
    day19b,
  )
where

import AoC.Solution
import Control.DeepSeq (NFData)
import Control.Lens (Lens', (&), (.~), (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Char (isLower)
import Data.Foldable (foldl')
import Data.Interval (Boundary (..), Interval)
import qualified Data.Interval as IV
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IVS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

data Part a = Part {_x :: a, _m :: a, _a :: a, _s :: a}
  deriving (Eq, Generic, NFData, Show)

makeLenses ''Part

allParts :: Part a -> [a]
allParts p = (p ^.) <$> [x, m, a, s]

data Category = X | M | A | S
  deriving (Eq, Generic, NFData, Show)

lensOfC :: Category -> Lens' (Part a) a
lensOfC X = x
lensOfC M = m
lensOfC A = a
lensOfC S = s

data Comparator = Lt | Gt
  deriving (Eq, Generic, NFData, Show)

data Test = Test
  { _category :: Category,
    _comparator :: Comparator,
    _threshold :: Int
  }
  deriving (Eq, Generic, NFData, Show)

makeLenses ''Test

data Workflow
  = Rejected
  | Accepted
  | Named String
  deriving (Eq, Generic, NFData, Show)

data Rule
  = Condition Test Workflow
  | Always Workflow
  deriving (Eq, Generic, NFData, Show)

instructionParser :: MP.Parsec Void String (String, [Rule])
instructionParser = do
  name <- MP.takeWhile1P (Just "name") isLower
  MP.char '{'
  rules <- MP.sepBy ruleParser (MP.char ',')
  MP.char '}'
  pure (name, rules)
  where
    ruleParser :: MP.Parsec Void String Rule
    ruleParser = do
      ruleConstructor <- MP.choice [Condition <$> MP.try testParser, pure Always]
      target <-
        MP.choice
          [ Rejected <$ MP.char 'R',
            Accepted <$ MP.char 'A',
            Named <$> MP.takeWhile1P (Just "workflow") isLower
          ]
      pure $ ruleConstructor target

    testParser :: MP.Parsec Void String Test
    testParser =
      Test
        <$> MP.choice [X <$ MP.char 'x', M <$ MP.char 'm', A <$ MP.char 'a', S <$ MP.char 's']
        <*> MP.choice [Lt <$ MP.char '<', Gt <$ MP.char '>']
        <*> MPL.decimal
        <* MP.char ':'

partParser :: MP.Parsec Void String (Part Int)
partParser =
  Part
    <$> (MP.string "{x=" *> MPL.decimal <* MP.char ',')
    <*> (MP.string "m=" *> MPL.decimal <* MP.char ',')
    <*> (MP.string "a=" *> MPL.decimal <* MP.char ',')
    <*> (MP.string "s=" *> MPL.decimal <* MP.char '}')

inputParser :: MP.Parsec Void String (Map String [Rule], [Part Int])
inputParser = do
  instructions <- MP.many (instructionParser <* MP.space)
  parts <- MP.sepBy partParser (MP.char '\n')
  pure (M.fromList instructions, parts)

parse :: String -> Either String (Map String [Rule], [Part Int])
parse =
  first MP.errorBundlePretty . MP.parse inputParser "day19"

applyRule :: Part Int -> Rule -> Maybe Workflow
applyRule _ (Always dest) = Just dest
applyRule part (Condition test dest) =
  let cond = case test ^. comparator of
        Lt -> (<)
        Gt -> (>)
      l = lensOfC (test ^. category)
   in dest <$ guard (cond (part ^. l) (test ^. threshold))

applyWorkflow :: Part Int -> [Rule] -> Workflow
applyWorkflow _ [] = error "Rule without default"
applyWorkflow part (w : ws) =
  fromMaybe (applyWorkflow part ws) (applyRule part w)

applyWorkflows :: String -> Map String [Rule] -> Part Int -> Workflow
applyWorkflows name rules part =
  case applyWorkflow part (rules M.! name) of
    Named name' -> applyWorkflows name' rules part
    dest -> dest

solveA :: (Map String [Rule], [Part Int]) -> Int
solveA (wfs, ps) =
  sum . fmap rating . filter ((== Accepted) . applyWorkflows "in" wfs) $ ps
  where
    rating :: Part Int -> Int
    rating = sum . allParts

day19a :: Solution (Map String [Rule], [Part Int]) Int
day19a = Solution {sParse = parse, sShow = show, sSolve = Right . solveA}

type RangePart = Part (IntervalSet Int)

emptyRangePart :: RangePart
emptyRangePart = Part IVS.empty IVS.empty IVS.empty IVS.empty

anyEmpty :: RangePart -> Bool
anyEmpty = any IVS.null . allParts

ivSize :: Interval Int -> Int
ivSize iv =
  case (IV.lowerBound' iv, IV.upperBound' iv) of
    ((IV.Finite l, lb), (IV.Finite u, ub)) ->
      let lbreal = case lb of
            Open -> l + 1
            Closed -> l
          ubreal = case ub of
            Open -> u - 1
            Closed -> u
       in ubreal - lbreal + 1
    _ -> error "Infinite interval!"

ivsSize :: IntervalSet Int -> Int
ivsSize = sum . fmap ivSize . IVS.toList

possibilities :: RangePart -> Int
possibilities rp = product $ ivsSize <$> [rp ^. x, rp ^. m, rp ^. a, rp ^. s]

applyRangeRule :: Rule -> RangePart -> ((RangePart, RangePart), Workflow)
applyRangeRule (Always w) rp = ((rp, emptyRangePart), w)
applyRangeRule (Condition test w) rp =
  let testRange = IVS.singleton $ case test ^. comparator of
        Lt -> IV.Finite 1 IV.<=..< IV.Finite (test ^. threshold)
        Gt -> IV.Finite (test ^. threshold) IV.<..<= IV.Finite 4000

      (intersection, difference) =
        ( IVS.intersection (rp ^. l) testRange,
          IVS.difference (rp ^. l) testRange
        )

      -- This needs a type signature to compile.
      l :: Lens' (Part (IntervalSet Int)) (IntervalSet Int)
      l = lensOfC (test ^. category)
   in ((rp & l .~ intersection, rp & l .~ difference), w)

solveB :: Map String [Rule] -> Int
solveB wfs =
  go (wfs M.! "in") 0 (Part fullRange fullRange fullRange fullRange)
  where
    fullRange = IVS.singleton (IV.Finite 1 IV.<=..<= IV.Finite 4000)

    go :: [Rule] -> Int -> RangePart -> Int
    go rules acc part =
      fst $ foldl' go' (acc, part) rules

    go' :: (Int, RangePart) -> Rule -> (Int, RangePart)
    go' (acc, part) rule =
      let ((intersection, difference), wf) = applyRangeRule rule part
       in ( if anyEmpty intersection
              then acc
              else case wf of
                Rejected -> acc
                Accepted -> acc + possibilities intersection
                Named w -> go (wfs M.! w) acc intersection,
            difference
          )

day19b :: Solution (Map String [Rule]) Int
day19b = Solution {sParse = fmap fst . parse, sShow = show, sSolve = Right . solveB}

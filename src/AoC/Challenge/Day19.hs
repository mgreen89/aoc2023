module AoC.Challenge.Day19 (
  day19a,
  day19b,
  )
where

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Char (isLower)
import Data.Foldable (foldl')
import Data.Interval (Interval, Boundary(..))
import qualified Data.Interval as IV
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IVS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

import Debug.Trace

data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int }
  deriving (Eq, Generic, NFData, Show)

data Workflow = Rejected
              | Accepted
              | Named String
  deriving (Eq, Generic, NFData, Show)

data Rule = Rule { condition :: (Part -> Bool), target :: Workflow }
  deriving (Generic, NFData)

newtype Instruction = Instruction { rules :: [Rule] }
  deriving (Generic, NFData)

instructionParser :: MP.Parsec Void String (String, Instruction)
instructionParser = do
  name <- MP.takeWhile1P (Just "name") (isLower)
  MP.char '{'
  rules <- MP.sepBy ruleParser (MP.char ',')
  MP.char '}'
  pure (name, Instruction rules)
  where
    ruleParser :: MP.Parsec Void String Rule
    ruleParser = do
      condition <- MP.choice [MP.try conditionParser, (pure (const True))]
      target <- MP.choice [
        Rejected <$ MP.char 'R',
        Accepted <$ MP.char 'A',
        Named <$> MP.takeWhile1P (Just "workflow") (isLower)
        ]
      pure $ Rule condition target

    conditionParser :: MP.Parsec Void String (Part -> Bool)
    conditionParser = do
      getter <- MP.choice [
        (.x) <$ MP.char 'x',
        (.m) <$ MP.char 'm',
        (.a) <$ MP.char 'a',
        (.s) <$ MP.char 's'
        ]
      test <- MP.choice [
        (<) <$ MP.char '<',
        (>) <$ MP.char '>'
        ]
      val <- MPL.decimal
      MP.char ':'
      pure $ ((`test` val) . getter)

partParser :: MP.Parsec Void String Part
partParser = do
  MP.char '{'
  x <- MP.string "x=" *> MPL.decimal <* MP.char ','
  m <- MP.string "m=" *> MPL.decimal <* MP.char ','
  a <- MP.string "a=" *> MPL.decimal <* MP.char ','
  s <- MP.string "s=" *> MPL.decimal <* MP.char '}'
  pure $ Part x m a s

inputParser :: MP.Parsec Void String (Map String Instruction, [Part])
inputParser = do
  instructions <- MP.many (instructionParser <* MP.space)
  parts <- MP.sepBy partParser (MP.char '\n')
  pure (M.fromList instructions, parts)

parse :: String -> Either String (Map String Instruction, [Part])
parse =
  first MP.errorBundlePretty . MP.parse inputParser "day19"

accept :: Map String Instruction -> Part -> Bool
accept instrs p =
  go (instrs M.! "in")
  where
    go :: Instruction -> Bool
    go i =
      case evalRules i.rules of
        Rejected -> False
        Accepted -> True
        Named x -> go (instrs M.! x)

    evalRules :: [Rule] -> Workflow
    evalRules (r : rest) = if r.condition p then r.target else evalRules rest
    evalRules _ = error "Rule with no default case"
      
solveA :: (Map String Instruction, [Part]) -> Int
solveA (instrs, ps) =
  sum . fmap rating . filter (accept instrs) $ ps
  where
    rating :: Part -> Int
    rating p = p.x + p.m + p.a + p.s

day19a :: Solution (Map String Instruction, [Part]) Int
day19a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

data RangePart = RangePart { x :: IntervalSet Int, m :: IntervalSet Int, a :: IntervalSet Int, s :: IntervalSet Int }
  deriving (Eq, Generic, NFData, Show)

-- Can't just do this - duh!
union :: RangePart -> RangePart -> RangePart
union a b =
  RangePart
    (IVS.union a.x b.x)
    (IVS.union a.m b.m)
    (IVS.union a.a b.a)
    (IVS.union a.s b.s)

data RangeRule = RangeRule { applyRange :: (RangePart -> Maybe RangePart), target :: Workflow }
  deriving (Generic, NFData)

newtype RangeInstruction = RangeInstruction { rules :: [RangeRule] }
  deriving (Generic, NFData)

rangeParser :: MP.Parsec Void String (Map String RangeInstruction)
rangeParser = do
  ranges <- MP.some $ do
      name <- MP.takeWhile1P (Just "name") (isLower)
      MP.char '{'
      rules <- MP.sepBy rangeRuleParser (MP.char ',')
      MP.char '}'
      MP.space
      pure (name, RangeInstruction rules)
  pure $ M.fromList ranges

  where
    rangeRuleParser :: MP.Parsec Void String RangeRule
    rangeRuleParser = do
      range <- MP.choice [MP.try rangeConditionParser, (pure (Just))]
      target <- MP.choice [
        Rejected <$ MP.char 'R',
        Accepted <$ MP.char 'A',
        Named <$> MP.takeWhile1P (Just "workflow") (isLower)
        ]
      pure $ RangeRule range target

    rangeConditionParser :: MP.Parsec Void String (RangePart -> Maybe RangePart)
    rangeConditionParser = do
      (get, set) <- MP.choice [
        ((.x), (\rp a -> (rp { x = a }) :: RangePart)) <$ MP.char 'x',
        ((.m), (\rp a -> (rp { m = a }) :: RangePart)) <$ MP.char 'm',
        ((.a), (\rp a -> (rp { a = a }) :: RangePart)) <$ MP.char 'a',
        ((.s), (\rp a -> (rp { s = a }) :: RangePart)) <$ MP.char 's'
        ]
      rangeGen <- MP.choice [
        (\v -> IV.Finite 0 IV.<..< IV.Finite v) <$ MP.char '<',
        (\v -> IV.Finite v IV.<..<= IV.Finite 4000) <$ MP.char '>'
        ]
      val <- MPL.decimal
      MP.char ':'
      let rs = IVS.singleton (rangeGen val)
      pure $ (\rp ->
        let is = IVS.intersection (get rp) rs
        in if (IVS.null is) then Nothing else Just (set rp is))

parseB :: String -> Either String (Map String RangeInstruction)
parseB =
  first MP.errorBundlePretty . MP.parse rangeParser "day19b"

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

poss :: RangePart -> Int
poss rp = (ivsSize rp.x) * (ivsSize rp.m) * (ivsSize rp.a) * (ivsSize rp.s)

solveB :: Map String RangeInstruction -> Int
solveB m = 
  poss . traceShowId $ go (m M.! "in") (RangePart fullRange fullRange fullRange fullRange)
  where
    fullRange = IVS.singleton (IV.Finite 1 IV.<=..<= IV.Finite 4000)

    go :: RangeInstruction -> RangePart -> RangePart
    go ri rp =
      foldl' (go' rp) (RangePart IVS.empty IVS.empty IVS.empty IVS.empty) ri.rules

    go' :: RangePart -> RangePart -> RangeRule -> RangePart 
    go' rp a rr =
        case rr.applyRange rp of
          Nothing -> (trace "no overlap" a)
          Just rp' -> case rr.target of
            Rejected -> (trace ("rejected " ++ show rp') a)
            Accepted -> (trace ("accepted " ++ (show rp')) union a rp')
            Named w -> (trace ("going to " ++ w ++ " " ++ (show rp')) union a (go (m M.! w) rp'))

day19b :: Solution (Map String RangeInstruction) Int
day19b = Solution{sParse = parseB, sShow = show, sSolve = Right . solveB }

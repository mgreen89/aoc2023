{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day19 (
  day19a,
  )
where

-- , day19b

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Char (isLower)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

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

day19b :: Solution _ _
day19b = Solution{sParse = Right, sShow = show, sSolve = Right}

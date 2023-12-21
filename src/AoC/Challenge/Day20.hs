{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day20
  ( day20a,
  )
where

-- , day20b

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Char (isAsciiLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq, ViewL (..))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

newtype FlipFlopState = FFS {on :: Bool}
  deriving (Generic, NFData, Show)

newtype ConjunctionState = CS {mem :: Map String Bool}
  deriving (Generic, NFData, Show)

data ModuleState
  = Broadcast
  | FlipFlop FlipFlopState
  | Conjunction ConjunctionState
  deriving (Generic, NFData, Show)

type Module = (ModuleState, [String])

inputParser :: MP.Parsec Void String (Map String Module)
inputParser =
  M.fromList <$> MP.sepBy moduleParser (MP.char '\n')
  where
    moduleParser :: MP.Parsec Void String (String, Module)
    moduleParser = do
      modConstruct <-
        MP.choice
          [ (,(Broadcast,)) <$> MP.string "broadcaster",
            (,(FlipFlop (FFS False),)) <$> (MP.char '%' *> modNameParser),
            (,(Conjunction (CS M.empty),)) <$> (MP.char '&' *> modNameParser)
          ]
      MP.string " -> "
      dests <- MP.sepBy modNameParser (MP.string ", ")
      pure (($ dests) <$> modConstruct)

    modNameParser :: MP.Parsec Void String String
    modNameParser = MP.takeWhile1P (Just "Module name") isAsciiLower

parse :: String -> Either String (Map String [String], Map String ModuleState)
parse =
  fmap (\m -> (M.map snd m, M.mapWithKey (findConjunctionInputs m) m))
    . first MP.errorBundlePretty
    . MP.parse inputParser "day20"
  where
    findConjunctionInputs :: Map String Module -> String -> Module -> ModuleState
    findConjunctionInputs mp s (m, _) =
      case m of
        Broadcast -> m
        FlipFlop _ -> m
        Conjunction _ ->
          let hasDest (_, (_, nds)) = elem s nds
              allLow =
                M.fromList
                  . fmap (fmap (const False))
                  . filter hasDest
                  . M.toList
                  $ mp
           in Conjunction (CS allLow)

data State = State
  { dests :: Map String [String],
    states :: Map String ModuleState,
    pulses :: Seq (String, String, Bool), -- (From, To, High?)
    nHigh :: Int,
    nLow :: Int
  }
  deriving (Show)

solveA :: Map String [String] -> Map String ModuleState -> Int
solveA destMap stateMap =
  (\s -> s.nHigh * s.nLow)
    . (!! 1000)
    . iterate pushButton
    $ State destMap stateMap Seq.empty 0 0
  where
    pushButton :: State -> State
    pushButton s =
      go $ s {pulses = ("button", "broadcaster", False) Seq.<| s.pulses}

    go :: State -> State
    go s =
      case Seq.viewl s.pulses of
        ((p, d, h) Seq.:< ps) ->
          let s' =
                s
                  { pulses = ps,
                    nHigh = s.nHigh + (if h then 1 else 0),
                    nLow = s.nLow + (if h then 0 else 1)
                  }
              addPulses isH = ps Seq.>< Seq.fromList [(d, x, isH) | x <- s.dests M.! d]
           in maybe
                (go s')
                ( go . \case
                    Broadcast -> s' {pulses = addPulses h}
                    FlipFlop ffs ->
                      if h
                        then s'
                        else
                          let ffs' = FFS {on = not ffs.on}
                              m' = M.insert d (FlipFlop ffs') s.states
                           in if ffs'.on
                                then s' {states = m', pulses = addPulses True}
                                else s' {states = m', pulses = addPulses False}
                    Conjunction cs ->
                      let cs' = M.insert p h cs.mem
                          m' = M.insert d (Conjunction (CS cs')) s.states
                       in if and (M.elems cs')
                            then s' {states = m', pulses = addPulses False}
                            else s' {states = m', pulses = addPulses True}
                )
                (M.lookup d s.states)
        EmptyL -> s

day20a :: Solution (Map String [String], Map String ModuleState) Int
day20a = Solution {sParse = parse, sShow = show, sSolve = Right . uncurry solveA}

day20b :: Solution _ _
day20b = Solution {sParse = Right, sShow = show, sSolve = Right}

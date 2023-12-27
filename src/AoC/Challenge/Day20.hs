module AoC.Challenge.Day20
  ( day20a,
    day20b,
  )
where

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Char (isAsciiLower)
import Data.Foldable (foldl', toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Sequence (Seq, ViewL (..))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

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

data Pulse = Pulse
  { from :: String,
    to :: String,
    high :: Bool
  }
  deriving (Show)

data State = State
  { dests :: Map String [String],
    states :: Map String ModuleState,
    pulses :: Seq Pulse,
    history :: Seq Pulse
  }
  deriving (Show)

initialState :: Map String [String] -> Map String ModuleState -> State
initialState dests states = State dests states Seq.empty Seq.empty

pushButton :: State -> State
pushButton state =
  go $ state {pulses = Pulse "button" "broadcaster" False Seq.<| state.pulses}
  where
    go :: State -> State
    go s =
      case Seq.viewl s.pulses of
        (pulse Seq.:< ps) ->
          let s' = s {pulses = ps, history = s.history Seq.|> pulse}
              addPulses high = ps Seq.>< Seq.fromList [Pulse pulse.to to high | to <- s.dests M.! pulse.to]
           in go $
                maybe
                  s'
                  ( \case
                      Broadcast -> s' {pulses = addPulses pulse.high}
                      FlipFlop ffs ->
                        if pulse.high
                          then s'
                          else
                            let ffs' = FFS {on = not ffs.on}
                                m' = M.insert pulse.to (FlipFlop ffs') s.states
                             in if ffs'.on
                                  then s' {states = m', pulses = addPulses True}
                                  else s' {states = m', pulses = addPulses False}
                      Conjunction cs ->
                        let cs' = M.insert pulse.from pulse.high cs.mem
                            m' = M.insert pulse.to (Conjunction (CS cs')) s.states
                         in if and (M.elems cs')
                              then s' {states = m', pulses = addPulses False}
                              else s' {states = m', pulses = addPulses True}
                  )
                  (M.lookup pulse.to s.states)
        EmptyL -> s

solveA :: Map String [String] -> Map String ModuleState -> Int
solveA destMap stateMap =
  uncurry (*)
    . foldl' getHighLow (0, 0)
    . (.history)
    . (!! 1000)
    . iterate pushButton
    $ initialState destMap stateMap
  where
    getHighLow :: (Int, Int) -> Pulse -> (Int, Int)
    getHighLow (h, l) p = if p.high then (h + 1, l) else (h, l + 1)

day20a :: Solution (Map String [String], Map String ModuleState) Int
day20a = Solution {sParse = parse, sShow = show, sSolve = Right . uncurry solveA}

{-
  From a manual inspection of the input, there are four inputs to the
  final rx input.
  Do each of these cycle and need to find LCM?

  kh feeds rx
  pv, qh, xm and hz feed kh.
  Let's start by running a few iterations and seeing when they flip.

  On inspection, they do cycle.
  They are also all cycle before any reach two, so just iterate
  until they'll all cycle (should be <5k, but do <10k to be sure) and
  get the LCM of their entries.
-}

solveB :: Map String [String] -> Map String ModuleState -> Int
solveB destMap stateMap =
  foldl' lcm 1
    . fmap fst
    . take 4
    . mapMaybe (\(i, (_, h)) -> listToMaybe [(i, x) | x <- wanted, q <- h, x == q.from, q.high])
    . zip [0 ..]
    . take 10000
    . iterate (\(s, _) -> let s' = pushButton s in (s' {history = Seq.empty}, toList s'.history))
    $ (initialState destMap stateMap, [])
  where
    wanted = ["pv", "qh", "xm", "hz"]

day20b :: Solution (Map String [String], Map String ModuleState) Int
day20b = Solution {sParse = parse, sShow = show, sSolve = Right . uncurry solveB}

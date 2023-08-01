module Day10 where

import Common
import Data.List (scanl')
import Data.List.Split (chunksOf)
import Data.Void
import System.FilePath ((</>))
import Tests
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)

type CharParser a = Parsec Void String a

data Instruction = NoOp | Add Int deriving (Show, Eq)

type Cycle = Int

data CPUState = CPUState
  { register :: Int,
    cpuCycle :: Cycle
  }

day :: IO ()
day = do
  let easy = Problem parseDay solveEasy show
  let hard = Problem parseDay solveHard unlines
  let inputFilename = "app" </> "inputs" </> "10.txt"
  runProblem inputFilename easy
  runProblem inputFilename hard
  runTests testsEasy

-- runTests testsHard

solveHard :: [Instruction] -> Maybe [String]
solveHard inst = chunksOf screenWidth <$> traverse (choosePixel states) pixels
  where
    pixels = [0 .. 239]
    screenWidth = 40
    states = eval inst

    choosePixel :: [CPUState] -> Int -> Maybe Char
    choosePixel states pixel = do
      registerVal <- register <$> findStateDuring states pixel
      pure $
        if (registerVal - (pixel `mod` screenWidth)) `elem` [-1 .. 1]
          then 'O'
          else ' '

solveEasy :: [Instruction] -> Maybe Int
solveEasy inst =
  sum
    . zipWith (*) cycles
    <$> traverse (fmap register . findStateAfter states) cycles
  where
    states = eval inst
    cycles = [20, 60, 100, 140, 180, 220]

findStateAfter :: [CPUState] -> Cycle -> Maybe CPUState
findStateAfter = findState (<)

findStateDuring :: [CPUState] -> Cycle -> Maybe CPUState
findStateDuring = findState (<=)

findState :: (Cycle -> Cycle -> Bool) -> [CPUState] -> Cycle -> Maybe CPUState
findState comp states cycle =
  safeLast $ filter ((`comp` cycle) . cpuCycle) states

eval :: [Instruction] -> [CPUState]
eval = scanl' step (CPUState 1 0)
  where
    step :: CPUState -> Instruction -> CPUState
    step (CPUState reg cycle) NoOp = CPUState reg (cycle + 1)
    step (CPUState reg cycle) (Add x) = CPUState (reg + x) (cycle + 2)

parseDay :: String -> Maybe [Instruction]
parseDay = parseMaybe (parserInstruction `sepEndBy1` eol <* eof)

parserInstruction :: CharParser Instruction
parserInstruction = parserNoOp <|> parserAdd

parserNoOp :: CharParser Instruction
parserNoOp = string "noop" >> pure NoOp

parserAdd :: CharParser Instruction
parserAdd = Add <$> (string "addx " >> L.signed hspace L.decimal)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

testsEasy :: Tests String String
testsEasy =
  Tests
    { run = runProblemString (Problem parseDay solveEasy show),
      inputs =
        [ "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"
        ],
      expectedOutputs = ["13140"]
    }

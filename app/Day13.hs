module Day13 where

import Common (Problem (Problem), runProblem)
import qualified Control.Monad
import Data.List (sort, sortBy)
import Data.List.Split (splitOn)
import Data.Void (Void)
import System.FilePath ((</>))
import Tests ()
import Text.Megaparsec (Parsec, between, many, parseMaybe, sepBy, (<|>), MonadParsec (eof), sepEndBy)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal, lexeme)

type CharParser a = Parsec Void String a

data Cell
  = Num Integer
  | List List
  deriving (Show, Eq)

data TriBool = T | F | N
  deriving (Eq)

type List = [Cell]

day :: IO ()
day = do
  let easy = Problem parseEasy solveEasy show
  let hard = Problem parseHard solveHard show
  let inputFilename = "app" </> "inputs" </> "13.txt"
  runProblem inputFilename hard

solveHard :: [List] -> Maybe Int
solveHard ins = Just $ snd (head (filter (\(l, i) -> l == firstDivider) wIdx)) * snd (head (filter (\(l, i) -> l == secondDivider) wIdx))
  where
    firstDivider = [List [List [Num 6]]]
    secondDivider = [List [List [Num 2]]]
    inputs = firstDivider : secondDivider : ins
    sorted = sortBy comparison inputs
    wIdx = zip sorted [1 ..]

comparison :: List -> List -> Ordering
comparison fsts snds = case isRightOrder fsts snds of
  T -> LT
  F -> GT
  N -> EQ

solveEasy :: [(List, List)] -> Maybe Integer
solveEasy input = Just $ sum $ map snd $ filter (\(res, idx) -> res == T) $ zip results [1 ..]
  where
    results = map (uncurry isRightOrder) input

isRightOrder :: [Cell] -> [Cell] -> TriBool
isRightOrder (Num left : ls) (Num right : rs)
  | left < right = T
  | left == right = isRightOrder ls rs
  | otherwise = F
isRightOrder ((Num left) : ls) rs@((List _) : _) = isRightOrder (List [Num left] : ls) rs
isRightOrder ls@((List _) : _) (Num right : rs) = isRightOrder ls (List [Num right] : rs)
isRightOrder (List left : ls) (List right : rs)
  | isRightOrder left right == N = isRightOrder ls rs
  | otherwise = isRightOrder left right
isRightOrder [] [] = N
isRightOrder [] rs = T
isRightOrder ls [] = F

parseHard :: String -> Maybe [List]
parseHard = parseMaybe (sepEndBy parseList (many eol) <* eof)

parseEasy :: String -> Maybe [(List, List)]
parseEasy = parseMaybe (sepBy parsePair eol)

parsePair :: CharParser (List, List)
parsePair = do
  first <- parseList
  eol
  second <- parseList
  eol
  return (first, second)

parseCell :: CharParser Cell
parseCell = lexeme (Control.Monad.void (many (char ' '))) $ fmap Num decimal <|> fmap List parseList

parseList :: CharParser [Cell]
parseList = between (char '[') (char ']') $ sepBy parseCell (char ',')

module Day13 where

import Common (Problem (Problem), runProblem)
import qualified Control.Monad
import Data.List.Split (splitOn)
import Data.Void (Void)
import System.FilePath ((</>))
import Tests ()
import Text.Megaparsec (Parsec, between, many, parseMaybe, sepBy, (<|>))
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal, lexeme)

type CharParser a = Parsec Void String a

data Cell
  = Num Integer
  | List List
  deriving (Show)

data TriBool = T | F | N
  deriving (Eq)

type List = [Cell]

day :: IO ()
day = do
  let easy = Problem parseDay solveEasy show
  let inputFilename = "app" </> "inputs" </> "13.txt"
  runProblem inputFilename easy

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

parseDay :: String -> Maybe [(List, List)]
parseDay = parseMaybe (sepBy parsePair eol)

parsePair :: CharParser (List, List)
parsePair = do
  first <- parseList
  eol
  second <- parseList
  eol
  return (first, second)

parseCell :: CharParser Cell
parseCell =
  lexeme
    (Control.Monad.void (many (char ' ')))
    $ fmap Num decimal <|> fmap List (between (char '[') (char ']') parseList)

parseList :: CharParser [Cell]
parseList = sepBy parseCell (char ',')

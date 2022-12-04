
module Day01 where

import Text.Read (readMaybe)
import Data.List (sort)
import Data.List.Split (splitOn)
import System.FilePath ((</>))

import Common

day :: IO ()
day = do
    let easy = Problem myParse solveEasy show
    let hard = Problem myParse solveHard show
    let inputFilename = "app" </> "inputs" </> "01.txt"
    runProblem inputFilename easy
    runProblem inputFilename hard

solveEasy :: [[Int]] -> Maybe Int
solveEasy foods = Just $ maximum $ map sum foods

solveHard :: [[Int]] -> Maybe Int
solveHard foods = Just $ sum $ take 3 $ reverse $ sort $ map sum foods

myParse :: String -> Maybe [[Int]]
myParse s = traverse (traverse readMaybe . words) (elfs s)
    where
        elfs :: String -> [String]
        elfs = splitOn "\n\n"

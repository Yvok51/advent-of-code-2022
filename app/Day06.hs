module Day06 where

import Text.Read (readMaybe)
import System.FilePath ((</>))
import Data.List (inits, elemIndex)

import Common
import Tests

day :: IO ()
day = do
    let easy = Problem parseEasy solveEasy show
    let hard = Problem parseEasy solveHard show
    let inputFilename = "app" </> "inputs" </> "06.txt"
    runProblem inputFilename easy
    runProblem inputFilename hard
    -- runTests testsEasy

solveEasy :: [Char] -> Maybe Int
solveEasy = solve06 4

solveHard :: [Char] -> Maybe Int
solveHard = solve06 14

solve06 :: Int -> [Char] -> Maybe Int
solve06 windowSize s = fmap (+ windowSize) $ elemIndex True $ map allUnique $ windowed windowSize s
    where
        allUnique :: [Char] -> Bool
        allUnique []     = True
        allUnique (x:xs) = notElem x xs && allUnique xs

        windowed :: Int -> [Char] -> [[Char]]
        windowed a b = map (take a . reverse) $ drop a $ inits b

parseEasy :: String -> Maybe [Char]
parseEasy = Just

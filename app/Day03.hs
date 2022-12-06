
module Day03 where

import System.FilePath ((</>))
import Data.Set (fromList, toList, intersection)
import Data.Char (isLower, isUpper, ord)

import Common
import Tests

day :: IO ()
day = do
    let easy = Problem parseEasy solveEasy show
    let hard = Problem parseHard solveHard show
    let inputFilename = "app" </> "inputs" </> "03.txt"
    runProblem inputFilename easy
    runProblem inputFilename hard
    -- runTests testsEasy

solveEasy :: [(String, String)] -> Maybe Int
solveEasy s = Just $ sum $ map (sum . map letterValue . toList . uncurry intersection . bimap fromList fromList) s

solveHard :: [(String, String, String)] -> Maybe Int
solveHard s = Just $ sum $ map (sum . map letterValue . toList . intersectThree . trimap fromList fromList fromList) s
    where
        intersectThree (a, b, c) = intersection c $ intersection a b

parseEasy :: String -> Maybe [(String, String)]
parseEasy = Just . map halve . lines
    where       
        halve :: String -> (String, String)
        halve s = splitAt (length s `div` 2) s

parseHard :: String -> Maybe [(String, String, String)]
parseHard = Just . threesomes . lines
    where
        threesomes :: [a] -> [(a, a, a)]
        threesomes (a:b:c:xs) = (a, b, c) : threesomes xs
        threesomes _ = []

lowerCaseLetters = ord 'z' - ord 'a' + 1

letterValue :: Char -> Int
letterValue c | isLower c = ord c - ord 'a' + 1
                | isUpper c = ord c - ord 'A' + 1 + lowerCaseLetters
                | otherwise = 0

trimap :: (a -> b) -> (c -> d) -> (e -> f) -> (a, c, e) -> (b, d, f)
trimap f g h (a, b, c) = (f a, g b, h c)

testsEasy :: Tests String String
testsEasy = Tests {run = runProblemString (Problem parseEasy solveEasy show)
                  , inputs = ["vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"]
                  , expectedOutputs = ["157"]}


            
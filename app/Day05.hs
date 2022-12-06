
module Day05 where

import qualified Data.Map as M
import Text.Read (readMaybe)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.List.Split (splitOn)
import System.FilePath ((</>))
import Control.Monad (join)

import Tests
import Common

data Move = Move {from :: Int, to :: Int, amount :: Int} deriving (Show, Eq)

data Stack

day :: IO ()
day = do
    let easy = Problem parseEasy (uncurry solveEasy) id
    let hard = Problem parseEasy (uncurry solveHard) show
    let inputFilename = "app" </> "inputs" </> "05.txt"
    runProblem inputFilename easy
    runProblem inputFilename hard
    runTests testsEasy
    -- runTests testsMove
    -- runTests testsHard

solveEasy :: [[Char]] -> [Move] -> Maybe String
solveEasy crates moves = Just $ map head $ makeMoves performMove crates moves
    where
        performMove :: [[Char]] -> Move -> [[Char]]
        performMove crates move = let (toMove, left) = splitAt (amount move) (crates !! from move)
                                      newStack       = reverse toMove ++ (crates !! to move) 
                                  in replace (to move - 1) newStack $ replace (from move) left crates

solveHard :: [[Char]] -> [Move] -> Maybe String
solveHard crates moves = Just $ map head $ makeMoves performMove crates moves
    where
        performMove :: [[Char]] -> Move -> [[Char]]
        performMove crates move = let (toMove, left) = splitAt (amount move) (crates !! (from move - 1))
                                      newStack       = toMove ++ (crates !! (to move - 1)) 
                                  in replace (to move - 1) newStack $ replace (from move - 1) left crates

makeMoves :: ([[Char]] -> Move -> [[Char]]) -> [[Char]] -> [Move] -> [[Char]]
makeMoves performMove crates moves = foldl performMove crates 
                $ map (\m -> Move (from m - 1) (to m - 1) (amount m)) moves

parseEasy :: String -> Maybe ([[Char]],[Move])
parseEasy s = join $ fmap present 
                $ fmap (\(crates, moves) -> (parseCrates (dropLast (lines crates)), parseMoves moves))
                $ makeTuple $ splitOn "\n\n" s
    where
        parseMoves :: String -> Maybe [Move]
        parseMoves = mapM parseMove . lines

        parseMove :: String -> Maybe Move
        parseMove line = createMove (readMaybe (list !! 3)) (readMaybe (list !! 5)) (readMaybe (list !! 1))
            where
                list = words line

                createMove :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Move
                createMove (Just a) (Just b) (Just c) = Just $ Move a b c
                createMove _ _ _ = Nothing
        
        parseCrates :: [String] -> [[Char]]
        parseCrates s = map catMaybes $ transpose $ map (map parseCreate . cutUp crateLength) s
            where
                crateLength = 4

                parseCreate :: String -> Maybe Char
                parseCreate s = case dropWhile (/= '[') s of
                    ('[':c:']':_) -> Just c
                    _ -> Nothing

cutUp :: Int -> String -> [String]
cutUp a [] = []
cutUp a s  = take a s : cutUp a (drop a s)

dropLast :: [a] -> [a]
dropLast xs = take (length xs - 1) xs

present :: (a, Maybe b) -> Maybe (a, b)
present (a, Just b) = Just (a, b)
present _ = Nothing

replace :: Int -> a -> [a] -> [a]
replace n x xs = take n xs ++ [x] ++ drop (n + 1) xs


testsEasy :: Tests String String
testsEasy = Tests {run = runProblemString (Problem parseEasy (uncurry solveEasy) id)
                  , inputs = ["    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"]
                  , expectedOutputs = ["CMZ"]}

-- testsMove :: Tests ([[Char]], Move) [[Char]]
-- testsMove = Tests { run = Just . uncurry performMove
--                   , inputs = [(["NZ", "DCM", "P"], Move 2 1 1), (["DNZ", "CM", "P"], Move 1 3 3)]
--                   , expectedOutputs = [["DNZ", "CM", "P"], ["", "CM", "ZNDP"]]
--                   }
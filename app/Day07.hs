module Day07 where

import Common
import Data.List (foldl', intersperse, tails)
import qualified Data.Map as M
import System.FilePath ((</>))
import Tests
import Text.Read (readMaybe)

day :: IO ()
day = do
  let easy = Problem parseEasy solveEasy show
  let hard = Problem parseEasy solveHard show
  let inputFilename = "app" </> "inputs" </> "07.txt"
  runProblem inputFilename easy
  runProblem inputFilename hard

-- runTests testsEasy
-- runTests testsHard

solveHard :: M.Map [String] Int -> Maybe Int
solveHard m = case needToFree of
  Just n -> safeMinimum $ filter (>= n) $ M.elems m
  Nothing -> Nothing
  where
    available = 70000000
    needed = 30000000

    used = M.lookup [] m
    free = fmap (available -) used
    needToFree = fmap (needed -) free

solveEasy :: M.Map [String] Int -> Maybe Int
solveEasy = Just . sum . M.filter (<= 100000)

parseEasy :: String -> Maybe (M.Map [String] Int)
parseEasy s = Just $ fst $ foldl' readCommand (M.empty, []) $ lines s

readCommand :: (M.Map [String] Int, [String]) -> String -> (M.Map [String] Int, [String])
readCommand (m, path) command = case words command of
  ["$", "cd", ".."] -> (m, tail path)
  ["$", "cd", "/"] -> (m, [])
  ["$", "cd", relPath] -> (m, relPath : path)
  [sizeOrDir, name] -> case readMaybe sizeOrDir :: Maybe Int of
    Just size -> (updateAllPaths' m (getPaths path) size, path)
    Nothing -> (m, path)
  _ -> (m, path)

-- updateAllPaths :: M.Map String Int -> [String] -> Int -> M.Map String Int
-- updateAllPaths m paths size = foldl' (\m' path -> M.insertWith (+) path size m') m paths

updateAllPaths' :: M.Map [String] Int -> [[String]] -> Int -> M.Map [String] Int
updateAllPaths' m paths size = M.unionWith (+) m $ M.fromList $ zip paths (repeat size)

getPaths :: [String] -> [[String]]
getPaths = tails -- map (concat . (:) "/" . intersperse "/") $

safeMinimum :: Ord a => [a] -> Maybe a
safeMinimum [] = Nothing
safeMinimum xs = Just $ minimum xs

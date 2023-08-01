{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use mapMaybe" #-}
{-# HLINT ignore "Use map once" #-}
module Day12 where

import Common
import Data.Char (isAsciiLower, ord)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq (Empty, (:<|)), fromList, singleton, (><))
import Data.Set (Set, empty, fold, insert, member, notMember)
import Data.Void
import System.FilePath ((</>))
import Tests
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Height = Int

type Point = (Int, Int)

data Step = Step Point Int

data Heights = Heights [[Height]] Int Int
  deriving (Show)

day :: IO ()
day = do
  let easy = Problem parseDay solveEasy show
  let hard = Problem parseDayHard solveHard show
  let inputFilename = "app" </> "inputs" </> "12.txt"
  -- runTests testsEasy
  -- runTests testsHard
  -- runProblem inputFilename easy
  runProblem inputFilename hard

solveHard :: (Heights, [Point], Point) -> Maybe Int
solveHard (heights, startPos, endPos) = Just $ minimum correctSolutions
  where
    starts = map (\p -> (heights, p, endPos)) startPos
    solutions = map solveEasy starts
    correctSolutions = catMaybes solutions

solveEasy :: (Heights, Point, Point) -> Maybe Int
solveEasy (heights, startPos, endPos) = bfs heights Data.Set.empty $ singleton startPoint
  where
    startPoint = (startPos, 0)

    bfs :: Heights -> Set Point -> Seq (Point, Int) -> Maybe Int
    bfs _ _ Empty = Nothing
    bfs heights visited ((p, s) :<| xs)
      | p == endPos = Just s
      | otherwise = bfs heights newVisited $ xs >< fromList toVisit
      where
        toVisit = filter (\(p, s) -> notMember p visited) (step (p, s) heights)
        newVisited = foldl (\set (p, s) -> insert p set) visited toVisit

step :: (Point, Int) -> Heights -> [(Point, Int)]
step (p, s) heights = map (\newP -> (newP, s + 1)) $ possibleNeihgbors p heights

possibleNeihgbors :: Point -> Heights -> [Point]
possibleNeihgbors point (Heights heights lenX lenY) = map fst possible
  where
    pointElevation = getElevation heights point
    neigbors = getNeighborPoints point lenX lenY
    withElevation = zip neigbors $ map (getElevation heights) neigbors
    possible = filter (\(p, e) -> canClimb pointElevation e) withElevation

getNeighborPoints :: Point -> Int -> Int -> [Point]
getNeighborPoints (x, y) lenX lenY =
  filter (\(x, y) -> x >= 0 && x < lenX && y >= 0 && y < lenY) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

getElevation :: [[Height]] -> Point -> Height
getElevation heights (x, y) = (heights !! y) !! x

canClimb :: Height -> Height -> Bool
canClimb currHeight target = currHeight + 1 >= target

-- Parsing

parseDayHard :: String -> Maybe (Heights, [Point], Point)
parseDayHard input = Just (Heights heights (length (head heights)) (length heights), startPos, endPos)
  where
    rows = lines input
    heights = map (map elevation) rows
    startPos = findPositions rows 'a'
    endPos = head $ findPositions rows 'E'

parseDay :: String -> Maybe (Heights, Point, Point)
parseDay input = Just (Heights heights (length (head heights)) (length heights), startPos, endPos)
  where
    rows = lines input
    heights = map (map elevation) rows
    startPos = head $ findPositions rows 'S'
    endPos = head $ findPositions rows 'E'

elevation :: Char -> Height
elevation c
  | isAsciiLower c = ord c - ord 'a'
  | c == 'S' = elevation 'a'
  | c == 'E' = elevation 'z'

findPositions :: [[Char]] -> Char -> [Point]
findPositions heights wantedChar = map snd $ filter (\(c, pos) -> c == wantedChar) $ concat $ decorateHeights heights

decorateHeights :: [[Char]] -> [[(Char, Point)]]
decorateHeights heights = map addXCoord $ addYCoord heights
  where
    addYCoord :: [[Char]] -> [[(Char, Int)]]
    addYCoord = go 0
      where
        go _ [] = []
        go y (row : rows) = map (\c -> (c, y)) row : go (y + 1) rows

    addXCoord :: [(Char, Int)] -> [(Char, Point)]
    addXCoord = go 0
      where
        go _ [] = []
        go x ((c, y) : others) = (c, (x, y)) : go (x + 1) others

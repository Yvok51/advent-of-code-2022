module Day14 where

import Common (Problem (Problem), runProblem)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, insert, notMember)
import System.FilePath ((</>))

data Point = Point Int Int
  deriving (Show, Eq, Ord)

day :: IO ()
day = do
  let easy = Problem parseEasy solveEasy show
  -- let hard = Problem parseHard solveHard show
  let inputFilename = "app" </> "inputs" </> "14.txt"
  runProblem inputFilename easy

solveEasy :: (Set Point, Point) -> Maybe Int
solveEasy (points, lowestBlock) = Just $ length $ go points lowestBlock
  where
    go :: Set Point -> Point -> [Set Point]
    go still lowestBlock = case nextStill of
      (Just next) -> next : go next lowestBlock
      Nothing -> []
      where
        nextStill = iteration still lowestBlock sandStartPosition

iteration :: Set Point -> Point -> Point -> Maybe (Set Point)
iteration still lowestBlock@(Point _ minY) sand@(Point x y)
  | isAtRest still sand = Just $ insert sand still
  | y > minY = Nothing
  | otherwise = iteration still lowestBlock $ step still sand

step :: Set Point -> Point -> Point
step still curr = head $ availablePositions still curr

isAtRest :: Set Point -> Point -> Bool
isAtRest still sand = null (availablePositions still sand)

availablePositions :: Set Point -> Point -> [Point]
availablePositions still sand = filter (`notMember` still) $ nextPositions sand

nextPositions :: Point -> [Point]
nextPositions (Point x y) = [Point x (y + 1), Point (x - 1) (y + 1), Point (x + 1) (y + 1)]

sandStartPosition :: Point
sandStartPosition = Point 500 0

parseEasy :: String -> Maybe (Set Point, Point)
parseEasy input = Just (fromList points, lowestBlock points)
  where
    stringPoints = map (splitOn " -> ") $ lines input
    toPoint s = case "," `splitOn` s of
      [x, y] -> Point (read x) (read y)
      xs -> error $ "Unexpected amount of elements in list: " ++ show xs ++ "\nOriginal string: " ++ s
    pointLines = map (map toPoint) stringPoints

    createLines :: [Point] -> [Point]
    createLines (p1@(Point x1 y1) : p2@(Point x2 y2) : xs)
      | x1 == x2 = map (Point x1) [min y1 y2 .. max y1 y2] ++ restOfLines
      | y1 == y2 = map (`Point` y1) [min x1 x2 .. max x1 x2] ++ restOfLines
      | otherwise = [p1, p2] ++ restOfLines
      where
        restOfLines = createLines (p2 : xs)
    createLines [p] = [p]
    createLines [] = []

    points = concatMap createLines pointLines

lowestBlock :: [Point] -> Point
lowestBlock points = go points (head points)
  where
    go (p@(Point _ y) : ps) min@(Point _ minY) = if y > minY then go ps p else go ps min
    go [] min = min

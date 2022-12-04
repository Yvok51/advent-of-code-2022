
module Day02 where

import System.FilePath ((</>))

import Common
import Tests

data Shape = Rock 
           | Paper 
           | Scissors 
           deriving (Show, Eq)

data Outcome = Win 
             | Loss 
             | Draw 
             deriving (Show, Eq)

(<<) :: Shape -> Shape -> Outcome
a << b | a == b = Draw
       | next a == b = Win
       | a == next b = Loss 

next :: Shape -> Shape
next Rock = Paper
next Paper = Scissors
next Scissors = Rock

pointsOutcome :: Outcome -> Int
pointsOutcome Win = 6
pointsOutcome Draw = 3
pointsOutcome Loss = 0

pointsShape :: Shape -> Int
pointsShape Rock = 1
pointsShape Paper = 2
pointsShape Scissors = 3

day :: IO ()
day = do
    let easy = Problem parseEasy solveEasy show
    let hard = Problem parseHard solveHard show
    let inputFilename = "app" </> "inputs" </> "02.txt"
    runProblem inputFilename easy
    runProblem inputFilename hard

solveEasy :: [(Shape, Shape)] -> Maybe Int
solveEasy rounds = Just $ shapePoints + outcomePoints
    where
        shapePoints = sum $ map (pointsShape . snd) rounds
        outcomePoints = sum $ map (pointsOutcome . uncurry (<<)) rounds

solveHard :: [(Shape, Outcome)] -> Maybe Int
solveHard = Just . sum . map (\(s, o) -> pointsShape (transform s o) + pointsOutcome o)
    where
        transform :: Shape -> Outcome -> Shape
        transform opponent Win = next opponent
        transform opponent Draw = opponent
        transform opponent Loss = next $ next opponent

parseEasy :: String -> Maybe [(Shape, Shape)]
parseEasy s = mapM ((makeMaybeTuple . map parseShape) . words) $ lines s
    where
        parseShape :: String -> Maybe Shape
        parseShape "A" = Just Rock
        parseShape "X" = Just Rock
        parseShape "B" = Just Paper
        parseShape "Y" = Just Paper
        parseShape "C" = Just Scissors
        parseShape "Z" = Just Scissors
        parseShape _ = Nothing

        makeMaybeTuple :: [Maybe a] -> Maybe (a, a)
        makeMaybeTuple [Just a, Just b] = Just (a, b)
        makeMaybeTuple _ = Nothing

parseHard :: String -> Maybe [(Shape, Outcome)]
parseHard s = sequence $ map ((=<<) fromMaybeTuple) $ map (fmap (\(op, out) -> (parseShape op, parseOutcome out))) $ map makeTuple $ map words $ lines s
    where
        parseShape :: String -> Maybe Shape
        parseShape "A" = Just Rock
        parseShape "B" = Just Paper
        parseShape "C" = Just Scissors
        parseShape _   = Nothing

        parseOutcome :: String -> Maybe Outcome
        parseOutcome "X" = Just Loss
        parseOutcome "Y" = Just Draw
        parseOutcome "Z" = Just Win
        parseOutcome _   = Nothing

        fromMaybeTuple :: (Maybe a, Maybe b) -> Maybe (a, b)
        fromMaybeTuple (Just a, Just b) = Just (a, b)
        fromMaybeTuple _ = Nothing

makeTuple :: [a] -> Maybe (a, a)
makeTuple [a, b] = Just (a, b)
makeTuple _ = Nothing


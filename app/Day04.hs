module Day04 where

import Text.Read (readMaybe)
import System.FilePath ((</>))

import Common
import Tests

data Range = Range Int Int deriving (Show, Eq)

day :: IO ()
day = do
    let easy = Problem parseEasy solveEasy show
    let hard = Problem parseHard solveHard show
    let inputFilename = "app" </> "inputs" </> "04.txt"
    runProblem inputFilename easy
    runProblem inputFilename hard
    -- runTests testsEasy
    -- runTests testsHard

solveEasy :: [(Range, Range)] -> Maybe Int
solveEasy input = Just $ countElem True $ map (uncurry isWhollyContained) input
    where
        isWhollyContained :: Range -> Range -> Bool
        isWhollyContained (Range a b) (Range c d) = (a >= c && b <= d) || (a <= c && b >= d)

solveHard :: [(Range, Range)] -> Maybe Int
solveHard input = Just $ countElem True $ map (uncurry isPartiallyContained) input
    where
        isPartiallyContained :: Range -> Range -> Bool
        isPartiallyContained (Range a b) (Range c d) = ((a >= c && a <= d) || (b >= c && b <= d)) || ((c >= a && c <= b) || (d >= a && d <= b))

parseEasy :: String -> Maybe [(Range, Range)]
parseEasy s = mapM (liftTuple . bimap parseRange parseRange . breakElem ',') $ lines s
    where
        parseRange :: String -> Maybe Range
        parseRange s = uncurry createRange $ bimap readMaybe readMaybe $ breakElem '-' s

        createRange :: Maybe Int -> Maybe Int -> Maybe Range
        createRange (Just a) (Just b) = Just $ Range a b
        createRange _ _ = Nothing

        liftTuple :: (Maybe a, Maybe b) -> Maybe (a, b)
        liftTuple (Just a, Just b) = Just (a, b)
        liftTuple _ = Nothing

parseHard :: String -> Maybe [(Range, Range)]
parseHard = parseEasy

breakElem :: Char -> String -> (String, String)
breakElem c s = (takeWhile (/= c) s, drop 1 $ dropWhile (/= c) s)

countElem :: (Eq a) => a -> [a] -> Int
countElem a = length . filter (== a)

testsEasy :: Tests String String
testsEasy = Tests {run = runProblemString (Problem parseEasy solveEasy show)
                  , inputs = ["2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"]
                  , expectedOutputs = ["2"]}

testsHard :: Tests String String
testsHard = Tests {run = runProblemString (Problem parseHard solveHard show)
                  , inputs = ["2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"]
                  , expectedOutputs = ["4"]}
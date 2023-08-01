module Day08 where

import Common
import Data.List
  ( scanl',
    transpose,
  )
import Data.Void
import System.FilePath ((</>))
import Tests
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)

type CharParser a = Parsec Void String a

type Tree = Int

day :: IO ()
day = do
  let easy = Problem parseEasy solveEasy show
  let hard = Problem parseEasy solveHard show
  let inputFilename = "app" </> "inputs" </> "08.txt"
  runProblem inputFilename easy
  runProblem inputFilename hard
  runTests testsEasy
  runTests testsHard

solveEasy :: [[Tree]] -> Maybe Int
solveEasy = Just . sum . map (countElem True) . isVisible
  where
    isVisible :: [[Tree]] -> [[Bool]]
    isVisible = solve08 (||) visibleFromLeft

    visibleFromLeft :: [Tree] -> [Bool]
    visibleFromLeft row = zipWith (>) row $ maximums row

    maximums :: [Tree] -> [Int]
    maximums =
      init . scanl' (\max el -> if el > max then el else max) (minBound :: Int)

solveHard :: [[Tree]] -> Maybe Int
solveHard = Just . maximum . map maximum . productVisible
  where
    productVisible :: [[Tree]] -> [[Int]]
    productVisible = solve08 (*) visibleToRight

    visible :: Tree -> [Tree] -> Int
    visible _ [] = 0
    visible height trees
      | length (takeWhile (< height) trees) == length trees = length trees
      | otherwise = 1 + length (takeWhile (< height) trees)

    visibleToRight :: [Tree] -> [Int]
    visibleToRight (x : xs) = visible x xs : visibleToRight xs
    visibleToRight [] = []

solve08 :: (b -> b -> b) -> ([a] -> [b]) -> [[a]] -> [[b]]
solve08 combine fromLeftTrans bs =
  zipWith
    (zipWith combine)
    (zipWith (zipWith combine) (fromLeft bs) (fromRight bs))
    (zipWith (zipWith combine) (fromTop bs) (fromBottom bs))
  where
    fromLeft = map fromLeftTrans

    fromRight = map reverse . fromLeft . map reverse

    fromTop = transpose . fromLeft . transpose

    fromBottom = transpose . map reverse . fromLeft . map reverse . transpose

parseEasy :: String -> Maybe [[Tree]]
parseEasy = parseMaybe parserForest

parserTree :: CharParser Tree
parserTree = do
  c <- digitChar
  return $ read [c]

parserRow :: CharParser [Tree]
parserRow = L.lexeme space $ some parserTree

parserForest :: CharParser [[Tree]]
parserForest = some parserRow

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)

testsEasy :: Tests String String
testsEasy =
  Tests
    { run = runProblemString (Problem parseEasy solveEasy show),
      inputs = ["30373\n25512\n65332\n33549\n35390"],
      expectedOutputs = ["21"]
    }

testsHard :: Tests String String
testsHard =
  Tests
    { run = runProblemString (Problem parseEasy solveHard show),
      inputs = ["30373\n25512\n65332\n33549\n35390"],
      expectedOutputs = ["8"]
    }

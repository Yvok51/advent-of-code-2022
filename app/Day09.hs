module Day09 where

import Common
import Data.List
  ( foldl',
    scanl',
    transpose,
  )
import qualified Data.Set as S
import Data.Void
import System.FilePath ((</>))
import Tests
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)

type CharParser a = Parsec Void String a

data Direction = Direction
  { dx :: Int,
    dy :: Int
  }
  deriving (Show, Eq)

data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq)

instance Ord Position where
  (Position x1 y1) <= (Position x2 y2) = x1 < x2 || (x1 == x2 && y1 <= y2)

data RelativePosition = RelativePosition
  { rx :: Int,
    ry :: Int
  }
  deriving (Show, Eq)

data Move = Move Direction Int
  deriving (Show, Eq)

day :: IO ()
day = do
  let easy = Problem parseDay solveEasy show
  let hard = Problem parseDay solveHard show
  let inputFilename = "app" </> "inputs" </> "09.txt"
  runProblem inputFilename easy
  runProblem inputFilename hard
  runTests testsEasy
  runTests testsHard

solveEasy :: [Move] -> Maybe Int
solveEasy = Just . S.size . S.fromList . visitedPositions
  where
    visitedPositions :: [Move] -> [Position]
    visitedPositions moves =
      snd $
        foldl' (uncurry performMove) (Position 0 0, [Position 0 0]) $
          unrollMoves moves

    performMove :: Position -> [Position] -> Direction -> (Position, [Position])
    performMove h ts dir =
      (newH, updatePosition t (neededStep $ newH `relativeTo` t) : ts)
      where
        t = head ts
        newH = updatePosition h dir

solveHard :: [Move] -> Maybe Int
solveHard = Just . S.size . S.fromList . visitedPositions
  where
    visitedPositions :: [Move] -> [Position]
    visitedPositions moves =
      thd3
        $ foldl'
          performMove
          (Position 0 0, replicate knotNumber (Position 0 0), [Position 0 0])
        $ unrollMoves moves

    knotNumber = 9

    performMove ::
      (Position, [Position], [Position]) ->
      Direction ->
      (Position, [Position], [Position])
    performMove (head, rope, ts) dir = (newHead, newRope, newTs)
      where
        newHead = updatePosition head dir
        newRope =
          tail $
            scanl'
              (\prev curr -> updatePosition curr (neededStep (prev `relativeTo` curr)))
              newHead
              rope
        newTs = last newRope : ts

neededStep :: RelativePosition -> Direction
neededStep (RelativePosition dx dy)
  | abs dx >= 2 || abs dy >= 2 = Direction (signum dx) (signum dy)
  | otherwise = Direction 0 0

unrollMoves :: [Move] -> [Direction]
unrollMoves = concatMap replicateMove
  where
    replicateMove :: Move -> [Direction]
    replicateMove (Move dir num) = replicate num dir

updatePosition :: Position -> Direction -> Position
updatePosition (Position x y) (Direction dx dy) = Position (x + dx) (y + dy)

relativeTo :: Position -> Position -> RelativePosition
relativeTo (Position hx hy) (Position tx ty) =
  RelativePosition (hx - tx) (hy - ty)

parseDay :: String -> Maybe [Move]
parseDay = parseMaybe parserMoves
  where
    parserMoves = do
      parserMove `sepEndBy1` eol

parseMove :: String -> Maybe Move
parseMove = parseMaybe parserMove

parserMove :: CharParser Move
parserMove = do
  dir <-
    choice
      [ char 'U' >> pure (Direction 0 1),
        char 'D' >> pure (Direction 0 (-1)),
        char 'L' >> pure (Direction (-1) 0),
        char 'R' >> pure (Direction 1 0)
      ]
  char ' '
  Move dir <$> L.decimal

testsEasy :: Tests String String
testsEasy =
  Tests
    { run = runProblemString (Problem parseDay solveEasy show),
      inputs = ["R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"],
      expectedOutputs = ["13"]
    }

testsHard :: Tests String String
testsHard =
  Tests
    { run = runProblemString (Problem parseDay solveHard show),
      inputs =
        [ "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2",
          "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"
        ],
      expectedOutputs = ["1", "36"]
    }

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

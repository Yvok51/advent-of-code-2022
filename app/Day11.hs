module Day11 where

import           Control.Monad.Combinators.Expr ( Operator(InfixL)
                                                , makeExprParser
                                                )
import           Data.List                      ( foldl'
                                                , sort
                                                )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromJust )
import           Data.Void
import           System.FilePath                ( (</>) )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Read                      ( readMaybe )

import           Common
import           Tests

type CharParser a = Parsec Void String a

type Worry = Int

data Expr = Add Expr Expr
          | Mul Expr Expr
          | Lit Int
          | Var String
          deriving (Show, Eq)

data Monkey = Monkey
  { items   :: [Worry]
  , update  :: Worry -> Worry
  , toThrow :: Worry -> Int
  , counter :: Int
  }

instance Show Monkey where
  show (Monkey items update throw counter) =
    "{ Items: " <> show items <> ", Counter: " <> show counter <> " }"

day :: IO ()
day = do
  let easy          = Problem parseDay solveEasy show
  let hard          = Problem parseDay solveHard show
  let inputFilename = "app" </> "inputs" </> "11.txt"
  runTests testsEasy
  -- runTests testsHard
  runProblem inputFilename easy
  runProblem inputFilename hard

solveEasy :: [Monkey] -> Maybe Int
solveEasy =
  Just
    . product
    . take 2
    . reverse
    . sort
    . map (\(Monkey is _ _ c) -> c - length is)
    . last
    . take 21
    . iterate (performRound (`div` 3))

solveHard :: [Monkey] -> Maybe Int
solveHard =
  Just
    . product
    . take 2
    . reverse
    . sort
    . map (\(Monkey is _ _ c) -> c - length is)
    . last
    . take 10001
    . iterate (performRound (`mod` modulo))
  where modulo = 17 * 7 * 13 * 2 * 19 * 3 * 5 * 11

performRound :: (Int -> Int) -> [Monkey] -> [Monkey]
performRound u ms = foldl' (performTurn u) ms [0 .. length ms - 1]

performTurn :: (Int -> Int) -> [Monkey] -> Int -> [Monkey]
performTurn u ms idx = newMonkeys
 where
  m@(Monkey _ u t c) = ms !! idx
  newWorries =
    map ((`mod` (17 * 7 * 13 * 2 * 19 * 3 * 5 * 11)) . update m) $ items m
  thrownTo   = map (toThrow m) newWorries
  newMonkeys = replace idx (Monkey [] u t c)
    $ map (addItemsToMonkey (zip thrownTo newWorries)) (indexed ms)

  addItemsToMonkey :: [(Int, Worry)] -> (Int, Monkey) -> Monkey
  addItemsToMonkey is (idx, m) = addItems m itemsToAdd
    where itemsToAdd = map snd $ filter ((== idx) . fst) is

addItems :: Monkey -> [Worry] -> Monkey
addItems = foldl' addItem

addItem :: Monkey -> Worry -> Monkey
addItem (Monkey items u t c) item = Monkey (items <> [item]) u t (c + 1)

-------------------------------------------------------------------------------

mkMonkey :: [Worry] -> (Worry -> Worry) -> (Worry -> Int) -> Monkey
mkMonkey items update throw = Monkey items update throw (length items)

evalExpr :: M.Map String Int -> Expr -> Maybe Int
evalExpr m (Add l r) = do
  x <- evalExpr m l
  y <- evalExpr m r
  pure $ x + y
evalExpr m (Mul l r) = do
  x <- evalExpr m l
  y <- evalExpr m r
  pure $ x * y
evalExpr m (Lit v) = Just v
evalExpr m (Var s) = M.lookup s m

parseDay :: String -> Maybe [Monkey]
parseDay = parseMaybe (parserMonkey `sepEndBy` space)

parserMonkey :: CharParser Monkey
parserMonkey = do
  space >> string "Monkey " >> L.decimal >> string ":" >> space
  items <- parserItems
  space
  update <- parserOp
  space
  throw <- parserThrow
  space
  pure $ mkMonkey items update throw

parseMonkey = parseMaybe parserMonkey
parseItems = parseMaybe parserItems
parseUpdate = parseMaybe parserOp
parseThrow = parseMaybe parserThrow

parserItems :: CharParser [Worry]
parserItems =
  hspace >> string "Starting items: " >> L.decimal `sepBy` string ", "

parserThrow :: CharParser (Worry -> Int)
parserThrow = do
  space >> string "Test: divisible by "
  d <- L.decimal
  space >> string "If true: throw to monkey "
  tru <- L.decimal
  space >> string "If false: throw to monkey "
  fals <- L.decimal
  pure $ \x -> if x `mod` d == 0 then tru else fals

parserOp :: CharParser (Worry -> Worry)
parserOp = hspace >> string "Operation: new = " >> do
  makeUpdate <$> parserExpression
 where
  parserExpression :: CharParser Expr
  parserExpression = makeExprParser parserTerm operatorTable

makeUpdate :: Expr -> (Worry -> Worry)
makeUpdate e = \x -> fromJust $ evalExpr (M.fromList [("old", x)]) e

operatorTable :: [[Operator (Parsec Void String) Expr]]
operatorTable = [[binary "*" Mul], [binary "+" Add]]

binary
  :: String -> (Expr -> Expr -> Expr) -> Operator (Parsec Void String) Expr
binary name f = InfixL (f <$ hlexeme (string name))

hlexeme :: CharParser a -> CharParser a
hlexeme = L.lexeme hspace

lexeme :: CharParser a -> CharParser a
lexeme = L.lexeme space

parserTerm :: CharParser Expr
parserTerm = parserVar <|> parserInt

parserVar :: CharParser Expr
parserVar = Var "old" <$ hlexeme (string "old")

parserInt :: CharParser Expr
parserInt = do
  Lit <$> hlexeme L.decimal

indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]

replace :: Int -> a -> [a] -> [a]
replace n x xs = take n xs ++ [x] ++ drop (n + 1) xs

testsEasy :: Tests String String
testsEasy = Tests
  { run             = runProblemString (Problem parseDay solveEasy show)
  , inputs          =
    [ "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"
    ]
  , expectedOutputs = ["10605"]
  }

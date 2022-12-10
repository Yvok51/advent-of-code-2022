{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common where

import           Data.Maybe                     ( fromMaybe )
import           Debug.Trace                    ( traceShowM )

data Difficulty = Easy | Hard

data Problem a b = Problem
  { parseInput  :: String -> Maybe a
  , solve       :: a -> Maybe b
  , printOutput :: b -> String
  }

runProblem :: (Show a, Show b) => FilePath -> Problem a b -> IO ()
runProblem path p = do
  input <- readFile path

  let maybeResult = do
        a <- parseInput p $ input
        traceShowM a
        b <- solve p $ a
        traceShowM b
        pure $ printOutput p $ b

  case maybeResult of
    Just result -> putStrLn result
    Nothing     -> putStrLn "ohno, error!"

runProblemString :: (Show a, Show b) => Problem a b -> String -> Maybe String
runProblemString p input = do
  a <- parseInput p $ input
  traceShowM a
  b <- solve p $ a
  traceShowM b
  return $ printOutput p $ b


eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = \case
  Left  err -> Nothing
  Right xs  -> Just xs

bimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
bimap f g p = (f (fst p), g (snd p))

makeTuple :: [a] -> Maybe (a, a)
makeTuple [a, b] = Just (a, b)
makeTuple _      = Nothing


module Tests where

import Debug.Trace ( traceShowM )

import Common (Problem, bimap)

data Tests a b = Tests {run :: String -> Maybe String, inputs:: [String], expectedOutputs :: [String]}

runTests :: (Show a, Show b) => Tests a b -> IO()
runTests tests = do
    let results = zip (map (run tests) (inputs tests)) (expectedOutputs tests)
    traceShowM results
    let errors = filter (not . presentAndEqual) results
    case errors of
        [] -> putStrLn "All tests passed!"
        _ -> putStrLn $ "Tests failed: " ++ show (annotateErrors errors)

  where
    annotateErrors :: [(Maybe String, String)] -> [(String, String)]
    annotateErrors = map (bimap (\m -> "Output: " ++ show m) ("Expected: " ++))

    presentAndEqual :: (Eq a) => (Maybe a, a) -> Bool
    presentAndEqual (Just a, b) = a == b
    presentAndEqual (Nothing, _) = False
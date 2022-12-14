module Dec14 (run) where

import Prelude hiding (round)

import Test(test)

run :: String -> IO ()
run input =
  do
    unitTest
    let parsed = parse input
    putStrLn "Task 1:"
    print $ task1 parsed
    putStrLn "Task 2:"
    print $ task2 parsed

{- Parsing -}

-- parse :: String -> (Troop, Hold)
parse input = "not implemented"

{- Task 1 -}

-- task1 :: Troop -> Hold -> Int
task1 troop = "not implemented"

{- Task 2 -}

-- task2 :: Troop -> Hold -> Int
task2 _ = "not implemented"

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    validateExample

example = unlines [
  "498,4 -> 498,6 -> 496,6",
  "503,4 -> 502,4 -> 502,9 -> 494,9"
  ]

validateExample :: IO ()
validateExample = return ()

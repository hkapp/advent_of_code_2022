module Dec15 (run) where

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

parse = id

{- Coordinate system -}

type Pos = (Int, Int)

{- Task 1 -}

-- task1 :: Cave -> Int
task1 = id

{- Task 2 -}

-- task2 :: Cave -> Int
task2 = id

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    validateExample

example = unlines [
  "498,4 -> 498,6 -> 496,6",
  "503,4 -> 502,4 -> 502,9 -> 494,9"
  ]

exampleCave = parse example

exampleDisp = unlines [
  "..........",
  "..........",
  "..........",
  "..........",
  "....#...##",
  "....#...#.",
  "..###...#.",
  "........#.",
  "........#.",
  "#########."
  ]

validateExample :: IO ()
validateExample = return ()
  -- do
    -- testFmtStr "display example" exampleDisp (showCave exampleCave)
    -- test "task1 example" 24 (task1 exampleCave)
    -- test "task2 example" 93 (task2 exampleCave)

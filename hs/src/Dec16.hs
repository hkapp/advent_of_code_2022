module Dec16 (run) where

import Test(test)

run :: String -> IO ()
run input =
  do
    unitTest
    let parsed = parse input
    putStrLn "Task 1:"
    testRun "task1" Nothing (task1 parsed)
    putStrLn "Task 2:"
    testRun "task2" Nothing (task2 parsed)
  where
    testRun name expected found =
      do
        case expected of
          Just expres -> test (name ++ " (real input)") expres found
          Nothing     -> return ()
        print found

{- Parsing -}

-- parse :: String -> Field
parse = id

{- Task 1 -}

-- task1 :: Field -> Int
task1 = id

{- Task 2 -}

-- task2 :: Field -> Integer
task2 = id

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    validateExample

example = unlines [
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
  "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
  "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
  "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
  "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
  "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
  "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
  "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
  "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
  "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
  "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
  "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
  "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
  "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
  ]

exampleParsed = parse example

validateExample :: IO ()
validateExample = return ()
  -- do
    -- test "task1 example" [(-2, 24)] (fieldInfluenceOnLine 10 exampleParsed)
    -- test "task1 example" 26 (task1Param 10 exampleParsed)
    -- test "task2 example" 56000011 (task2Param 0 20 exampleParsed)

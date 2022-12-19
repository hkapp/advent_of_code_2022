module Dec4 where

run :: String -> IO ()
run input =
  do
    unitTest
    putStrLn "Task 1:"
    print $ task1 $ lines input
    putStrLn "Task 2:"
    print $ task2 $ lines input

{- Task 1 -}

type Range = (Int, Int)

parseRangePairs :: String -> (Range, Range)
parseRangePairs = both (both read) . both (splitFirstSep '-') . splitFirstSep ','

task1 :: [String] -> Int
task1 lines = length $ filter eitherSubsumed $ parseRangePairs <$> lines

splitFirstSep :: (Eq a) => a -> [a] -> ([a], [a])
splitFirstSep sep xs =
  let (prefix, suffix) = span ((/=) sep) xs
  in (prefix, tail suffix)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

eitherSubsumed :: (Range, Range) -> Bool
eitherSubsumed (x, y) = (x `subsumes` y) || (y `subsumes` x)

subsumes :: Range -> Range -> Bool
subsumes (parentLow, parentHigh) (childLow, childHigh) =
  (parentLow <= childLow) && (parentHigh >= childHigh)

-- {- Task 2 -}

task2 :: [String] -> Int
task2 lines = length $ filter (uncurry overlap) $ parseRangePairs <$> lines

-- 1 2 3 . .
-- . 2 3 4 .
--   ^ ^     overlap
-- . 2 3 4 .
-- 1 2 3 . .
--   ^ ^     overlap
-- . 2 3 . .
-- 1 2 3 4 .
--   ^ ^     overlap
-- . . 3 4 .
-- 1 2 . . .
--           disjoint
overlap :: Range -> Range -> Bool
overlap x y = not $ disjoint x y
  where disjoint (xLow, xHigh) (yLow, yHigh) = (xLow > yHigh) || (yLow > xHigh)

{- Unit tests -}

unitTest :: IO ()
unitTest =
  do
    validateExample

test :: (Eq a, Show a) => String -> a -> a -> IO ()
test name expected found =
  if (expected == found)
    then return ()
    else
      do
        putStrLn $ "[FAIL] " ++ name
        putStrLn $ "Expected: " ++ (show expected)
        putStrLn $ "Found:    " ++ (show found)

example = [
  "2-4,6-8",
  "2-3,4-5",
  "5-7,7-9",
  "2-8,3-7",
  "6-6,4-6",
  "2-6,4-8"
  ]

validateExample =
  do
    test "task1 example" 2 (task1 example)
    test "task2 example" 4 (task2 example)

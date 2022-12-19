module Dec1 where

import Data.List(unfoldr, sortOn)
import Data.Ord(Down(..))

run :: String -> IO ()
run input =
  do
    let calories = elfCalories $ lines input
    putStrLn "Task 1:"
    print $ task1 calories
    putStrLn "Task 2:"
    print $ task2 calories

(<$$>) :: (a -> b) -> [[a]] -> [[b]]
(<$$>) f = map (map f)

intoBatches :: [String] -> [[Int]]
intoBatches lines = read <$$> (splitUntil null lines)

elfCalories :: [String] -> [Int]
elfCalories lines = sum <$> intoBatches lines

task1 :: [Int] -> Int
task1 calories = maximum calories

task2 :: [Int] -> Int
task2 calories = sum $ take 3 $ sortOn Down calories

splitUntil :: (a -> Bool) -> [a] -> [[a]]
splitUntil p = unfoldr f
  where
    f [] = Nothing
    f xs =
      let (matchingPrefix, remainderWithSep) = span (not . p) xs
          remainderWithoutSep = drop 1 remainderWithSep
      in Just (matchingPrefix, remainderWithoutSep)

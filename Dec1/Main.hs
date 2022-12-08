import Data.List(unfoldr, sortOn)
import Data.Ord(Down(..))

main :: IO ()
main =
  do
    lines <- inputLines
    let calories = elfCalories lines
    putStrLn "Task 1:"
    print $ task1 calories
    putStrLn "Task 2:"
    print $ task2 calories

filename = "day1.data.txt"

inputLines :: IO [String]
inputLines = lines <$> readFile filename

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

import qualified Data.Set as Set
import Data.Set(Set)
import Data.Char(ord)
import Data.Foldable(traverse_)
import Data.List(unfoldr)

main :: IO ()
main =
  do
    unitTest
    lines <- inputLines
    putStrLn "Task 1:"
    print $ task1 lines
    putStrLn "Task 2:"
    print $ task2 lines

filename = "day3.data.txt"

inputLines :: IO [String]
inputLines = lines <$> readFile filename

{- Task 1 -}

task1 :: [String] -> Int
task1 lines = sum $ (priority . head . Set.toList . commonItems . both Set.fromList . splitInHalf) <$> lines
  where commonItems (x, y) = overlap x y

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

overlap :: Ord a => Set a -> Set a -> Set a
overlap xs ys = Set.filter (containedIn ys) xs
  where containedIn s x = Set.member x s

splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = splitAt (div (length xs) 2) xs

priority :: Char -> Int
priority c | between 'a' 'z' c = (ord c) - (ord 'a') + 1
priority c | between 'A' 'Z' c = (ord c) - (ord 'A') + 27

between :: Ord a => a -> a -> a -> Bool
between low high x = (x >= low) && (x <= high)

{- Task 2 -}

task2 rucksacks = sum $ (priority . commonItem) <$> groups rucksacks

groups :: [String] -> [[String]]
groups = splitEvery 3

-- This is normally defined in Data.Split
splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = unfoldr f xs
  where f [] = Nothing
        f ys = Just $ splitAt n ys

commonItem :: Ord a => [[a]] -> a
commonItem xxs =
  let sets = Set.fromList <$> xxs
      commonSet = foldl1 overlap sets
  in head $ Set.toList commonSet

{- Unit tests -}

unitTest :: IO ()
unitTest =
  do
    testPriority
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

testPriority = yes [('a', 1), ('z', 26), ('A', 27), ('Z', 52),
                    ('p', 16), ('L', 38), ('P', 42), ('v', 22), ('t', 20), ('s', 19)]
  where yes = traverse_ no
        no :: (Char, Int) -> IO ()
        no (c, n) = test ("priority '" ++ [c] ++ "'") n (priority c)

example = [
  "vJrwpWtwJgWrhcsFMMfFFhFp",
  "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
  "PmmdzqPrVvPwwTWBwg",
  "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
  "ttgJtRGJQctTZtZT",
  "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]

validateExample =
  do
    test "task1 example" 157 (task1 example)
    test "sum $ priority ['p', 'L', 'P', 'v', 't', 's']" 157 (sum $ priority <$> ['p', 'L', 'P', 'v', 't', 's'])
    test "task2 example" 70 (task2 example)

module Dec6 (run) where

import Data.List(foldl')

run :: IO ()
run =
  do
    unitTest
    input <- readFile filename
    putStrLn "Task 1:"
    print $ task1 input
    putStrLn "Task 2:"
    print $ task2 input

filename = "../../data/day6.data.txt"

process :: Int -> String -> Int
process ndis = finalize . foldl' (signalOne ndis) newReader

data ReaderState =
  Searching [Char] Int
  | Found Int

newReader :: ReaderState
newReader = Searching [] 0

signalOne :: Int -> ReaderState -> Char -> ReaderState
signalOne ndis (Searching s n) c = search ndis (readOne ndis s c) (n + 1)
signalOne ndis (Found n) _       = Found n

search :: Int -> [Char] -> Int -> ReaderState
search ndis s n | n >= ndis && allDifferent s = Found n
search ndis s n                               = Searching s n

allDifferent :: [Char] -> Bool
allDifferent (x:xs) = (not $ elem x xs) && (allDifferent xs)
allDifferent []     = True

readOne :: Int -> [Char] -> Char -> [Char]
readOne ndis s c | length s < ndis = s ++ [c]
readOne ndis s c                   = (tail s) ++ [c]

finalize :: ReaderState -> Int
finalize (Found n) = n

{- Task 1 -}

task1 :: String -> Int
task1 = process 4

{- Task 2 -}

task2 :: String -> Int
task2 = process 14

{- Unit test -}

unitTest :: IO ()
unitTest =
  do
    validateExample

test :: (Eq a, Show a) => String -> a -> a -> IO ()
test message expected found =
  if expected == found
    then return ()
    else
      do
        putStrLn $ "[FAIL] " ++ message
        putStrLn $ "Expected: " ++ (show expected)
        putStrLn $ "Found:    " ++ (show found)

examples = [
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
  "bvwbjplbgvbhsrlpgdmjqwftvncz",
  "nppdvjthqldpwncqszvftbrmjlhg",
  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]

validateExample =
  do
    test "task1 examples" [7, 5, 6, 10, 11] (task1 <$> examples)
    test "task2 examples" [19, 23, 23, 29, 26] (task2 <$> examples)

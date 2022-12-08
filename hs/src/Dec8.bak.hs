module Dec8 (run) where

import Test(test)

import Data.List(transpose, unfoldr)
import Data.Bifunctor(second)

run :: String -> IO ()
run input =
  do
    unitTest
    let forest = parse input
    putStrLn "Task 1:"
    print $ task1 forest
    putStrLn "Task 2:"
    print $ task2 forest

{- Parsing -}

data Forest = Forest {
  trows :: [[Int]],
  tcols :: [[Int]]
  }
  deriving Show

parse :: String -> Forest
parse input = intoForest $ parseRow <$> lines input

intoForest :: [[Int]] -> Forest
intoForest rows = Forest rows (transpose rows)

parseRow :: [Char] -> [Int]
parseRow = map (read . return)

{- Forest -}

data Tree = Tree Int Int
  deriving Show

neighbourHeights :: Forest -> Tree -> [[Int]]
neighbourHeights (Forest rows cols) (Tree x y) =
  let
    (left, right) = splitAtAndDrop (rows !! y) x
    (top, down) = splitAtAndDrop (cols !! x) y
  in
    [left, right, top, down]

-- Need to access the right row or column!
splitAtAndDrop :: [a] -> Int -> ([a], [a])
splitAtAndDrop xs idx = second tail $ splitAt idx xs

{- Task 1 -}

task1 :: Forest -> Int
task1 forest =
  let
    irow = indexes $ trows forest
    icol = indexes $ tcols forest
    trees = (uncurry Tree) <$> allCombinations irow icol
  in
    length $ filter (visible forest) trees

allCombinations :: [a] -> [b] -> [(a, b)]
allCombinations xs ys =
  do
    x <- xs
    map (\y -> (x, y)) ys

indexes :: [a] -> [Int]
indexes = range . length

range :: Int -> [Int]
range n = unfoldr f 0
  where f x | x == n = Nothing
        f x          = Just (x, x+1)

visible :: Forest -> Tree -> Bool
visible forest = not . hidden forest

hidden :: Forest -> Tree -> Bool
hidden forest tree =
  let
    myHeight = treeHeight forest tree
    partiallyVisible = all (\h -> h >= myHeight)
  in
    all partiallyVisible $ neighbourHeights forest tree

treeHeight :: Forest -> Tree -> Int
treeHeight forest (Tree x y) = (trows forest !! x) !! y

{- Task 2 -}

-- task2 :: Forest -> Int
task2 forest = "not implemented"

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    testHidden
    validateExample

example = unlines [
  "30373",
  "25512",
  "65332",
  "33549",
  "35390"
  ]

exampleForest = parse example

validateExample :: IO ()
validateExample =
  do
    test "task1 example" 21 (task1 exampleForest)
    -- test "task2 example" 24933642 (task2 exampleFs)

testHidden :: IO ()
testHidden = return ()
  -- do
    -- hid (Tree 0 0 3) False
  -- where hid t e = test ("hidden (" ++ show t ++ ")") e (hidden exampleForest t)

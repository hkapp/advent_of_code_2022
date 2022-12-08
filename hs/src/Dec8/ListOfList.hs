module Dec8.ListOfList (run) where

import Test(test)

import Data.List(transpose, unfoldr)
import Data.Bifunctor(second)

run :: String -> IO ()
run input =
  do
    unitTest
    let forest = parse input
    putStrLn "Task 1:"
    let res1 = task1 forest
    test "task1 realInput" 1533 res1
    print $ res1
    putStrLn "Task 2:"
    let res2 = task2 forest
    test "task2 realInput" 345744 res2
    print $ res2

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
    (left, right) = splitAtAndDrop (rows !! x) y
    (top, down) = splitAtAndDrop (cols !! y) x
  in
    [left, right, top, down]

-- Need to access the right row or column!
splitAtAndDrop :: [a] -> Int -> ([a], [a])
splitAtAndDrop xs idx = second tail $ splitAt idx xs

{- Task 1 -}

task1 :: Forest -> Int
task1 forest = length $ filter (visible forest) $ allTrees forest

allTrees :: Forest -> [Tree]
allTrees forest =
  let
    irow = indexes $ trows forest
    icol = indexes $ tcols forest
  in
    (uncurry Tree) <$> allCombinations irow icol

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
    partiallyHidden = any (\h -> h >= myHeight)
  in
    all partiallyHidden $ neighbourHeights forest tree

treeHeight :: Forest -> Tree -> Int
treeHeight forest (Tree x y) = (trows forest !! x) !! y

{- Task 2 -}

task2 :: Forest -> Int
task2 forest = maximum $ scenicScore forest <$> allTrees forest

scenicScore :: Forest -> Tree -> Int
scenicScore forest start = product $ viewingDistance (treeHeight forest start) <$> observeNeighbours forest start

type Height = Int

viewingDistance :: Height -> [Height] -> Int
viewingDistance _     []                = 0
viewingDistance orig (h:hs) | h >= orig = 1
viewingDistance orig (h:hs)             = 1 + (viewingDistance orig hs)

observeNeighbours :: Forest -> Tree -> [[Height]]
observeNeighbours forest start =
  case neighbourHeights forest start of
    (l:r:u:d:[]) -> [reverse l, r, reverse u, d]

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    testNeighbourHeights
    testVisibleHidden
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
    test "task2 example" 8 (task2 exampleForest)

testNeighbourHeights :: IO ()
testNeighbourHeights =
  do
    test "neighbourHeights (2, 1)" [[6], [3, 3, 2], [0, 5], [3, 5]] (neighbourHeights exampleForest (Tree 2 1))
    test "neighbourHeights (2, 2)" [[6, 5], [3, 2], [3, 5], [5, 3]] (neighbourHeights exampleForest (Tree 2 2))

testVisibleHidden =
  do
    vis 1 1
    vis 1 2
    hid 1 3

    vis 2 1
    hid 2 2
    vis 2 3

    hid 3 1
    vis 3 2
    hid 3 3
  where
    vis = vh "visible" visible
    hid = vh "hidden" hidden

    vh fname f x y = test (fname ++ " " ++ show (x, y)) True (f exampleForest (Tree x y))

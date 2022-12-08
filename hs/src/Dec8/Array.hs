module Dec8.Array (run) where

import Test(test)
import Utils(zipSquareWithIndex)

import Data.List(transpose, unfoldr)
import Data.Bifunctor(second)
import Data.Array.IArray(Array, array, (!), bounds, indices)
import Data.Ix(inRange)

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

type Forest = Array Pos Height

type Pos = (Int, Int)
type Height = Int

parse :: String -> Forest
parse input = intoForest $ parseRow <$> lines input

parseRow :: [Char] -> [Int]
parseRow = map (read . return)

intoForest :: [[Int]] -> Forest
intoForest = arrayFromNestedList

arrayFromNestedList :: [[a]] -> Array (Int, Int) a
arrayFromNestedList xs =
  let
    lowestIdx = (0, 0)

    highestListIdx ys = (length ys) - 1

    highestRowIdx = highestListIdx xs
    highestColIdx = maximum $ highestListIdx <$> xs
    highestIdx = (highestRowIdx, highestColIdx)

    withIndex = map (\(r, c, x) -> ((r, c), x)) $ zipSquareWithIndex xs
  in
    array (lowestIdx, highestIdx) withIndex

{- Forest -}

collectWalk :: (Pos -> Pos) -> Pos -> Forest -> [Height]
collectWalk next start forest = walk (next start)
  where
    walk pos =
      if not $ withinBounds pos
        then []
        else (forest ! pos):(walk $ next pos)

    withinBounds pos = inRange (bounds forest) pos

{-
 0 --y-->
 |
 x
 |
 V
-}

walkLeft :: Pos -> Pos
walkLeft (x, y) = (x, y-1)

walkRight :: Pos -> Pos
walkRight (x, y) = (x, y+1)

walkUp :: Pos -> Pos
walkUp (x, y) = (x-1, y)

walkDown :: Pos -> Pos
walkDown (x, y) = (x+1, y)

neighbourHeights :: Forest -> Pos -> [[Int]]
neighbourHeights forest start =
  map (\w -> collectWalk w start forest) [walkLeft, walkRight, walkUp, walkDown]

-- Need to access the right row or column!
splitAtAndDrop :: [a] -> Int -> ([a], [a])
splitAtAndDrop xs idx = second tail $ splitAt idx xs

{- Task 1 -}

task1 :: Forest -> Int
task1 forest = length $ filter (visible forest) $ allTrees forest

allTrees :: Forest -> [Pos]
allTrees = indices

visible :: Forest -> Pos -> Bool
visible forest = not . hidden forest

hidden :: Forest -> Pos -> Bool
hidden forest tree =
  let
    myHeight = treeHeight forest tree
    partiallyHidden = any (\h -> h >= myHeight)
  in
    all partiallyHidden $ neighbourHeights forest tree

treeHeight :: Forest -> Pos -> Int
treeHeight = (!)

{- Task 2 -}

task2 :: Forest -> Int
task2 forest = maximum $ scenicScore forest <$> allTrees forest

scenicScore :: Forest -> Pos -> Int
scenicScore forest start = product $ viewingDistance (treeHeight forest start) <$> neighbourHeights forest start

viewingDistance :: Height -> [Height] -> Int
viewingDistance _     []                = 0
viewingDistance orig (h:hs) | h >= orig = 1
viewingDistance orig (h:hs)             = 1 + (viewingDistance orig hs)

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
    test "neighbourHeights (2, 1)" [[6], [3, 3, 2], [5, 0], [3, 5]] (neighbourHeights exampleForest (2, 1))
    test "neighbourHeights (2, 2)" [[5, 6], [3, 2], [5, 3], [5, 3]] (neighbourHeights exampleForest (2, 2))

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

    vh fname f x y = test (fname ++ " " ++ show (x, y)) True (f exampleForest (x, y))

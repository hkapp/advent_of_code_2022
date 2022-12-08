module Dec8.FlatList (run) where

import Test(test)
import Utils((<$$>), zipSquareWithIndex)

import Data.List(transpose, unfoldr, sortOn)
import Data.Bifunctor(second)
import Data.Ord(Down(..))

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

{-
  0 --y-->
  |
  x
  |
  V
-}
type Forest = [Tree]
data Tree = Tree {
  posx   :: Int,
  posy   :: Int,
  height :: Int
  }
  deriving Show

parse :: String -> Forest
parse input = intoForest $ parseRow <$> lines input

parseRow :: [Char] -> [Int]
parseRow = map (read . return)

intoForest :: [[Int]] -> Forest
intoForest rows = intoTree <$> zipSquareWithIndex rows

intoTree :: (Int, Int, Int) -> Tree
intoTree (x, y, h) = Tree x y h

{- Forest -}

neighbours :: Forest -> Tree -> [[Tree]]
neighbours forest tree =
  let
    same   f otherTree = f otherTree == f tree
    before f otherTree = f otherTree < f tree
    after  f otherTree = f otherTree > f tree

    isPos fx fy t = fx posx t && fy posy t

    isLeft  = isPos same   before
    isRight = isPos same   after
    isUp    = isPos before same
    isDown  = isPos after  same

    left = filter isLeft forest
  in
    map (\p -> filter p forest) [isLeft, isRight, isUp, isDown]

-- Need to access the right row or column!
splitAtAndDrop :: [a] -> Int -> ([a], [a])
splitAtAndDrop xs idx = second tail $ splitAt idx xs

{- Task 1 -}

task1 :: Forest -> Int
task1 forest = length $ filter (visible forest) forest

visible :: Forest -> Tree -> Bool
visible forest = not . hidden forest

hidden :: Forest -> Tree -> Bool
hidden forest tree =
  let
    higher otherTree = height otherTree >= height tree
    hiddenOneDir = any higher
  in
    all hiddenOneDir $ neighbours forest tree

{- Task 2 -}

task2 :: Forest -> Int
task2 forest = maximum $ scenicScore forest <$> forest

scenicScore :: Forest -> Tree -> Int
scenicScore forest start = product $ viewingDistance start <$> orderedNeighbours forest start

viewingDistance :: Tree -> [Tree] -> Int
viewingDistance _     []     = 0
viewingDistance start (t:ts) | height t >= height start = 1
viewingDistance start (t:ts) = 1 + (viewingDistance start ts)

orderedNeighbours :: Forest -> Tree -> [[Tree]]
orderedNeighbours forest start =
  case neighbours forest start of
    (l:r:u:d:[]) ->
      let
        leftOrdering  = Down . posy
        rightOrdering = posy
        upOrdering    = Down . posx
        downOrdering  = posx
      in
        [
          sortOn leftOrdering l,
          sortOn rightOrdering r,
          sortOn upOrdering u,
          sortOn downOrdering d
        ]

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    testForest
    testHidden
    testOrderedNeighbours
    testViewingDistance
    testScenicScore
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

testHidden :: IO ()
testHidden =
  do
    hid (Tree 0 0 3) False
  where hid t e = test ("hidden (" ++ show t ++ ")") e (hidden exampleForest t)

testOrderedNeighbours :: IO ()
testOrderedNeighbours =
  do
    test "orderedNeighbours (Tree 2 1 5)" [[6], [3, 3, 2], [5, 0], [3, 5]] (height <$$> orderedNeighbours exampleForest (Tree 2 1 5))

testForest :: IO ()
testForest =
  do
    test "forest[0][1]" 0 (height $ head $ filter (\(Tree x y _) -> x == 0 && y == 1) exampleForest)
    test "forest[..][1]" [0, 5, 5, 3, 5] (height <$> filter (\(Tree _ y _) -> y == 1) exampleForest)

testViewingDistance :: IO ()
testViewingDistance =
  do
    test "viewingDistance Up (2, 1)" 1 (viewingDistance start $ (orderedNeighbours exampleForest start) !! 2)
    test "viewingDistance Down (2, 1)" 2 (viewingDistance start $ (orderedNeighbours exampleForest start) !! 3)
    test "viewingDistance Left (2, 1)" 1 (viewingDistance start $ (orderedNeighbours exampleForest start) !! 0)
    test "viewingDistance Right (2, 1)" 3 (viewingDistance start $ (orderedNeighbours exampleForest start) !! 1)
    where start = Tree 2 1 5

testScenicScore :: IO ()
testScenicScore =
  do
    test "scenicScore (1, 2)" 4 (scenicScore exampleForest (Tree 1 2 5))
    test "scenicScore (3, 2)" 8 (scenicScore exampleForest (Tree 3 2 5))

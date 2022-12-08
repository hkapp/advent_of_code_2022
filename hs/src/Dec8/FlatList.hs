module Dec8.FlatList (run) where

import Test(test)

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

zipSquareWithIndex :: [[a]] -> [(Int, Int, a)]
zipSquareWithIndex xs =
  let
    withColIdx = map zipWithIndex xs
    withOuterRowIdx = zipWithIndex withColIdx
  in
    (\(ri, rx) -> map (\(ci, y) -> (ri, ci, y)) rx) =<< withOuterRowIdx

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = z 0
  where z n (x:xs) = (n, x):(z (n+1) xs)
        z _ []     = []

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
task2 forest = maximum $ grade <$> forest
  where grade start = product $ viewingDistance start <$> orderedNeighbours forest start

viewingDistance :: Tree -> [Tree] -> Int
viewingDistance _     [] = 0
viewingDistance start ts = 1 + (length $ takeWhile (shorterThan start) ts)
  where shorterThan t1 t2 = height t2 < height t1

orderedNeighbours :: Forest -> Tree -> [[Tree]]
orderedNeighbours forest start =
  case neighbours forest start of
    (l:r:u:d:[]) ->
      let
        leftOrdering  (Tree x _ _) = Down x
        rightOrdering (Tree x _ _) = x
        upOrdering    (Tree _ y _) = Down y
        downOrdering  (Tree _ y _) = y
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
    test "task2 example" 8 (task2 exampleForest)

testHidden :: IO ()
testHidden =
  do
    hid (Tree 0 0 3) False
  where hid t e = test ("hidden (" ++ show t ++ ")") e (hidden exampleForest t)

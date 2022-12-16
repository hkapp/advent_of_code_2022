module Dec12 (run) where

import Test(test)
import Utils(ascii, arrayFromNestedList, withinBounds, findIndexesWhere, flattenMaybe, repeatUntil)
import Utils.Queue(Queue)
import qualified Utils.Queue as Queue

import Data.Array.IArray(Array, (!))
import qualified Data.Array.IArray as Array
import Data.Maybe(fromJust)
import Data.Foldable(find)
import Data.Bifunctor(second)
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad.State(State, get, put, evalState)
import Data.Functor((<&>))

run :: String -> IO ()
run input =
  do
    unitTest
    let parsed = parse input
    putStrLn "Task 1:"
    print $ task1 parsed
    putStrLn "Task 2:"
    print $ task2 parsed

{- Parsing -}

data Mountain = Mountain {
  topo     :: Topo,
  startPos :: Pos,
  endPos   :: Pos
  }
  deriving Show

type Pos = (Int, Int)
type Topo = Array Pos Int

parse :: String -> Mountain
parse = parseMountain . lines

parseMountain :: [String] -> Mountain
parseMountain lines =
  let
    charArray = arrayFromNestedList lines
    elevations = fmap parseElevation charArray
  in
    Mountain elevations (findStartPos charArray) (findEndPos charArray)

parseElevation :: Char -> Int
parseElevation 'S' = parseElevation 'a'
parseElevation 'E' = parseElevation 'z'
parseElevation c   = (ascii c) - (ascii 'a')

findStartPos :: Array Pos Char -> Pos
findStartPos = findCharPos 'S'

findEndPos :: Array Pos Char -> Pos
findEndPos = findCharPos 'E'

findCharPos :: Char -> Array Pos Char -> Pos
findCharPos c = head . findIndexesWhere ((==) c)

{- Graph -}

newtype Graph n = Graph (n -> [n])

neighbours :: Graph n -> n -> [n]
neighbours (Graph neigh) = neigh

bfs :: (Ord n) => (n -> Bool) -> Graph n -> n -> Maybe [n]
bfs isGoal graph startNode = fmap reverse $ bfsSearch isGoal graph startNode

type BFS n = (Queue [n], Set n)

visit :: (Ord n) => (n -> Bool) -> Graph n -> State (BFS n) (Maybe [n])
visit isGoal graph =
  do
    (queue, visited) <- get
    let (currPath, remainingQueue) = Queue.pop queue
    let currNode = head currPath
    if Set.member currNode visited
      then
        do
          put (remainingQueue, visited)
          return Nothing
      else
        if isGoal currNode
          then return $ Just currPath
          else
            do
              let extendedPaths = neighbours graph currNode <&> (\newNode -> newNode:currPath)
              let newQueue = Queue.pushAll remainingQueue extendedPaths
              let newVisited = Set.insert currNode visited
              put (newQueue, newVisited)
              return Nothing

bfsSearch :: (Ord n) => (n -> Bool) -> Graph n -> n -> Maybe [n]
bfsSearch isGoal graph startNode =
  (flip evalState) initBFS $ repeatUntil stopCondition (visit isGoal graph)
  where
    stopCondition (Just path,  _) = True
    stopCondition (_ , (remQ, _)) = Queue.null remQ

    initBFS = (Queue.singleton [startNode], Set.empty)

{- Task 1 -}

task1 :: Mountain -> Int
task1 mountain = fromJust $ hikeLength mountain (startPos mountain)

hikeLength :: Mountain -> Pos -> Maybe Int
hikeLength mountain hikeStart =
  let
    isGoal pos = pos == endPos mountain
    graph = intoGraph $ topo mountain
    shortestPath = bfs isGoal graph hikeStart
  in
    fmap (length . tail) shortestPath

intoGraph :: Topo -> Graph Pos
intoGraph = Graph . reachableNeighbours

reachableNeighbours :: Topo -> Pos -> [Pos]
reachableNeighbours topo currPos =
  let
    allNeighbours =
      case currPos of
        (x, y) -> [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

    canClimbTo pos = (topo ! pos) <= ((topo ! currPos) + 1)

    validCandidate pos = (withinBounds topo pos) && (canClimbTo pos)
  in
    filter validCandidate allNeighbours

{- Task 2 -}

task2 :: Mountain -> Int
task2 mountain = minimum $ flattenMaybe $ (hikeLength mountain) <$> findIndexesWhere ((==) 0) (topo mountain)

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    validateExample

example = unlines [
  "Sabqponm",
  "abcryxxl",
  "accszExk",
  "acctuvwj",
  "abdefghi"
  ]

exampleMountain = parse example

validateExample :: IO ()
validateExample =
  do
    test "task1 example" 31 (task1 exampleMountain)
    test "task2 example" 29 (task2 exampleMountain)

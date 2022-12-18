module Utils.Bfs(
  Graph(..),
  bfs,
  bfsAllReachable
) where

import Utils.Queue(Queue)
import qualified Utils.Queue as Queue
import Utils.State

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Functor((<&>))

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
  (flip evalState) (initBFS startNode) $ repeatUntil stopCondition (visit isGoal graph)
  where
    stopCondition (Just path,  _) = True
    stopCondition (_ , (remQ, _)) = Queue.null remQ

initBFS :: n -> BFS n
initBFS startNode = (Queue.singleton [startNode], Set.empty)

-- To find all the reachable nodes we need to continuously expand neighbours until there are none left
-- We do this by calling 'visit' with a goal function that always returns false,
-- and retrieve the final 'visited' set as final result
bfsAllReachable :: (Ord n) => Graph n -> n -> Set n
bfsAllReachable graph startNode =
  snd $ (flip execState) (initBFS startNode) $ repeatUntil queueIsEmpty (visit neverGoal graph)
  where
    neverGoal = const False

    queueIsEmpty (_ , (remQ, _)) = Queue.null remQ

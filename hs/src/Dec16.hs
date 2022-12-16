module Dec16 (run) where

import Test(test)
import Utils(maxBy)
import Utils.State
import Utils.PQueue(PQueue)
import qualified Utils.PQueue as PQ

import Data.Bifunctor(second)
import Data.Maybe(fromMaybe)

run :: String -> IO ()
run input =
  do
    unitTest
    let parsed = parse input
    putStrLn "Task 1:"
    testRun "task1" Nothing (task1 parsed)
    putStrLn "Task 2:"
    testRun "task2" Nothing (task2 parsed)
  where
    testRun name expected found =
      do
        case expected of
          Just expres -> test (name ++ " (real input)") expres found
          Nothing     -> return ()
        print found

{- Parsing -}

-- type Res = ([Valve], [Map Room [Room]])

-- parse :: String -> Field
parse = id

{- AStar -}

type AStarState n a = State (AStarSearch n) a
type AStarSearch n = (Maybe (RevPath n), AStarQueue n)
type AStarQueue n = PQueue n (RevPath n)

type RevPath n = [n]

{- Result is in reverse order -}
{- This AStar maximizes -}
astar :: (Ord n) => (n -> [n]) -> (n -> n -> Bool) -> n -> RevPath n
astar expandFrom stopEarly start =
  (flip evalState) initState $ repeatUntil stopCriteria expandOnce
  where
    -- With the following signature, ghc assumes that the 'n' below is
    -- free, and not bound to the enclosing 'n'
    -- See https://stackoverflow.com/questions/5476378/how-to-reuse-a-type-variable-in-an-inner-type-declaration
    -- expandOnce :: AStarState n (RevPath n)
    expandOnce =
      do
        currPath <- popNextNode
        let currNode = head currPath
        bestPath <- getBestPath
        case bestPath of
          Just best | stopEarly (head best) currNode ->
            return best
          _ ->
            {- actually need to expand-}
            case expandFrom currNode of
              [] ->
                {- This is a final node: candidate for best path -}
                considerForBestPath currPath
              ns ->
                do
                  pushNodes (map (\nnew -> (nnew:currPath)) ns)
                  return $ fromMaybe [] bestPath

    -- stopCriteria :: (RevPath n, AStarSearch n) -> Bool
    stopCriteria (_, (_, pq)) = PQ.null pq

    -- initState :: AStarSearch n
    initState = (Nothing, PQ.singleton start [start])

getBestPath :: AStarState n (Maybe (RevPath n))
getBestPath = gets fst

popNextNode :: (Ord n) => AStarState n (RevPath n)
popNextNode = fmap snd $ modPQ PQ.popMax

pushNodes :: (Ord n) => [RevPath n] -> AStarState n ()
pushNodes ns = modPQ f
  where
    f pq =
      let
        nsWithKey = map (\n -> (head n, n)) ns
      in
        ((), PQ.pushAll nsWithKey pq)

modPQ :: (AStarQueue n -> (a, AStarQueue n)) -> AStarState n a
modPQ f =
  do
    (b, pq) <- get
    let (x, pq') = f pq
    put (b, pq')
    return x

considerForBestPath :: (Ord n) => RevPath n -> AStarState n (RevPath n)
considerForBestPath path = modBestPath pickBest
  where
    pickBest Nothing = path
    pickBest (Just (bestSoFar)) = maxBy head bestSoFar path

modBestPath :: (Maybe (RevPath n) -> RevPath n) -> AStarState n (RevPath n)
modBestPath f =
  do
    (b, pq) <- get
    let b' = f b
    put (Just b', pq)
    return b'

-- n :: ([Valve] {-still closed-}, Int {- remMinutes -}, Valve {- open in this state -}, Steam)
{- need to give no neighbours after 30 steps -}

{- Task 1 -}

-- task1 :: Field -> Int
task1 = id

{- Task 2 -}

-- task2 :: Field -> Integer
task2 = id

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    validateExample

example = unlines [
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
  "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
  "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
  "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
  "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
  "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
  "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
  "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
  "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
  "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
  "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
  "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
  "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
  "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
  ]

exampleParsed = parse example

validateExample :: IO ()
validateExample = return ()
  -- do
    -- test "task1 example" [(-2, 24)] (fieldInfluenceOnLine 10 exampleParsed)
    -- test "task1 example" 26 (task1Param 10 exampleParsed)
    -- test "task2 example" 56000011 (task2Param 0 20 exampleParsed)

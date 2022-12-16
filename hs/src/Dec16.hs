module Dec16 (run) where

import Test(test)
import Utils.State

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

{- Graph -}

{- Returns the next possible states given a state path -}
newtype Search s = Search ([s] -> [s])
newtype Graph n = Graph (n -> [n])

{- This A* search maximizes d -}
astar :: (Ord d) => (n -> d) -> Graph n -> n -> [n]

-- type AStarState n d a = State (AStarSearch n d) a
-- type AStarSearch n d = (Maybe (d, RevPath n), PQueue d (RevPath n))

-- {- Result is in reverse order -}
-- {- This AStar maximizes -}
-- astar :: (Ord d) => (n -> [(d, n)]) -> (n -> n -> Bool) -> (d, n) -> RevPath n
-- astar expandFrom stopEarly start =
  -- (flip evalState) (Nothing, PQ.singleton start) $ repeatUntil stopCriteria $ expandOnce
  -- where
    -- expandOnce :: AStarState (RevPath n)
    -- expandOnce =
      -- do
        -- (currScore, currPath) <- popNextNode
        -- let currNode = head currPath
        -- bestPath <- getBestPath
        -- case bestPath of
          -- Just best | stopEarly (head best) currNode ->
            -- return bestPath
          -- _ ->
            -- {- actually need to expand-}
            -- case expandFrom currNode of
              -- [] ->
                -- {- This is a final node: candidate for best path -}
                -- considerForBestPath currScore currPath
              -- ns ->
                -- pushNodes (map $ second (\nnew -> (nnew:currPath)) ns)
                -- return $ maybe [] bestPath

-- getBestPath :: AStarState (Maybe (RevPath n))
-- getBestPath = gets fst

-- popNextNode :: AStarState (d, RevPath n)
-- popNextNode = modPQ PQ.pop

-- pushNodes :: [(d, RevPath n)] -> AStarState ()
-- pushNodes ns = modPQ (\pq -> ((), PQ.pushAll ns pq))

-- modPQ :: (PQueue d n -> (a, PQueue d n)) -> AStarState a
-- modPQ f =
  -- do
    -- (b, pq) <- get
    -- let (x, pq') = f pq
    -- push (b, pq')
    -- return x

-- considerForBestPath :: (Ord d) => d -> RevPath n -> AStarState (RevPath n)
-- considerForBestPath score path = modBestPath pickBest
  -- where
    -- pickBest Nothing = path
    -- pickBest (Just (d, bestSoFar)) = max bestSoFar path

-- modBestPath :: (Maybe (RevPath n) -> RevPath n) -> AStarState (RevPath n)
-- modBestPath f =
  -- do
    -- (b, pq) <- get
    -- let b' = f b
    -- push (b', pq')
    -- return b'

type AStarState n a = State (AStarSearch n) a
type AStarSearch n = (Maybe (RevPath n), AStarQueue n)
type AStarQueue n = PQueue n (RevPath n)

type RevPath n = [n]

{- Result is in reverse order -}
{- This AStar maximizes -}
astar :: (Ord n) => (n -> [n]) -> (n -> n -> Bool) -> n -> RevPath n
astar expandFrom stopEarly start =
  (flip evalState) (Nothing, PQ.singleton start) $ repeatUntil stopCriteria $ expandOnce
  where
    expandOnce :: AStarState (RevPath n)
    expandOnce =
      do
        currPath <- popNextNode
        let currNode = head currPath
        bestPath <- getBestPath
        case bestPath of
          Just best | stopEarly (head best) currNode ->
            return bestPath
          _ ->
            {- actually need to expand-}
            case expandFrom currNode of
              [] ->
                {- This is a final node: candidate for best path -}
                considerForBestPath currPath
              ns ->
                pushNodes (map $ second (\nnew -> (nnew:currPath)) ns)
                return $ maybe [] bestPath

getBestPath :: AStarState (Maybe (RevPath n))
getBestPath = gets fst

popNextNode :: AStarState (RevPath n)
popNextNode = snd . modPQ PQ.popMax

pushNodes :: [RevPath n] -> AStarState ()
pushNodes ns = modPQ (\pq -> ((), PQ.pushAll ns pq))

modPQ :: (AStarQueue n -> (a, AStarQueue n)) -> AStarState a
modPQ f =
  do
    (b, pq) <- get
    let (x, pq') = f pq
    push (b, pq')
    return x

considerForBestPath :: (Ord n) => RevPath n -> AStarState (RevPath n)
considerForBestPath path = modBestPath pickBest
  where
    pickBest Nothing = path
    pickBest (Just (bestSoFar)) = maxBy head bestSoFar path

modBestPath :: (Maybe (RevPath n) -> RevPath n) -> AStarState (RevPath n)
modBestPath f =
  do
    (b, pq) <- get
    let b' = f b
    push (Just b', pq')
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

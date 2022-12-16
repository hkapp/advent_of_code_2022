module Dec16 (run) where

import Test(test)
import Utils(maxBy, parserStrip, splitSubSeq)
import Utils.State
import Utils.PQueue(PQueue)
import qualified Utils.PQueue as PQ

import Data.Bifunctor(first, second)
import Data.Maybe(fromMaybe)
import Data.Map(Map ,(!))
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(sortOn)
import Data.Ord(Down(..))
import Data.Char(isDigit)

run :: String -> IO ()
run input =
  do
    unitTest
    let parsed = parse input
    putStrLn "Task 1:"
    -- testRun "task1" Nothing (task1 parsed)
    print (task1 parsed)
    putStrLn "Task 2:"
    print $ task2 parsed
    -- testRun "task2" Nothing (task2 parsed)
  where
    testRun name expected found =
      do
        case expected of
          Just expres -> test (name ++ " (real input)") expres found
          Nothing     -> return ()
        print found

{- Parsing -}

-- type Res = ([Valve], [Map Room [Room]])

parse :: String -> Volcano
parse = intoVolcano . map parseLine . lines

parseLine :: String -> (Room, Steam, [Room])
parseLine inputLine = (flip evalState) inputLine $
  do
    parserStrip "Valve "
    room <- parseRoom
    parserStrip " has flow rate="
    flow <- parseFlow
    parserStrip "; tunnels lead to valves "
    destinations <- parseRoomList
    return (room, flow, destinations)

parseRoom :: State String Room
parseRoom = state (splitAt 2)

-- TODO move to Utils as parserInt
parseFlow :: State String Steam
parseFlow = state (first read . span isDigit)

parseRoomList :: State String [Room]
parseRoomList = state (\s -> (splitSubSeq ", " s, []))

intoVolcano :: [(Room, Steam, [Room])] -> Volcano
intoVolcano xs =
  let
    tunnels = Map.fromList $ map (\(r, _, rs) -> (r, rs)) xs
    valves  = Map.fromList $ map (\(r, s, _)  -> (r, s))  xs
  in
    Volcano tunnels valves

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

{- Volcano -}

data Volcano = Volcano {
  tunnelNetwork :: Map Room [Room],
  allValves     :: Map Room Steam
  }
  deriving Show

type Room = String

possibleDestinations :: Volcano -> Room -> [Room]
possibleDestinations volcano room = (tunnelNetwork volcano) ! room

valveFlow :: Volcano -> Room -> Steam
valveFlow volcano room = (allValves volcano) ! room

{- Flux -}

{- Actually, partial flux (in construction) -}
data Flux = Flux {
  steamSoFar   :: Steam,
  currPos      :: Room,
  closedValves :: Set Room,
  timeLeft     :: Minutes
  }

type Steam = Int
type Minutes = Int

{- These Eq and Ord instances are really only for AStar
   They compare score equivalence rather than full content
-}
instance Eq Flux where
  fx == fy = (steamSoFar fx) == (steamSoFar fy)

instance Ord Flux where
  compare fx fy = compare (steamSoFar fx) (steamSoFar fy)

-- Flux moves

legalMoves :: Volcano -> Flux -> [Flux]
legalMoves volcano flux =
  if (timeLeft flux) > 0
    then (tunnelMoves volcano flux) ++ (openValveMoves volcano flux)
    else []

tunnelMoves :: Volcano -> Flux -> [Flux]
tunnelMoves volcano flux = moveTo flux <$> possibleDestinations volcano (currPos flux)

moveTo :: Flux -> Room -> Flux
moveTo (Flux steam _ valves time) newPos = Flux steam newPos valves (time - 1)

openValveMoves :: Volcano -> Flux -> [Flux]
openValveMoves volcano flux =
  if canOpenValve flux
    then [openValve volcano flux]
    else []

canOpenValve :: Flux -> Bool
canOpenValve flux = Set.member (currPos flux) (closedValves flux)

openValve :: Volcano -> Flux -> Flux
openValve volcano (Flux prevSteam pos prevValves prevTime) =
  let
    newValves = Set.delete pos prevValves
    newTime = prevTime - 1
    newSteam = prevSteam + (newTime * (valveFlow volcano pos))
  in
    Flux newSteam pos newValves newTime

-- Flux early stop

{- We stop if the theoretical maximum of this flux can't exceed the current best -}
discardFlux :: Volcano -> Flux -> Flux -> Bool
discardFlux volcano currBest candidate = (potential volcano candidate) <= (steamSoFar currBest)

{- To compute the theoretical maximum, we simply assume that we can
   move to the highest flow valve in one move every time
-}
potential :: Volcano -> Flux -> Steam
potential volcano flux =
    max (startByMoving volcano flux) (startByOpening volcano flux)

startByMoving :: Volcano -> Flux -> Steam
startByMoving volcano flux =
  let
    sortedValves = sortOn (\room -> Down $ valveFlow volcano room) (Set.toList $ closedValves flux)

    fluxGenerators :: [(Flux -> Flux)]
    fluxGenerators =
      do
        valve <- sortedValves
        [(flip moveTo) valve, openValve volcano]

    generateFluxes :: [(Flux -> Flux)] -> Flux -> [Flux]
    generateFluxes (gen:rem) fx = (gen fx):(generateFluxes rem (gen fx))
    generateFluxes []        _  = []

    idealFluxes :: [Flux]
    idealFluxes = generateFluxes fluxGenerators flux

    maxFlux = head $ dropWhile (\fx -> (timeLeft fx) /= 0) idealFluxes
    maxSteam = steamSoFar maxFlux
  in
    if (timeLeft flux) > 0
      then maxSteam
      else steamSoFar flux

startByOpening :: Volcano -> Flux -> Steam
startByOpening volcano flux =
  if (canOpenValve flux) && ((timeLeft flux) > 0)
    then startByMoving volcano (openValve volcano flux)
    else steamSoFar flux

{- Task 1 -}

task1' :: Volcano -> Steam
task1' volcano = steamSoFar $ head $ astar (legalMoves volcano) (discardFlux volcano) (initFlux volcano)

initFlux :: Volcano -> Flux
initFlux volcano = Flux 0 "AA" (Map.keysSet $ allValves volcano) 30

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

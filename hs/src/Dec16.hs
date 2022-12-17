module Dec16 (run) where

import Test(test)
import Utils(maxBy, parserStrip, splitSubSeq, parserInt, padRight)
import Utils.State
import Utils.PQueue(PQueue)
import qualified Utils.PQueue as PQ
import Utils.Queue(Queue)
import qualified Utils.Queue as Queue

import Data.Bifunctor(first, second)
import Data.Maybe(fromMaybe)
import Data.Map(Map ,(!))
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(sortOn, find)
import Data.Ord(Down(..))
import Data.Char(isDigit)
import Data.Word(Word32)

import System.IO.Unsafe(unsafePerformIO)
import System.Random(randomIO)

run :: String -> IO ()
run input =
  do
    unitTest
    let parsed = parse input

    putStrLn "Task 1:"
    let (res1, stats1) = task1 parsed
    print res1
    print stats1
    -- Before the refactoring for part 2:
    -- AStarStats {stopEarlyCount = 1899600, fullPathsCount = 99, nodesExpanded = 2860962}
    -- After the change in potential computation:
    -- AStarStats {stopEarlyCount = 7508930, fullPathsCount = 99, nodesExpanded = 11353475}
    test "task1 (real input)" 2077 res1

    putStrLn "Task 2:"
    let (res2, stats2) = task2 parsed
    print res2
    print stats2
    -- testRun "task2" Nothing (task2 parsed)

{- Parsing -}

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

parseFlow :: State String Steam
parseFlow = parserInt

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

data AStarSearch n = AStarSearch {
  astarCurrBest  :: Maybe (RevPath n),
  astarWorkQueue :: AStarQueue n,
  astarStats     :: AStarStats
  }

type AStarQueue n = PQueue n (RevPath n)

type RevPath n = [n]

{- Result is in reverse order -}
{- This AStar maximizes -}
astar :: (Ord n, Show n) => (n -> [n]) -> (n -> n -> Bool) -> n -> (RevPath n, AStarStats)
astar expandFrom stopEarly start =
  second astarStats $ (flip runState) initState $ repeatUntil stopCriteria expandOnce
  where
    -- With the following signature, ghc assumes that the 'n' below is
    -- free, and not bound to the enclosing 'n'
    -- See https://stackoverflow.com/questions/5476378/how-to-reuse-a-type-variable-in-an-inner-type-declaration
    -- expandOnce :: AStarState n (RevPath n)
    expandOnce =
      do
        currPath <- popNextNode
        maybePrintStats
        let currNode = headDbg "97" currPath --(unsafePerformIO $ print currPath >>= (const $ return currPath))
        bestPath <- getBestPath
        case bestPath of
          Just best | stopEarly (headDbg "100" best) currNode ->
            -- (debugPrint "Filtered out a path" return) best
            do
              incStopEarlyCount
              return best
          _ ->
            {- actually need to expand-}
            case expandFrom currNode of
              [] ->
                {- This is a final node: candidate for best path -}
                -- (debugPrint "Reached final path" considerForBestPath) currPath
                do
                  incFullPathsCount
                  considerForBestPath currPath
              ns ->
                do
                  -- (debugPrint ("Expanded " ++ (show $ length ns) ++ " nodes") pushNodes) (map (\nnew -> (nnew:currPath)) ns)
                  incNodesExpandedCount $ length ns
                  pushNodes (map (\nnew -> (nnew:currPath)) ns)
                  return $ fromMaybe [] bestPath

    -- stopCriteria :: (RevPath n, AStarSearch n) -> Bool
    stopCriteria (_, s) = PQ.null $ astarWorkQueue s

    -- initState :: AStarSearch n
    initState = AStarSearch Nothing (PQ.singleton start [start]) initStats

-- TODO remove
debugPrint :: String -> a -> a
debugPrint msg x = unsafePerformIO $ putStrLn msg >>= (const $ return x)

-- TODO remove
maybePrintStats :: AStarState n ()
maybePrintStats =
  do
    stats <- gets astarStats
    unsafePerformIO $
      do
        x <- (randomIO :: IO Word32)
        if (x `mod` 1000000) == 0
          then print stats >>= (const $ return (return ()))
          else return (return ())

getBestPath :: AStarState n (Maybe (RevPath n))
getBestPath = gets astarCurrBest

popNextNode :: (Ord n) => AStarState n (RevPath n)
popNextNode = fmap snd $ modPQ PQ.popMax

pushNodes :: (Ord n) => [RevPath n] -> AStarState n ()
pushNodes ns = modPQ f
  where
    f pq =
      let
        nsWithKey = map (\n -> (headDbg "133" n, n)) ns
      in
        ((), PQ.pushAll nsWithKey pq)

modPQ :: (AStarQueue n -> (a, AStarQueue n)) -> AStarState n a
modPQ f =
  do
    s <- get
    let (x, pq') = f (astarWorkQueue s)
    put $ withAStarQueue (const pq') s
    return x

withAStarQueue :: (AStarQueue n -> AStarQueue n) -> AStarSearch n -> AStarSearch n
withAStarQueue f (AStarSearch currBest pq stats) = AStarSearch currBest (f pq) stats

considerForBestPath :: (Ord n) => RevPath n -> AStarState n (RevPath n)
considerForBestPath path = modBestPath pickBest
  where
    pickBest Nothing = path
    pickBest (Just (bestSoFar)) = maxBy (headDbg "149") bestSoFar path

modBestPath :: (Maybe (RevPath n) -> RevPath n) -> AStarState n (RevPath n)
modBestPath f =
  do
    s <- get
    let b' = f $ astarCurrBest s
    put $ withAStarCurrBest (const $ Just b') s
    return $ b'

withAStarCurrBest :: (Maybe (RevPath n) -> Maybe (RevPath n)) -> AStarSearch n -> AStarSearch n
withAStarCurrBest f (AStarSearch currBest workQueue stats) =
  AStarSearch (f currBest) workQueue stats

initStats :: AStarStats
initStats = AStarStats 0 0 0

-- AStarStats

data AStarStats = AStarStats {
  stopEarlyCount :: Word32,
  fullPathsCount :: Word32,
  nodesExpanded  :: Word32
  }
  deriving Show

incStopEarlyCount :: AStarState n ()
incStopEarlyCount = state (\s -> ((), withAStarStats (\(AStarStats sec fp ne) -> AStarStats (sec+1) fp ne) s))

incFullPathsCount :: AStarState n ()
incFullPathsCount = state (\s -> ((), withAStarStats (\(AStarStats sec fp ne) -> AStarStats sec (fp + 1) ne) s))

incNodesExpandedCount :: Int -> AStarState n ()
incNodesExpandedCount n = state (\s -> ((), withAStarStats (\(AStarStats sec fp ne) -> AStarStats sec fp (ne + (toEnum n))) s))

withAStarStats :: (AStarStats -> AStarStats) -> AStarSearch n -> AStarSearch n
withAStarStats f (AStarSearch b pq stats) = AStarSearch b pq (f stats)

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
  eachPos      :: Serialized Room, -- the current room of each worker
  closedValves :: Set Room,
  timeLeft     :: Minutes
  }
  deriving Show

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
  if hasLegalMoves flux
    then (tunnelMoves volcano flux) ++ (openValveMoves volcano flux)
    else []

hasLegalMoves :: Flux -> Bool
hasLegalMoves flux = ((timeLeft flux) > 0) && (hasClosedValves flux)

noLegalMoves :: Flux -> Bool
noLegalMoves = not . hasLegalMoves

hasClosedValves :: Flux -> Bool
hasClosedValves flux = not $ Set.null $ closedValves flux

tunnelMoves :: Volcano -> Flux -> [Flux]
tunnelMoves volcano flux = moveTo flux <$> possibleDestinations volcano (currWorkerPos flux)

currWorkerPos :: Flux -> Room
currWorkerPos flux = fst $ szPeek $ eachPos flux

moveTo :: Flux -> Room -> Flux
moveTo flux@(Flux steam _ valves time) newWorkerPos =
  let
    (newPos, newTime) = moveSpaceAndTime (const newWorkerPos) flux
  in
    Flux steam newPos valves newTime

openValveMoves :: Volcano -> Flux -> [Flux]
openValveMoves volcano flux =
  if canOpenValve flux
    then [openValve volcano flux]
    else []

canOpenValve :: Flux -> Bool
canOpenValve flux = Set.member (currWorkerPos flux) (closedValves flux)

openValve :: Volcano -> Flux -> Flux
openValve volcano flux@(Flux prevSteam wpos prevValves prevTime) =
  let
    pos = currWorkerPos flux
    newValves = Set.delete pos prevValves
    timeValveOpen = prevTime - 1
    newSteam = prevSteam + (timeValveOpen * (valveFlow volcano pos))
    -- Space doesn't move
    (newPos, newTime) = moveSpaceAndTime id flux
  in
    Flux newSteam newPos newValves newTime

moveSpaceAndTime :: (Room -> Room) -> Flux -> (Serialized Room, Minutes)
moveSpaceAndTime mv flux =
  let
    (roundComplete, newPos) = szMove mv (eachPos flux)
    prevTime = timeLeft flux
    newTime =
      if roundComplete
        then prevTime - 1
        else prevTime
  in
    (newPos, newTime)

-- Flux early stop

{- We stop if the theoretical maximum of this flux can't exceed the current best -}
discardFlux :: Volcano -> Flux -> Flux -> Bool
discardFlux volcano currBest candidate = (potential volcano candidate) <= (steamSoFar currBest)

type Stack a = [a]
data Walk = Move [Room] | Open (Stack Room)

{- To compute the theoretical maximum, we simply assume that we can
   move to the highest flow valve in one move every time
-}
potential :: Volcano -> Flux -> Steam
potential volcano flux =
    -- max (startByMoving volcano flux) (startByOpening volcano flux)
  let
    workerCount = szCount $ eachPos flux


    walk :: [Room] -> Walk -> Flux -> Flux

    walk [] (Open _) flux = flux

    walk _ _ flux | noLegalMoves flux = flux

    walk (r:rs) (Open opened) flux =
      if (length opened) < workerCount
        then walk rs (Open (r:opened)) (openValve volcano flux)
        else walk (r:rs) (Move $ reverse opened) flux

    walk rs (Move []) flux = walk rs (Open []) flux

    walk rs (Move (m:ms)) flux = walk rs (Move ms) (moveTo flux m)


    sortedValves = sortOn (\room -> Down $ valveFlow volcano room) (Set.toList $ closedValves flux)

    startingValves = padRight workerCount someRoom $ take workerCount sortedValves
    someRoom = currWorkerPos flux

    maxFlux = walk sortedValves (Open []) $ teleport startingValves flux
    maxSteam = steamSoFar maxFlux
  in
    maxSteam
    -- if hasLegalMoves flux
      -- then maxSteam
      -- else steamSoFar flux

-- Move each worker to the given room, without updating the time
teleport :: [Room] -> Flux -> Flux

teleport (r:rs) (Flux steam wpos valves time) =
  let
    (_, newPos) = szMove (const r) wpos
  in
    teleport rs (Flux steam newPos valves time)

teleport [] flux = flux

-- More precise approximation, but doesn't generalize easily to more than one worker:
{-
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
    -- TODO update for Serialized

    generateFluxes :: [(Flux -> Flux)] -> Flux -> [Flux]
    generateFluxes (gen:rem) fx = (gen fx):(generateFluxes rem (gen fx))
    generateFluxes []        _  = []

    idealFluxes :: [Flux]
    idealFluxes = generateFluxes fluxGenerators flux

    -- maxFlux = (headDbg ("261: " ++ (show sortedValves) ++ (show $ timeLeft flux))) $ dropWhile (\fx -> (timeLeft fx) /= 0) idealFluxes
    -- If the list is too short, just pick the last one
    -- TODO can't we simply use dropWhile hasLegalMoves?
    --   ~> if we also generate the input flux as part of the generators, we can forego the explicit legalMoves check
    maxFlux = fromMaybe (last idealFluxes) $ find (\fx -> (timeLeft fx) == 0) idealFluxes
    maxSteam = steamSoFar maxFlux
  in
    if hasLegalMoves flux
      then maxSteam
      else steamSoFar flux

startByOpening :: Volcano -> Flux -> Steam
startByOpening volcano flux =
  if (canOpenValve flux) && (hasLegalMoves flux)--((timeLeft flux) > 0)
    then startByMoving volcano (openValve volcano flux)
    else steamSoFar flux
-}

{- Task 1 -}

-- TODO remove
headDbg :: String -> [a] -> a
headDbg msg [] = error msg
headDbg _   xs = head xs

searchVolcano :: (Volcano -> Flux) -> Volcano -> (Steam, AStarStats)
searchVolcano buildFlux volcano =
  first (steamSoFar . headDbg "281") $ astar (legalMoves volcano) (discardFlux volcano) (buildFlux volcano)

task1 :: Volcano -> (Steam, AStarStats)
task1 = searchVolcano initSoloFlux

initFlux :: Int -> Minutes -> Volcano -> Flux
initFlux nWorkers totTime volcano = Flux 0 sz valves totTime
  where
    sz = serialize (take nWorkers $ repeat "AA")
    valves = Set.filter (\valve -> (valveFlow volcano valve) > 0) $ Map.keysSet $ allValves volcano

initSoloFlux :: Volcano -> Flux
initSoloFlux = initFlux 1 30

{- Task 2 -}

task2 :: Volcano -> (Steam, AStarStats)
task2 = searchVolcano initTeamFlux

initTeamFlux :: Volcano -> Flux
initTeamFlux = initFlux 2 26

{- Serialized -}
{- We simulate the "parallel" human and elephant actions by having them
   take turns. We only decrease the timer once both took an action.
   We generalize this to any number of "parallel" processors such that the
   logic also works for part 1, i.e. with a single processor.
   This is a form of serialization (we see parallel actions as though they
   were sequential).
-}

{- The Bool acts as a marker.
   In the Queue, the boolean is always false, except for one
   entry. This signals when we need to decrease the timer.
-}
newtype Serialized a = Serialized (Queue (a, Bool))
  deriving Show

serialize :: [a] -> Serialized a
serialize xs = Serialized $ Queue.fromList (szList xs)
  where
    szList (x:y:zs) = (x, False):(szList (y:zs))
    szList (z:[])   = [(z, True)]

-- Note that this rotates the queue completely
-- The queue never reduces in size
-- Note: for performance, we could actually use a ring buffer here
-- szNext :: Serialized a -> ((a, Bool), Serialized a)
-- szNext (Serialized q) = second Serialized $ Queue.rotate q

szMove :: (a -> a) -> Serialized a -> (Bool, Serialized a)
szMove f (Serialized q) =
  let
    ((x, mrk), poppedQ) = Queue.pop q
    newQ = Queue.push (f x, mrk) poppedQ
  in
    (mrk, Serialized newQ)

szPeek :: Serialized a -> (a, Bool)
szPeek (Serialized q) = Queue.peek q

szCount :: Serialized a -> Int
szCount (Serialized q) = Queue.length q

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    validateExample

example = unlines [
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
  "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
  "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
  "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
  "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
  "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
  "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
  "Valve HH has flow rate=22; tunnels lead to valves GG",
  "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
  "Valve JJ has flow rate=21; tunnels lead to valves II"
  ]

exampleParsed = parse example

validateExample :: IO ()
validateExample =
  do
    test "task1 example" 1651 (fst $ task1 exampleParsed)
    putStrLn "task2 example"
    test "task2 example" 1707 (fst $ task2 exampleParsed)

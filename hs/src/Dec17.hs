module Dec17 (run) where

import Prelude hiding (Left, Right)

import Test(test, testFmtStr)
import Utils.State
import Utils.Ring(Ring)
import qualified Utils.Ring as Ring
import qualified Utils.Bfs as Bfs

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Int(Int64)
import Data.List(genericTake)
import Data.Map(Map)
import qualified Data.Map as Map

import System.IO.Unsafe(unsafePerformIO)

run :: String -> IO ()
run input =
  do
    unitTest
    let parsed = parse input

    putStrLn "Task 1:"
    let res1 = task1 parsed
    print res1
    test "task1 (real input)" 3186 res1

    putStrLn "Task 2:"
    let res2 = task2 parsed
    print res2
    -- test "task2 (real input)" 2741 res2

{- Parsing -}

parse :: String -> Ring Gas
parse = Ring.fromList . map parseDir . filter ((/=) '\n')

parseDir :: Char -> HDir
parseDir '>' = Right
parseDir '<' = Left

{- Coordinate system -}

type Coord = Int64
type Pos = (Coord, Coord)

{- ^            |..@@@@.| 4
   |            |.......| 3
   y            |.......| 2
   |            |.......| 1
   0 --x-->     +-------+ 0
                 0123456

  The floor is at y=0
  Any stable rock must have y>=1
-}

hPos :: Pos -> Coord
hPos = fst

vPos :: Pos -> Coord
vPos = snd

move :: HDir -> Pos -> Pos
move Left  (x, y) = (x-1, y)
move Right (x, y) = (x+1, y)

moveDown :: Pos -> Pos
moveDown (x, y) = (x, y-1)

newPosHV :: Coord -> Coord -> Pos
newPosHV h v = (h, v)

-- Implements a translation that moves the origin (0, 0) to the first Pos argument
moveToPos :: Pos -> Pos -> Pos
moveToPos (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

{- Tetris -}

data Tetris = Tetris {
    rockPositions  :: Set Pos,
    gasRotation    :: Ring Gas,
    shapeGenerator :: Ring Shape
  }
  deriving Show

data Shape = ShapeHBar | ShapePlus | ShapeRevL | ShapeVBar | ShapeBox
  deriving Show

type Piece = [Pos]

type Gas = HDir

data HDir = Left | Right
  deriving Show

-- This is a constant
-- The returned value is still considered within range
leftEdgeOf :: Tetris -> Coord
leftEdgeOf _ = 0

-- This is a constant
-- The returned value is still considered within range
rightEdgeOf :: Tetris -> Coord
rightEdgeOf _ = 6

newTetris :: Ring Gas -> Tetris
newTetris gasRing = Tetris newRocks gasRing shapes
  where
    newRocks = Set.empty
    shapes = Ring.fromList [ShapeHBar, ShapePlus, ShapeRevL, ShapeVBar, ShapeBox]

-- Tetris: gasPush

-- Do nothing if it touches the sides OR another rock
gasPush :: Tetris -> HDir -> Piece -> Piece
gasPush tetris dir origPiece =
  let
    newUnchecked = pushNoCheck dir origPiece
    exceedsBounds = pieceExceedsBounds tetris dir newUnchecked
    blockedByRocks = superposition tetris dir newUnchecked
  in
    if exceedsBounds || blockedByRocks
      then origPiece
      else newUnchecked

superposition :: Tetris -> HDir -> Piece -> Bool
superposition tetris _ piece =
  any (hasRockAt tetris) piece

pushNoCheck :: HDir -> Piece -> Piece
pushNoCheck dir = map (move dir)

pieceExceedsBounds :: Tetris -> HDir -> Piece -> Bool
pieceExceedsBounds tetris dir piece =
  any (pointExceedsBounds tetris dir) piece

pointExceedsBounds :: Tetris -> HDir -> Pos -> Bool
pointExceedsBounds tetris dir pos =
  let
    h = hPos pos
    boundLeft  = leftEdgeOf tetris
    boundRight = rightEdgeOf tetris
    exceeds =
      case dir of
        Left  -> h < boundLeft
        Right -> h > boundRight
  in
    exceeds

pointExceedsAnyBounds :: Tetris -> Pos -> Bool
pointExceedsAnyBounds tetris pos = (pointExceedsBounds tetris Left pos) ||
                                   (pointExceedsBounds tetris Right pos) ||
                                   (vPos pos == 0)

-- Tetris: fall

-- Nothing if it touches the floor
fall :: Tetris -> Piece -> Maybe Piece
fall tetris piece =
  if resting tetris piece
    then Nothing
    else Just $ fallNoCheck piece

resting :: Tetris -> Piece -> Bool
resting tetris piece = any (pointResting tetris) piece

pointResting :: Tetris -> Pos -> Bool
pointResting tetris pos =
  let
    posBelow = moveDown pos
    restingOnFloor = (vPos posBelow) == 0
    restingOnRock  = hasRockAt tetris posBelow
  in
    restingOnFloor || restingOnRock

hasRockAt :: Tetris -> Pos -> Bool
hasRockAt tetris pos = Set.member pos (rockPositions tetris)

fallNoCheck :: Piece -> Piece
fallNoCheck = map moveDown

-- Tetris: starting position

-- Returns the left edge and right edge position that a new piece must have
-- in the given Tetris configuration
startingPos :: Tetris -> Pos
startingPos tetris =
  let
    -- Each rock appears so that its left edge is two units away from the left wall
    leftEdge = 2
    -- [...] and its bottom edge is three units above the highest rock in the room
    -- Note: we actually need an extra 1
    bottomEdge = 1 + 3 + highestRock tetris
  in
    newPosHV leftEdge bottomEdge

-- TODO we'll need to be smarter here for performance
--      we should be able to use Set.to[Asc|Desc]List
--      this requires understanding the ordering of pairs
--      OR we could declare our own
--      and then even use lookupMin/Max
highestRock :: Tetris -> Coord
highestRock tetris =
  case Set.toList (rockPositions tetris) of
    [] -> 0
    xs -> maximum $ map vPos xs

-- Tetris: new piece materialization

newPiece :: Shape -> Pos -> Piece
newPiece shape pos = map (moveToPos pos) $ newPieceAtOrigin shape

-- The bottom-left corner of that shape is at the origin (0, 0)
newPieceAtOrigin :: Shape -> Piece
--          y
--    ####  0
-- x: 0123
newPieceAtOrigin ShapeHBar = [(0, 0), (1, 0), (2, 0), (3, 0)]
--         y
--    .#.  2
--    ###  1
--    .#.  0
-- x: 012
newPieceAtOrigin ShapePlus = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
--         y
--    ..#  2
--    ..#  1
--    ###  0
-- x: 012
newPieceAtOrigin ShapeRevL = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
--         y
--    #  3
--    #  2
--    #  1
--    #  0
-- x: 0
newPieceAtOrigin ShapeVBar = [(0, 0), (0, 1), (0, 2), (0, 3)]
--        y
--    ##  1
--    ##  0
-- x: 01
newPieceAtOrigin ShapeBox = [(0, 0), (1, 0), (0, 1), (1, 1)]

-- Tetris: State

data Outcome = Rest | Falling

data Round = Round {
  tetrisRound :: Tetris,
  pieceRound  :: Piece
  }
  deriving Show

applyGas :: State Round ()
applyGas =
  do
    gas       <- nextGas
    tetris    <- gets tetrisRound
    currPiece <- gets pieceRound
    putPiece $ gasPush tetris gas currPiece

putPiece :: Piece -> State Round ()
putPiece p =
  do
    round <- get
    let newRound = round { pieceRound = p }
    put newRound

nextGas :: State Round Gas
nextGas =
  do
    tetris <- gets tetrisRound
    let (gas, newGasSeq) = Ring.next $ gasRotation tetris
    putGasRotation newGasSeq
    return gas

putGasRotation :: Ring Gas -> State Round ()
putGasRotation newGasRotation =
  do
    prevRound <- get
    let prevTetris = tetrisRound prevRound
    let newTetris = prevTetris { gasRotation = newGasRotation }
    let newRound = prevRound { tetrisRound = newTetris }
    put newRound

applyFall :: State Round Outcome
applyFall =
  do
    tetris    <- gets tetrisRound
    currPiece <- gets pieceRound
    let (outcome, newPiece) = case fall tetris currPiece of
                                Just p  -> (Falling, p)
                                Nothing -> (Rest, currPiece)
    putPiece newPiece
    return outcome

-- Makes the piece fall down and get pushed by gas
roundStep :: State Round Outcome
roundStep =
  do
    applyGas
    applyFall

-- Make a piece fall all the way
-- Don't update the Tetris with the final piece position
-- Note: we assume here that the piece has already been generated
fallAllTheWay :: State Round ()
fallAllTheWay = fmap (const ()) $ repeatUntil stopCond roundStep
  where
    stopCond (Rest, _)    = True
    stopCond (Falling, _) = False

-- Generate a new piece
-- Run the round, i.e. until the piece stops
-- Update the Tetris with the new piece position
oneRound :: State Tetris ()
oneRound =
  do
    -- Actually, there is not reset needed
    -- resetGas
    startingPiece <- generatePiece
    tetris        <- get
    let round = execState fallAllTheWay (Round tetris startingPiece)
    put $ tetrisRound round
    freezePiece $ pieceRound round

-- TODO remove
-- resetGas :: State Tetris ()
-- resetGas =
  -- do
    -- tetris <- get
    -- let newGasRotation = Ring.reset (gasRotation tetris)
    -- let newTetris = tetris { gasRotation = newGasRotation }
    -- put newTetris

generatePiece :: State Tetris Piece
generatePiece =
  do
    tetris  <- get
    let pos =  startingPos tetris
    shape   <- nextShape
    return $ newPiece shape pos

nextShape :: State Tetris Shape
nextShape =
  do
    tetris <- get
    let (shape, newGen) = Ring.next (shapeGenerator tetris)
    put $ tetris { shapeGenerator = newGen }
    return shape

freezePiece :: Piece -> State Tetris ()
freezePiece piece =
  do
    tetris <- get
    let insertPiecePositions prevSet = Set.union prevSet (Set.fromList piece)
    put $ reduceTetris $ withRockPositions insertPiecePositions tetris

withRockPositions :: (Set Pos -> Set Pos) -> Tetris -> Tetris
withRockPositions f tetris = tetris { rockPositions = f (rockPositions tetris) }

{- Task 1 -}

doNRounds :: Int64 -> State Tetris ()
doNRounds n = sequence_ $ genericTake n $ repeat oneRound

stopAfterNRounds :: Int64 -> Ring Gas -> Tetris
stopAfterNRounds nrounds input = execState (doNRounds nrounds) (newTetris input)

task1 :: Ring Gas -> Coord
task1 = task1Param 2022

task1Param :: Int64 -> Ring Gas -> Coord
task1Param n = highestRock . stopAfterNRounds n

{- Task 2 -}

reduceTetris :: Tetris -> Tetris
reduceTetris tetris =
  let
    {- Threshold experiment:

        threshold |    time |  iterations
        ----------+---------+-------------
               10 |  7.127s | 100'000
              100 |  3.365s | 100'000
             1000 |  3.117s | 100'000
            10000 | 12.597s | 100'000
        ----------+---------+-------------
              100 | 21.580s | 1'000'000
              150 | 14.751s | 1'000'000
              200 | 13.538s | 1'000'000
              250 | 13.910s | 1'000'000
              300 | 14.181s | 1'000'000
              400 | 14.887s | 1'000'000
              500 | 16.104s | 1'000'000
              750 | 18.099s | 1'000'000
             1000 | 20.427s | 1'000'000
             5000 | 61.855s | 1'000'000
            10000 | 155.95s | 1'000'000
    -}
    threshold = 200
    currRocks = rockPositions tetris

    validCandidate cand = (not $ pointExceedsAnyBounds tetris cand) && (not $ hasRockAt tetris cand)

    localNeighbours pos = filter validCandidate [move Left pos, move Right pos, moveDown pos]

    reachableEmptySpots = Bfs.bfsAllReachable (Bfs.Graph localNeighbours) (startingPos tetris)

    -- now keep all rocks that are above the lowest free reachable spot - 1
    lowestFreeSpot = minimum $ map vPos $ Set.toList reachableEmptySpots
    tideMark = lowestFreeSpot - 1

    reducedRocks_ = Set.filter (\r -> vPos r >= tideMark) currRocks
    reducedRocks =
      if (maximum $ map vPos $ Set.toList reducedRocks_) <= (tideMark + 1)
        then awfulPrint (Set.map (moveToPos (newPosHV 0 (-tideMark))) reducedRocks_) reducedRocks_
        else reducedRocks_
  in
    if length currRocks > threshold
      then tetris { rockPositions = reducedRocks }
      else tetris

awfulPrint :: (Show a) => a -> b -> b
awfulPrint x y = unsafePerformIO (print x >>= (const (return y)))

-- Number of dirs in the input gas ring: 10091
--   this is prime
task2 :: Ring Gas -> Coord
task2 = task1Param 1000000

data Archive = Archive {
  archiveEnabled    :: Bool,
  archiveRecords    :: Map Key Record,
  archiveCurrTetris :: Tetris,
  archiveTime       :: Int64
  }

{- We loop if the last couple of rows are the same AND the gas ring state is the same -}
type ArmPos = Int
type Key = (Set Pos, ArmPos)

data Record = Record {
  recordTetris :: Tetris,
  -- recordHeight :: Coord,
  recordTime   :: Time
  }

type Time = Int64

findRecurrenceTime :: Ring Gas -> Recurrence
findRecurrenceTime gasRotation = evalState (repeatUntilIsJust tryOneRecurrence) (initArchive gasRotation)

initArchive :: Ring Gas -> Archive
initArchive gas = Archive False Map.empty (newTetris gas) 0

tryOneRecurrence :: State Archive (Maybe Recurrence)
tryOneRecurrence =
  do
    tetris0 <- gets archiveCurrTetris
    let tetris1 = execState oneRound tetris0
    putArchiveCurrTetris tetris1
    enabled <- tryEnableArchive
    if enabled && recurrenceCandidate tetris1
      then checkRecurrence
      else return Nothing
    -- TODO update time

putArchiveCurrTetris :: Tetris -> State Archive ()
putArchiveCurrTetris tetris =
  do
    archive <- get
    put $ archive { archiveCurrTetris = tetris }

recurrenceCandidate :: Tetris -> Bool
recurrenceCandidate tetris =
  let
    sizeThreshold = 14 -- equivalent to two full rows
    size = length $ rockPositions tetris
  in
    size <= sizeThreshold

-- TODO remove the current height when inserting into the map
tryEnableArchive :: State Archive Bool
tryEnableArchive =
  do
    alreadyEnabled <- gets archiveEnabled
    if alreadyEnabled
      then return True
      else
        do
          tetris <- gets archiveCurrTetris
          let nowEnabled = recurrenceCandidate tetris
          if nowEnabled
            then
              do
                archive <- get
                put $ archive { archiveEnabled = True }
                return True
            else return False

data Recurrence = Recurrence {
  sdflk :: ()
  }

-- Here we assume that we are enabled and that the current tetris is a valid candidate
checkRecurrence :: State Archive (Maybe Recurrence)
checkRecurrence =
  do
    currKey <- buildCurrKey
    records <- gets archiveRecords
    if Map.member currKey records
      then fmap Just buildRecurrence
      else
        do
          updateFailedRecurrence
          return Nothing

buildRecurrence :: State Archive Recurrence
buildRecurrence = return $ Recurrence ()

buildCurrKey :: State Archive Key
buildCurrKey =
  do
    tetris <- gets archiveCurrTetris
    let rocks = heightInsensitive $ rockPositions tetris
    let armPos = Ring.armPos $ gasRotation tetris
    return (rocks, armPos)

heightInsensitive :: Set Pos -> Set Pos
heightInsensitive heightRocks =
  let
    newFloor = minimum $ map vPos $ Set.toList heightRocks
    removeHeight = moveToPos (newPosHV 0 (-newFloor))
  in
    Set.map removeHeight heightRocks

updateFailedRecurrence :: State Archive ()
updateFailedRecurrence =
  do
    newRecord <- buildRecord
    key       <- buildCurrKey
    archive   <- get
    put $ archive { archiveRecords = Map.insert key newRecord (archiveRecords archive) }

buildRecord :: State Archive Record
buildRecord =
  do
    currTetris <- gets archiveCurrTetris
    currTime <- gets archiveTime
    return $ Record currTetris currTime

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    debug1
    debug2
    debug3
    debug4
    validateExample

example = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

exampleParsed = parse example

validateExample :: IO ()
validateExample =
  do
    test "task1 example" 3068 (task1 exampleParsed)
    test "task2 example" 1514285714288 (task2 exampleParsed)

showTetris :: Tetris -> String
showTetris tetris =
  let
    rowIndexes = reverse $ [1..(highestRock tetris)]
    colIndexes = [(leftEdgeOf tetris)..(rightEdgeOf tetris)]
    rocks = rockPositions tetris

    showPos pos =
      case Set.member pos rocks of
        True  -> '#'
        False -> '.'

    showTetrisRow ridx = map (\cidx -> showPos (newPosHV cidx ridx)) colIndexes

    strRows = map showTetrisRow rowIndexes
  in
    unlines strRows

debug1 :: IO ()
debug1 =
  do
    let pc0 = [(2, 4), (3, 4), (4, 4), (5, 4)]
    test "example: init piece" pc0 (newPiece ShapeHBar (2, 4))
    let tetris0 = newTetris exampleParsed
    test "starting pos" (2, 4) (startingPos tetris0)
    testFmtStr "task1 example (1 rounds)" "..####.\n" (showTetris $ stopAfterNRounds 1 exampleParsed)
    test "task1 example (1 rounds)" 1 (task1Param 1 exampleParsed)

debug2 :: IO ()
debug2 =
  do
    rnd 2 4
    rnd 3 6
    rnd 4 7
    rnd 5 9

    rsn 5 $ unlines [
      "....##.",
      "....##.",
      "....#..",
      "..#.#..",
      "..#.#..",
      "#####..",
      "..###..",
      "...#...",
      "..####."
      ]
  where
    rnd n expected = test (msg n) expected (task1Param n exampleParsed)

    rsn n expected = testFmtStr (msg n) expected (showTetris $ stopAfterNRounds n exampleParsed)

    msg n = "task1 example (" ++ (show n) ++ " rounds)"

debug3 :: IO ()
debug3 =
  do
    rnd 6 10
    rnd 7 13
    rnd 8 15
    rnd 9 17
    rnd 10 17

    rsn 10 $ unlines [
      "....#..",
      "....#..",
      "....##.",
      "##..##.",
      "######.",
      ".###...",
      "..#....",
      ".####..",
      "....##.",
      "....##.",
      "....#..",
      "..#.#..",
      "..#.#..",
      "#####..",
      "..###..",
      "...#...",
      "..####."
      ]
  where
    rnd n expected = test (msg n) expected (task1Param n exampleParsed)

    rsn n expected = testFmtStr (msg n) expected (showTetris $ stopAfterNRounds n exampleParsed)

    msg n = "task1 example (" ++ (show n) ++ " rounds)"

debug4 :: IO ()
debug4 =
  do
    rsn 12 $ unlines $ [
      "...#...",
      "..###..",
      "...#...",
      "...####",
      "....#..",
      "....#..",
      "....##.",
      "##..##.",
      "######."
      ] ++ (take 12 $ repeat ".......")
  where
    rsn n expected = testFmtStr (msg n) expected (showTetris $ stopAfterNRounds n exampleParsed)
    msg n = "task1 example (" ++ (show n) ++ " rounds)"

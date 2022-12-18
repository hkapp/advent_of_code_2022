module Dec17 (run) where

import Prelude hiding (Left, Right)

import Test(test)
import Utils.State
import Utils.Ring(Ring)
import qualified Utils.Ring as Ring

import Data.Set(Set)
import qualified Data.Set as Set

run :: String -> IO ()
run input =
  do
    unitTest
    let parsed = parse input

    putStrLn "Task 1:"
    let res1 = task1 parsed
    print res1
    -- test "task1 (real input)" 2077 res1

    putStrLn "Task 2:"
    let res2 = task2 parsed
    print res2
    -- test "task2 (real input)" 2741 res2

{- Parsing -}

parse = id

{- Coordinate system -}

type Coord = Int
type Pos = (Coord, Coord)

{- ^
   |
   y
   |
   0 --x-->
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

-- Tetris: gasPush

-- Do nothing if it touches the sides
gasPush :: Tetris -> HDir -> Piece -> Piece
gasPush tetris dir origPiece =
  let
    newUnchecked = pushNoCheck dir origPiece
    exceedsBounds = pieceExceedsBounds tetris dir newUnchecked
  in
    if exceedsBounds
      then origPiece
      else newUnchecked

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

-- Tetris: fall

-- Nothing if it touches the floor
fall :: Tetris -> Piece -> Maybe Piece
fall tetris piece =
  if resting tetris piece
    then Nothing
    else Just $ fallNoCheck piece

-- TODO cover the case where there is no floor
resting :: Tetris -> Piece -> Bool
resting tetris piece = any (pointResting tetris) piece

pointResting :: Tetris -> Pos -> Bool
pointResting tetris pos =
  let
    restingOnFloor = (vPos pos) == 0
    restingOnRock  = hasRockAt tetris (moveDown pos)
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
    bottomEdge = 3 + highestRock tetris
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
-- TODO reset the gas
oneRound :: State Tetris ()
oneRound =
  do
    resetGas
    startingPiece <- generatePiece
    tetris        <- get
    let round = execState fallAllTheWay (Round tetris startingPiece)
    put $ tetrisRound round
    freezePiece $ pieceRound round

resetGas :: State Tetris ()
resetGas =
  do
    tetris <- get
    let newGasRotation = Ring.reset (gasRotation tetris)
    let newTetris = tetris { gasRotation = newGasRotation }
    put newTetris

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
    put $ withRockPositions insertPiecePositions tetris

withRockPositions :: (Set Pos -> Set Pos) -> Tetris -> Tetris
withRockPositions f tetris = tetris { rockPositions = f (rockPositions tetris) }

{- Task 1 -}

task1 = id

{- Task 2 -}

task2 = id

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
validateExample = return ()
  -- do
    -- test "task1 example" 1651 (fst $ task1 exampleParsed)
    -- putStrLn "task2 example"
    -- test "task2 example" 1707 (fst $ task2 exampleParsed)

module Dec17 (run) where

import Prelude hiding (Left, Right)

import Test(test)
import Utils.State
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

{- Tetris -}

type Tetris = ()
-- type Shape
type Piece = ()
type Gas = HDir
data HDir = Left | Right

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

resting :: Tetris -> Piece -> Bool
resting tetris piece = any (pointResting tetris) piece

pointResting :: Tetris -> Pos -> Bool
pointResting tetris pos = hasRockAt tetris (moveDown pos)

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
    rightEdge = 3 + highestRock tetris
  in
    newPosHV leftEdge bottomEdge

-- TODO we'll need to be smarter here for performance
--      we should be able to use Set.to[Asc|Desc]List
--      this requires understanding the ordering of pairs
--      OR we could declare our own
--      and then even use lookupMin/Max
highestRock :: Tetris -> Coord
highestRock tetris =
  case Set.toList (stableRocks tetris) of
    [] -> 0
    xs -> maximum $ map vPos xs

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
    tetris    <- gets tetrisRound
    currPiece <- gets pieceRound
    gas       <- nextGas tetrisRound
    putPiece $ gasPush tetris gas currPiece

nextGas :: State Round Gas
nextGas =
  do
    tetris <- gets tetrisRound
    (gas, newGasSeq) <- Ring.next $ gasRotation tetris
    putGasRotation newGasSeq
    return gas

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
    let round = execState fallAllTheWay (tetris, startingPiece)
    put $ tetrisRound round
    freezePiece $ pieceRound round

generatePiece :: State Tetris Piece
generatePiece =
  do
    tetris  <- get
    let pos =  startingPos tetris
    shape   <- nextShape
    return $ newPiece shape pos

freezePiece :: Piece -> State Tetris ()
freezePiece piece =
  do
    tetris <- get
    let insertPiecePositions prevSet = Set.union prevSet (Set.fromList piece)
    put $ withRockPositions insertPiecePositions tetris

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

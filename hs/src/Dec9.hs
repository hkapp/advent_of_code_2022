module Dec9 (run) where

import Prelude hiding (Left, Right)

import Utils(splitFirstSep)
import Test(test)

import Data.List(foldl')
import Data.Bifunctor(bimap)
import qualified Data.Set as Set
import Control.Monad.State(State, state, evalState)

run :: String -> IO ()
run input =
  do
    unitTest
    let moves = parse input
    putStrLn "Task 1:"
    print $ task1 moves
    putStrLn "Task 2:"
    print $ task2 moves

{- Parsing -}

parse :: String -> [Move]
parse input = parseMove <$> lines input

data Move = Move Dir Int
  deriving Show

data Dir  = Up | Down | Left | Right
  deriving Show

parseMove :: String -> Move
parseMove = intoMove . bimap (parseDir . head) read . splitFirstSep ' '

parseDir :: Char -> Dir
parseDir 'D' = Down
parseDir 'U' = Up
parseDir 'L' = Left
parseDir 'R' = Right

intoMove :: (Dir, Int) -> Move
intoMove = uncurry Move

{- Task 1 -}

task1 :: [Move] -> Int
task1 = countDistinctTailPos

class Physics where
  next :: Self -> Dir -> Self
  origin :: Self
  tailPos :: Self -> Pos

coundDistinctTailPos :: (Physics a) -> a -> [Moves] -> Int
  let
    knotSeq    = applyAll moves startKnot
    tailPosSeq = ktail <$> knotSeq
  in
    length $ Set.fromList tailPosSeq

data Knot = Knot Pos Pos
  deriving (Eq, Show)

type Pos = (Int, Int)

khead :: Knot -> Pos
khead (Knot h _) = h

ktail :: Knot -> Pos
ktail (Knot _ t) = t

origin :: Pos
origin = (0, 0)

startKnot :: Knot
startKnot = Knot origin origin

applyAll :: [Move] -> Knot -> [Knot]
-- applyAll moves start = start : (evalState (simulate stepKnot moves) start)
applyAll moves start = runSimulation moveKnot start moves

runSimulation :: (a -> Dir -> a) -> a -> [Move] -> [a]
runSimulation next start moves = start : (evalState (simulate (step next) moves) start)

simulate :: (Dir -> State a a) -> [Move] -> State a [a]
simulate advance moves = traverse advance (moves >>= explicitMove)

step :: (a -> Dir -> a) -> Dir -> State a a
step next dir = state (\x -> dup (next x dir))

-- stepKnot :: Dir -> State Knot Knot
-- stepKnot = step moveKnot

dup :: a -> (a, a)
dup x = (x, x)

explicitMove :: Move -> [Dir]
explicitMove (Move d n) = take n $ repeat d

moveKnot :: Knot -> Dir -> Knot
moveKnot knot = follow (ktail knot) . move (khead knot)

{-
^
|
y
|
0--x-->
-}
move :: Pos -> Dir -> Pos
move (x, y) Right = (x+1, y)
move (x, y) Left  = (x-1, y)
move (x, y) Up    = (x, y+1)
move (x, y) Down  = (x, y-1)

follow :: Pos -> Pos -> Knot
follow oldTail newHead =
  let
    (xdif, ydif) = mdist newHead oldTail

    newTail =
      if (abs xdif) > 1 || (abs ydif) > 1
        then foldl' move oldTail $ decideDir (xdif, ydif)
        else oldTail
  in
    Knot newHead newTail

-- Manhattan distance
mdist :: Num n => (n, n) -> (n, n) -> (n, n)
mdist (x, y) (x', y') = (x - x', y - y')

decideDir :: (Int, Int) -> [Dir]

decideDir (xdif, 0) | xdif /= 0 =
  if xdif > 0
    then [Right]
    else [Left]

decideDir (0, ydif) | ydif /= 0 =
  if ydif > 0
    then [Up]
    else [Down]

decideDir (0, 0) = []

decideDir (xdif, ydif) = decideDir (xdif, 0) ++ decideDir (0, ydif)

{- Task 2 -}

-- task2 :: [Move] -> Int
task2 _ = "not implemented"

type Rope = [Pos]

startRope :: Int -> Rope
startRope n = take n $ repeat origin

moveRope :: Rope -> Dir -> Rope
moveRope (p:ps) dir = followAll $ (move dir p):ps

followAll :: Rope -> Rope
followAll (p1:p2:ps) = p1:(follow p2 p1):(followAll ps)
followAll ps         = ps

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    testMdist
    testMoveKnot
    testApplyAll
    validateExample

example = unlines [
  "R 4",
  "U 4",
  "L 3",
  "D 1",
  "R 4",
  "D 1",
  "L 5",
  "R 2"
  ]

exampleMoves = parse example

validateExample :: IO ()
validateExample =
  do
    test "task1 example" 13 (task1 exampleMoves)
    -- test "task2 example" 24933642 (task2 exampleMoves)

exampleStateTransition = [
  startKnot,
  -- R 4 --
  Knot (1, 0) (0, 0),
  Knot (2, 0) (1, 0),
  Knot (3, 0) (2, 0),
  Knot (4, 0) (3, 0),
  -- U 4 --
  Knot (4, 1) (3, 0),
  Knot (4, 2) (4, 1),
  Knot (4, 3) (4, 2),
  Knot (4, 4) (4, 3),
  -- L 3 --
  Knot (3, 4) (4, 3),
  Knot (2, 4) (3, 4),
  Knot (1, 4) (2, 4)
  ]

testApplyAll =
  do
    test ("applyAll (" ++ show (length exampleStateTransition) ++ ")") exampleStateTransition (take (length exampleStateTransition) $ applyAll exampleMoves startKnot)

testMdist =
  do
    test "mdist (4, 2) (3, 0)" (1, 2) (mdist (4, 2) (3, 0))
    test "mdist (2, 4) (4, 3)" (-2, 1) (mdist (2, 4) (4, 3))

testMoveKnot =
  do
    let start = Knot (3, 4) (4, 3)
    let end = Knot (2, 4) (3, 4)
    let dir = Left
    test "moveKnot Left (3, 4) (4, 3)" end (moveKnot start dir)
    test "follow (2, 4) (4, 3)" end (follow (ktail start) (khead end))

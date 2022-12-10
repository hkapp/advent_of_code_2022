module Dec9 (run) where

import Prelude hiding (Left, Right)

import Utils(splitFirstSep, evalStarting)
import Test(test)

import Data.List(foldl')
import Data.Bifunctor(bimap)
import qualified Data.Set as Set
import Control.Monad.State(State, state)

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
task1 = countDistinctTailPos startKnot

class Physics a where
  motion  :: a -> Dir -> a
  tailPos :: a -> Pos

countDistinctTailPos :: (Physics a) => a -> [Move] -> Int
countDistinctTailPos start moves =
  let
    knotSeq    = applyAll moves start
    tailPosSeq = tailPos <$> knotSeq
  in
    length $ Set.fromList tailPosSeq

data Knot = Knot Pos Pos
  deriving (Eq, Show)

type Pos = (Int, Int)

instance Physics Knot where
  motion  = moveKnot
  tailPos = ktail

khead :: Knot -> Pos
khead (Knot h _) = h

ktail :: Knot -> Pos
ktail (Knot _ t) = t

origin :: Pos
origin = (0, 0)

startKnot :: Knot
startKnot = Knot origin origin

applyAll :: (Physics a) => [Move] -> a -> [a]
applyAll moves start = evalStarting (simulate (step motion) moves) start

simulate :: (Dir -> State a a) -> [Move] -> State a [a]
simulate advance moves = traverse advance (moves >>= explicitMove)

step :: (a -> Dir -> a) -> Dir -> State a a
step next dir = state (\x -> dup (next x dir))

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

decideDir (xdif, 0) | xdif > 0 = [Right]
decideDir (xdif, 0) | xdif < 0 = [Left]

decideDir (0, ydif) | ydif > 0 = [Up]
decideDir (0, ydif) | ydif < 0 = [Down]

decideDir (0, 0) = []

decideDir (xdif, ydif) = decideDir (xdif, 0) ++ decideDir (0, ydif)

{- Task 2 -}

task2 :: [Move] -> Int
task2 = countDistinctTailPos (WRope startRope)

type Rope = [Pos]
newtype WRope = WRope Rope

instance Physics WRope where
  motion (WRope rope)  = WRope . moveRope rope
  tailPos (WRope rope) = last rope

startRope :: Rope
startRope = take 10 $ repeat origin

moveRope :: Rope -> Dir -> Rope
moveRope (p:ps) dir = followAll $ (move p dir):ps

followAll :: Rope -> Rope
followAll (p1:p2:ps) =
  let
    p2' = ktail $ follow p2 p1
  in
    p1:(followAll $ p2':ps)
followAll ps         = ps

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    testMdist
    testMoveKnot
    testMoveRope
    testApplyAll
    validateExamples

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

example2 = unlines [
  "R 5",
  "U 8",
  "L 8",
  "D 3",
  "R 17",
  "D 10",
  "L 25",
  "U 20"
  ]

exampleMoves2 = parse example2

validateExamples :: IO ()
validateExamples =
  do
    test "task1 example" 13 (task1 exampleMoves)
    test "task2 example" 1 (task2 exampleMoves)
    test "task2 example2" 36 (task2 exampleMoves2)

exampleStateTransitionTask1 = [
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

exampleStateTransitionTask2 = [
  startRope,
  -- R 4 --
  startWith [(1, 0)],
  startWith [(2, 0), (1, 0)],
  startWith [(3, 0), (2, 0), (1, 0)]
  -- startWith [(4, 0), (3, 0), (2, 0), (1, 0)]
  ]
  where
    startWith xs = xs ++ drop (length xs) startRope

testApplyAll =
  do
    taa startKnot exampleStateTransitionTask1 id
    taa (WRope startRope) exampleStateTransitionTask2 (\(WRope r) -> r)
  where
    taa start expected feq =
      let
        n = length expected
      in
        test  ("applyAll (" ++ show n ++ ")") expected (feq <$> (take n $ applyAll exampleMoves start))

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

testMoveRope =
  do
    test "followAll [(3, 0), (1, 0), (0, 0)]" [(3, 0), (2, 0), (1, 0)] (followAll [(3, 0), (1, 0), (0, 0)])

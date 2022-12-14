module Dec14 (run) where

import Test(test, testFmtStr)
import Utils(splitFirstSep, splitSubSeq, both, (<$$>))

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Ix as Ix

run :: String -> IO ()
run input =
  do
    unitTest
    let parsed = parse input
    putStrLn "Task 1:"
    -- print $ task1 parsed
    putStrLn $ task1 parsed
    putStrLn "Task 2:"
    print $ task2 parsed

{- Parsing -}

{-                 y0=0
  x0=500  <--x-- (x0, y0) --x-->
                    |
                    y
                    |
                    V
-}
data Cave = Cave {
  caveState  :: Map Pos Mat,
  leftEdge   :: Int,
  rightEdge  :: Int,
  bottomEdge :: Int
  }

topEdge :: Cave -> Int
topEdge = const 0

data Mat = Rock | Air

parse :: String -> Cave
parse input = intoCave $ (lines input) >>= (explicitRockLine . parseRockLine)
  -- do
    -- line <- lines input
    -- rockPos <- explicitRockLine $ parseRockLine line
    -- return (rockPos, Rock)

type RockLine = [Pos]

parseRockLine :: String -> RockLine
parseRockLine = map parsePos . splitSubSeq " -> "

parsePos :: String -> Pos
parsePos = both read . splitFirstSep ','

{- This function generates duplicates but we don't really care -}
explicitRockLine :: RockLine -> [Pos]
explicitRockLine (x:y:zs) = (Ix.range (x, y)) ++ (Ix.range (y, x)) ++ (explicitRockLine (y:zs))
explicitRockLine _        = []

intoCave :: [Pos] -> Cave
intoCave rockPositions =
  let
    rockMap = Map.fromList $ map (\p -> (p, Rock)) rockPositions

    xCoords = xPos <$> rockPositions
    leftEdge = minimum xCoords
    rightEdge = maximum xCoords

    yCoords = yPos <$> rockPositions
    bottomEdge = maximum yCoords
  in
    Cave rockMap leftEdge rightEdge bottomEdge

{- Coordinate system -}

{-                 y0=0
  x0=500  <--x-- (x0, y0) --x-->
                    |
                    y
                    |
                    V
-}

{-         (  x,   y) -}
type Pos = (Int, Int)

xPos :: Pos -> Int
xPos = fst

yPos :: Pos -> Int
yPos = snd

below :: Pos -> Pos
below (x, y) = (x, y+1)

leftOf :: Pos -> Pos
leftOf (x, y) = (x-1, y)

rightOf :: Pos -> Pos
rightOf (x, y) = (x+1, y)

-- bottom-left diagonal
diagLeft :: Pos -> Pos
diagLeft = leftOf . below

-- bottom-right diagonal
diagRight :: Pos -> Pos
diagRight = rightOf . below


{- Cave -}

topLeft :: Cave -> Pos
topLeft cave = (leftEdge cave, topEdge cave)

bottomRight :: Cave -> Pos
bottomRight cave = (rightEdge cave, bottomEdge cave)

showCave :: Cave -> String
showCave cave =
  let
    caveLineY y = map (\x -> (x, y)) $ Ix.range (leftEdge cave, rightEdge cave)
    caveLinesPos = map caveLineY $ Ix.range (topEdge cave, bottomEdge cave)
    caveLines = (matAt cave) <$$> caveLinesPos
  in
    unlines $ matChar <$$> caveLines

matAt :: Cave -> Pos -> Mat
matAt cave pos = Map.findWithDefault Air pos (caveState cave)

matChar :: Mat -> Char
matChar Rock = '#'
matChar Air  = '.'

{- Sand -}

data Outcome = Stable | Leaks
type PartialOutcome = Maybe Outcome

{- A grain of sand -}
type Grain = Pos

leaks :: Cave -> Grain -> Bool
leaks cave grain =
  let
    leaksLeft = (xPos grain) < (leftEdge cave)
    leaksRight = (xPos grain) > (rightEdge cave)
    leaksBottom = (yPos grain) > (bottomEdge cave)
  in
    leaksLeft || leaksRight || leaksBottom

fall :: Cave -> Grain -> Maybe Grain
fall cave grain =
  join $ find isJust $ map (\mv -> tryPos $ mv cave grain) candidateDirections
  where
    tryPos pos = toMaybe (free pos) pos
    candidateDirections =  [below, diagLeft, diagRight]
    {- the order in this list is important! -}

grainPhysics :: Cave -> State Grain PartialOutcome
grainPhysics cave =
  do
    grain <- get
    if leaks cave grain
      then return $ Just Leaks
      else
        case fall cave grain of
          Just newPos ->
            do
              put newPos
              return Nothing
          Nothing     ->
            return $ Just Stable

newGrain :: State Cave Outcome
newGrain = repeatUntil (second . isJust) grainPhysics

{- Task 1 -}

-- task1 :: Troop -> Hold -> Int
task1 = showCave

{- Task 2 -}

-- task2 :: Troop -> Hold -> Int
task2 _ = "not implemented"

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    debug1
    validateExample

example = unlines [
  "498,4 -> 498,6 -> 496,6",
  "503,4 -> 502,4 -> 502,9 -> 494,9"
  ]

exampleCave = parse example

exampleDisp = unlines [
  "..........",
  "..........",
  "..........",
  "..........",
  "....#...##",
  "....#...#.",
  "..###...#.",
  "........#.",
  "........#.",
  "#########."
  ]

validateExample :: IO ()
validateExample =
  do
    testFmtStr "display example" exampleDisp (showCave exampleCave)

debug1 :: IO ()
debug1 =
  do
    test "leftEdge example" 494 (leftEdge exampleCave)
    test "rightEdge example" 503 (rightEdge exampleCave)
    test "topEdge example" 0 (topEdge exampleCave)
    test "bottomEdge example" 9 (bottomEdge exampleCave)

    erl [(1, 0), (1, 2)] [(1, 0), (1, 1), (1, 2)]
    test "Ix.range (1, 0) (1, 2)" [(1, 0), (1, 1), (1, 2)] (Ix.range ((1, 0), (1, 2)))
    erl [(1, 2), (1, 0)] [(1, 0), (1, 1), (1, 2)]
  where
    erl implicit explicit = test ("explicitRockLine " ++ (show implicit)) explicit (explicitRockLine implicit)

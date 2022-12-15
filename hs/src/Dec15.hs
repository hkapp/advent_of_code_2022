module Dec15 (run) where

import Test(test)
import Utils(parserStrip, splitFirstSep, fromSingleton, (<$$>))

import Control.Monad.State(State, evalState, put, get, gets)
import qualified Data.Ix as Ix
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe(isJust)
import Data.List(find, transpose)

run :: String -> IO ()
run input =
  do
    unitTest
    let parsed = parse input
    putStrLn "Task 1:"
    let res1 = task1 parsed
    test "task1 (real input)" 5461729 res1
    print $ res1
    putStrLn "Task 2:"
    print $ task2 parsed

{- Parsing -}

type Field = [Sensor]

parse :: String -> Field
parse = map parseSensor . lines

data Sensor = Sensor {
  sensorPos         :: Pos,
  closestBeacon     :: Beacon,
  sensorRangeRadius :: Radius
  }
  deriving Show

type Beacon = Pos
type Radius = Int

parseSensor :: String -> Sensor
parseSensor input = (flip evalState) input $
  do
    parserStrip "Sensor at "
    sensorPos <- parsePos
    parserStrip ": closest beacon is at "
    closestBeacon <- parsePos
    return $ buildSensor sensorPos closestBeacon

parsePos :: State String Pos
parsePos =
  do
    parserStrip "x="
    (xDigits, xRem) <- gets (splitFirstSep ',')
    put xRem
    parserStrip " y="
    (yDigits, yRem) <- gets (splitFirstSep ':')
    put (':':yRem)
    return (read xDigits, read yDigits)

buildSensor :: Pos -> Beacon -> Sensor
buildSensor sensorPos closestBeacon =
  Sensor sensorPos closestBeacon (mdist sensorPos closestBeacon)

{- Coordinate system -}

type Pos = (Int, Int)

xPos :: Pos -> Int
xPos = fst

yPos :: Pos -> Int
yPos = snd

{- Sensor logic -}

{- Absolute Manhattan distance -}
mdist :: Pos -> Pos -> Int
mdist (x, y) (x', y') = (abs (x - x')) + (abs (y - y'))

inSensorRange :: Sensor -> Pos -> Bool
inSensorRange sensor pos = (mdist (sensorPos sensor) pos) <= (sensorRangeRadius sensor)

couldHaveABeacon :: Field -> Pos -> Bool
couldHaveABeacon field = not . beaconWouldBeInRange field

beaconWouldBeInRange :: Field -> Pos -> Bool
beaconWouldBeInRange field pos = any (\sensor -> inSensorRange sensor pos) field

alreadyBeaconAt :: Set Beacon -> Pos -> Bool
-- alreadyBeaconAt xfield = (flip Set.member) (beaconSet xfield)
alreadyBeaconAt = flip Set.member

{- Task 1 -}

data ExtField = ExtField {
  fieldExt  :: Field,
  beaconSet :: Set Beacon
  }

{- Gives ALL edges of sensor radii -}
xFieldEdges :: Field -> [Int]
xFieldEdges = (=<<) sensorEdges
  where
    sensorEdges sensor =
      let
        x = xPos $ sensorPos sensor
        radius = sensorRangeRadius sensor
      in
        [x - radius, x + radius]

{- We need to figure out the min and max values of x for the field -}
buildRow :: Int -> Field -> [Pos]
buildRow y field =
  let
    xMin = minimum $ xFieldEdges field
    xMax = maximum $ xFieldEdges field
  in
    Ix.range ((xMin, y), (xMax, y))

findImpossibleBeacons :: Field -> [Pos] -> [Pos]
findImpossibleBeacons field candidates =
  let
    beacons = buildBeaconSet field
  in
    filter (beaconWouldBeInRange field) $ filter (not . alreadyBeaconAt beacons) $ candidates


impossibleBeaconsOnRow :: Int -> Field -> [Pos]
impossibleBeaconsOnRow y field = findImpossibleBeacons field $ buildRow y field

buildBeaconSet :: Field -> Set Beacon
buildBeaconSet field = Set.fromList $ map closestBeacon field

task1 :: Field -> Int
task1 = task1Param 2000000

task1Param :: Int -> Field -> Int
task1Param y field = length $ impossibleBeaconsOnRow y field

{- Task 2 -}

-- task2 :: Cave -> Int
task2 = task2Param 0 4000000

task2Param :: Int -> Int -> Field -> Int {- Integer -}
task2Param lowerBound upperBound field =
  fromSingleton $ map (\(x, y) -> x * 4000000 + y) $ findImpossibleBeacons field (Ix.range ((lowerBound, lowerBound), (upperBound, upperBound)))

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    debug1
    debug2
    showExample2
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

exampleSensor = buildSensor (8, 7) (2, 10)

validateExample :: IO ()
validateExample =
  do
    test "task1 example" 26 (task1Param 10 exampleParsed)
    test "task2 example" 56000011 (task2Param 0 20 exampleParsed)

debug1 :: IO ()
debug1 =
  do
    test "beacons are always in range" True (all (\s -> inSensorRange s (closestBeacon s)) exampleParsed)
    ier True (5, 2)
    test "range radius of example sensor" 9 (sensorRangeRadius exampleSensor)
    mdi 9 (8, 7) (2, 10)
  where
    ier expected pos = test ("in example range " ++ (show pos)) expected (inSensorRange exampleSensor pos)
    mdi expected pos1 pos2 = test ("mdist " ++ (show pos1) ++ " " ++ (show pos2)) expected (mdist pos1 pos2)

debug2 :: IO ()
debug2 =
  do
    test "impossible example row" ((Ix.range ((-2, 10), (1, 10))) ++ Ix.range ((3, 10), (24, 10))) (impossibleBeaconsOnRow 10 exampleParsed)

showExample2 :: IO ()
showExample2 = putStrLn (showField ((-2, -2), (25, 22)) exampleParsed)

showField :: (Pos, Pos) -> Field -> String
showField (tl, br) field = unlines $ posChar <$$> dispRange
  where
    posChar pos | beaconAt pos = 'B'
    posChar pos | sensorAt pos = 'S'
    posChar pos | beaconWouldBeInRange field pos = '#'
    posChar pos                = '.'

    beaconAt pos = alreadyBeaconAt (Set.fromList $ map closestBeacon field) pos

    sensorAt pos = isJust $ find (\s -> (sensorPos s) == pos) field

    dispRange :: [[Pos]]
    dispRange = map (\y -> map (\x -> (x, y)) $ Ix.range (xPos tl, xPos br)) (Ix.range (yPos tl, yPos br))

module Dec15 (run) where

import Test(test)
import Utils(parserStrip, splitFirstSep, fromSingleton, (<$$>), toMaybe, flattenMaybe, distinct)

import Control.Monad.State(State, evalState, put, get, gets)
import qualified Data.Ix as Ix
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe(isJust)
import Data.List(find, transpose, sortOn, foldl')

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
    let res2 = task2 parsed
    print res2
    test "task2 (real input)" 10621647166538 res2

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

{- Segment -}
{- In this section a "line" always refers to a horizontal line,
   i.e. fixed y, varying x
   Segments are finite subsections of a line
-}

{- Start (left) and end (right)-}
type Segment = (Int, Int)

segLeft :: Segment -> Int
segLeft = fst

segRight :: Segment -> Int
segRight = snd

{- Only cases when two segments are NOT contiguous:
   (we also allow overlapping as contiguous)
  [. .]
          [. .]

          [. .]
  [. .]
-}
notContiguous :: Segment -> Segment -> Bool
notContiguous s1 s2 =
  let
    maxLeft  = max (segLeft s1)  (segLeft s2)
    minRight = min (segRight s1) (segRight s2)
  in
    maxLeft > (1 + minRight)

{- Contiguous or overlapping -}
contiguous :: Segment -> Segment -> Bool
contiguous s1 s2 = not $ notContiguous s1 s2

{-  [. . .]
  [. . .]
  ->
  [. . . .]
-}
{-  [. . .]
      [. . .]
  ->
    [. . . .]
-}
{-  [. .]
        [. .]
  ->
    [. . . .]
-}
segCombine :: Segment -> Segment -> Maybe Segment
segCombine s1 s2 | contiguous s1 s2 = Just $ (min (segLeft s1) (segLeft s2), max (segRight s1) (segRight s2))
segCombine _ _ = Nothing

mergeSegments :: [Segment] -> [Segment]
mergeSegments xs = recMerge $ sortOn segLeft xs
  where
    recMerge (x:y:zs) =
      case segCombine x y of
        Just w  -> recMerge (w:zs)
        Nothing -> x:(recMerge (y:zs))

    recMerge zs = zs

{- Restricts the second segment to the range of the first one -}
{-
allowed:
  [. . . . . .]

  [. .]
  ->
  [. .]

            [. .]
 ->
            [.]

              [. .]
 ->
            []
-}
segRestrict :: Segment -> Segment -> Maybe Segment
segRestrict bounds seg =
  let
    leftBound = segLeft bounds
    rightBound = segRight bounds
    l = max leftBound (segLeft seg)
    r = min rightBound (segRight seg)
    exceedsBounds = l > r
    withinBounds = not exceedsBounds
  in
    toMaybe withinBounds (l, r)

segLength :: Segment -> Int
segLength seg = (segRight seg) - (segLeft seg) + 1

{- we assume that the segments have already been restricted to the given bounds, and that they're sorted -}
findHoles :: Segment -> [Segment] -> [Int]
findHoles bounds segs = snd $ foldl' f ((segLeft bounds) - 1, []) (segs ++ [(rightBound, rightBound)])
  where
    f (currX, currHoles) currSeg = (segRight currSeg, (Ix.range (currX + 1, (segLeft currSeg) - 1)) ++ currHoles)
    rightBound = segRight bounds + 1

{- Sensor logic -}
-- TODO cleanup what's not necessary

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

{-            #
              | (u)
y -->   #-(u)-#-(u)-#
              | (v)
              S
              | (v)
y -->   #-(u)-#-(u)-#
              | (u)
              #

We have:
  v + u = radius(S)
  v = S.y - y
      or y - S.y
    = abs(S.y - y)

If v > radius(S), then we don't get a segment
Otherwise, u = radius(S) - v

Note that when v == radius(S), u == 0
  so the segment length is 1
-}
sensorSegmentOnLine :: Int -> Sensor -> Maybe Segment
sensorSegmentOnLine y sensor =
  let
    v = abs ((yPos $ sensorPos sensor) - y)
    u = (sensorRangeRadius sensor) - v
    sx = xPos $ sensorPos sensor
  in
    toMaybe (u >= 0) (sx - u, sx + u)

fieldInfluenceOnLine :: Int -> Field -> [Segment]
fieldInfluenceOnLine y field =
  mergeSegments $ flattenMaybe $ map (sensorSegmentOnLine y) field

restrictedInfluenceOnLine :: Segment -> Int -> Field -> [Segment]
restrictedInfluenceOnLine bounds y field =
  flattenMaybe $ map (segRestrict bounds) $ fieldInfluenceOnLine y field

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

{- We can't have a beacon where there is already one -}
countCantHaveBeaconOnLine :: Int -> Field -> Int
countCantHaveBeaconOnLine y field =
  let
    sensorPrevents = sum $ map segLength $ fieldInfluenceOnLine y field
    {- Any beacon that appears on the line MUST be in the influence of sensors
       So doing a simple substraction of the counts is ok
     -}
    nbeacons = length $ filter (\b -> (yPos b) == y) $ distinct $ map closestBeacon field
  in
    sensorPrevents - nbeacons

task1Param :: Int -> Field -> Int
task1Param y field = countCantHaveBeaconOnLine y field
-- task1Param y field = length $ impossibleBeaconsOnRow y field

{- Task 2 -}

task2 :: Field -> Integer
task2 = task2Param 0 4000000

possibleDistressSignals :: Int -> Int -> Field -> [Pos]
possibleDistressSignals lowerBound upperBound field =
  let
    yRange = Ix.range (lowerBound, upperBound)
    xBounds = (lowerBound, upperBound)
    rowSegments y = restrictedInfluenceOnLine xBounds y field
    allRowSegments = map rowSegments yRange
    allRowHoles = map (findHoles xBounds) allRowSegments

    identifyDistress :: (Int, [Int]) -> [Pos]
    identifyDistress (y, (x1:x2:xs)) =
      error $ "More than one hole can't happen: y=" ++ (show y) ++ ", x1=" ++ (show x1) ++ ", x2=" ++ (show x2)
    identifyDistress (y, (x:[]))     = [(x, y)]
    identifyDistress (y, [])         = []

    rowHolesWithRowIdx = zip yRange allRowHoles
  in
    identifyDistress =<< rowHolesWithRowIdx

task2Param :: Int -> Int -> Field -> Integer
task2Param lowerBound upperBound field =
  let
    distressSignal = fromSingleton $ possibleDistressSignals lowerBound upperBound field
  in
    (toInteger $ xPos distressSignal) * 4000000 + (toInteger $ yPos distressSignal)
  -- fromSingleton $ map (\(x, y) -> x * 4000000 + y) $ findImpossibleBeacons field (Ix.range ((lowerBound, lowerBound), (upperBound, upperBound)))

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    debug1
    debug2
    testSegments
    showExample2
    debug3
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
    test "task1 example" [(-2, 24)] (fieldInfluenceOnLine 10 exampleParsed)
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

testSegments =
  do
    ctg True (1, 2) (2, 3)
    ctg True (1, 2) (3, 4)
    ctg True (1, 2) (0, 4)
    ctg False (1, 2) (4, 5)
    ctg True (1, 1) (2, 3)

    comb (Just (1, 3)) (1, 2) (2, 3)
    comb (Just (1, 4)) (1, 2) (3, 4)
    comb (Just (0, 4)) (1, 2) (0, 4)
    comb Nothing (1, 2) (4, 5)
    comb (Just (1, 3)) (1, 1) (2, 3)

    mrg [(1, 3)] [(1, 2), (2, 3)]
    mrg [(0, 4)] [(1, 2), (0, 4)]
    mrg [(0, 5)] [(5, 5), (0, 4)]
    mrg [(0, 4), (7, 8)] [(7, 8), (0, 4)]
    mrg [(0, 4), (7, 8)] [(7, 8), (1, 2), (0, 4)]
    mrg [(1, 5)] [(1, 2), (3, 3), (4, 5)]

    ssl (Just (8, 8)) (-2)
    ssl Nothing (-3)
    ssl (Just (7, 9)) (-1)
    ssl (Just (-1, 17)) 7

    sr (Just (2, 2)) (1, 2) (2, 3)
    sr Nothing (1, 2) (3, 4)
    sr (Just (1, 2)) (1, 2) (0, 4)
    sr Nothing (1, 2) (4, 5)
    sr Nothing (1, 1) (2, 3)
  where
    ctg expected s1 s2 = test ("contiguous " ++ (show s1) ++ " " ++ (show s2)) expected (contiguous s1 s2)
    comb expected s1 s2 = test ("segCombine " ++ (show s1) ++ " " ++ (show s2)) expected (segCombine s1 s2)
    mrg expected xs = test ("mergeSegments " ++ (show xs)) expected (mergeSegments xs)
    ssl expected y = test ("sensorSegmentOnLine " ++ (show y)) expected (sensorSegmentOnLine y exampleSensor)
    sr expected s1 s2 = test ("segRestrict " ++ (show s2) ++ " " ++ (show s1)) expected (segRestrict s2 s1)

debug3 :: IO ()
debug3 =
  do
    let lowerBound = 0
    let upperBound = 20
    let xBounds = (lowerBound, upperBound)
    let withinBounds (l, r) = (l >= lowerBound) && (r <= upperBound)
    test "restrictedInfluenceOnLine always within bounds" True (all withinBounds $ restrictedInfluenceOnLine xBounds 14 exampleParsed)
    test "findHoles example row 11" [14] (findHoles xBounds [(0, 13), (15, 20)])
    test "possibleDistressSignals example" [(14, 11)] (possibleDistressSignals lowerBound upperBound exampleParsed)

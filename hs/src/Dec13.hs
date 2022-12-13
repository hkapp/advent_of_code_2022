module Dec13 (run) where

import Test(test)
import Utils(intoPairs, zipWithIndexStarting)

import qualified Data.Ord as Ordering
import Data.Ord(Ordering)
import Data.Bifunctor(first, second)
import Data.List(sort, find)
import Data.Maybe(fromJust)

run :: String -> IO ()
run input =
  do
    unitTest
    let parsed = parse input
    putStrLn "Task 1:"
    print $ task1 parsed
    putStrLn "Task 2:"
    print $ task2 parsed

{- Parsing -}

parse :: String -> [Packet]
parse input = parsePacket <$> (filter (not . null) $ lines input)

parsePacket :: String -> Packet
parsePacket = Packet . read

instance (Read a) => Read (NestedValue a) where
  readsPrec prio s@('[':xs) = fmap (first Nested) $ readsPrec prio s
  readsPrec prio s          = fmap (first Value)  $ readsPrec prio s

instance Show Packet where
  show (Packet xs) = show xs

instance (Show a) => Show (NestedValue a) where
  show (Value  x)  = show x
  show (Nested xs) = show xs

{- Nested -}

newtype Packet = Packet [NestedValue Int]
  deriving Eq

data NestedValue a = Value a | Nested [NestedValue a]

instance Ord Packet where
  compare (Packet nvx) (Packet nvy) = compare (Nested nvx) (Nested nvy)

instance (Ord a) => Ord (NestedValue a) where
  compare (Value x)    (Value y)    = compare x y
  compare vx@(Value x) (Nested ys)  = compareNested [vx] ys
  compare (Nested xs)  vy@(Value y) = compareNested xs [vy]
  compare (Nested xs)  (Nested ys)  = compareNested xs ys

compareNested :: (Ord a) => [NestedValue a] -> [NestedValue a] -> Ordering
compareNested []     []     = EQ
compareNested xs     []     = GT
compareNested []     ys     = LT
compareNested (x:xs) (y:ys) =
  if x == y
    then compareNested xs ys
    else compare x y

instance (Ord a) => Eq (NestedValue a) where
  x == y = (compare x y) == EQ

{- Task 1 -}

inRightOrder :: (Packet, Packet) -> Bool
inRightOrder (x, y) | x == y = error "No right order for equality"
inRightOrder (x, y)          = x < y

task1 :: [Packet] -> Int
task1 = sum . map fst . filter (\(_, p) -> inRightOrder p) . zipWithIndexStarting 1 . intoPairs

{- Task 2 -}

task2 :: [Packet] -> Int
task2 packets =
  let
    div1 = parsePacket "[[2]]"
    div2 = parsePacket "[[6]]"

    addDividers xs = div1:div2:xs

    sortedIndexed = zipWithIndexStarting 1 $ sort $ addDividers packets

    findIndex p1 = fst $ fromJust $ find (\(_, p2) -> p1 == p2) sortedIndexed
  in
    (findIndex div1) * (findIndex div2)


{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    debug1
    debug2
    validateExample

example = unlines [
  "[1,1,3,1,1]",
  "[1,1,5,1,1]",
  "",
  "[[1],[2,3,4]]",
  "[[1],4]",
  "",
  "[9]",
  "[[8,7,6]]",
  "",
  "[[4,4],4,4]",
  "[[4,4],4,4,4]",
  "",
  "[7,7,7,7]",
  "[7,7,7]",
  "",
  "[]",
  "[3]",
  "",
  "[[[]]]",
  "[[]]",
  "",
  "[1,[2,[3,[4,[5,6,7]]]],8,9]",
  "[1,[2,[3,[4,[5,6,0]]]],8,9]"
  ]

parsedExample = parse example
parsedExample1 = intoPairs parsedExample

validateExample :: IO ()
validateExample =
  do
    test "task1 example" 13 (task1 parsedExample)
    test "task2 example" 140 (task2 parsedExample)

debug1 :: IO ()
debug1 =
  do
    test "inRightOrder example" [True, True, False, True, False, True, False, False] (inRightOrder <$> parsedExample1)
    testPair 4 True
    testPair 5 False
    testPair 6 True
    testPair 7 False
  where
    testPair idx expected = test ("inRightOrder (pair " ++ (show idx) ++ ")") expected (inRightOrder $ parsedExample1 !! (idx - 1))

debug2 :: IO ()
debug2 =
  do
    step "1" "1" EQ
    step "3" "5" LT
    step "[1]" "[1]" EQ
    step "[2,3,4]" "4" LT
    step "[2,3,4]" "[4]" LT
    step "2" "4" LT
    step "9" "[8,7,6]" GT
    step "[9]" "[8,7,6]" GT
    step "9" "8" GT
    step "[]" "[3]" LT
    step "[[]]" "[]" GT
    step "[2,[3,[4,[5,6,7]]]]" "[2,[3,[4,[5,6,0]]]]" GT
    step "[3,[4,[5,6,7]]]" "[3,[4,[5,6,0]]]" GT
    step "[4,[5,6,7]]" "[4,[5,6,0]]" GT
    step "[5,6,7]" "[5,6,0]" GT
    step "[4]" "[[4]]" EQ
    step "[[4]]" "[[[4]]]" EQ
    step "[4]" "[[[4]]]" EQ

    parsing "[]" (Packet [])
    parsing "[[]]" (Packet [Nested []])
    parsing "[[[]]]" (Packet [Nested [Nested []]])

    parseAndReturn "[]"
    parseAndReturn "[[]]"
    parseAndReturn "[[[]]]"
  where
    step sl sr expected = test ("compare " ++ sl ++ " " ++ sr) expected (compare (read sl :: NestedValue Int) (read sr :: NestedValue Int))
    parsing s expected = test ("parsing " ++ (show s)) expected (parsePacket s)
    parseAndReturn s = test ("parseAndReturn " ++ (show s)) s (show $ parsePacket s)

module Dec11 (run) where

import Prelude hiding (round)

import Utils(arrayFromIndexedList, splitSep, stripPrefix, stripSuffix, top)
import Test(test)

import Control.Monad(replicateM, replicateM_)
import Control.Monad.State(State, evalState, state, get, put)
import Data.Char(isDigit)
import Data.Bifunctor(first, second)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.List(transpose)
import Data.Word(Word64)
import Data.Foldable(traverse_)
import System.Random(randomRIO)

run :: String -> IO ()
run input =
  do
    unitTest
    let (troop, hold) = parse input
    putStrLn "Task 1:"
    print $ task1 troop hold
    putStrLn "Task 2:"
    print $ task2 troop hold

{- Queue -}

type Stack a = [a]
data Queue a = Queue (Stack a) (Stack a)

pushQ :: a -> Queue a -> Queue a
pushQ elem (Queue popStack pushStack) = Queue popStack (elem:pushStack)

popQ :: Queue a -> (a, Queue a)
popQ (Queue popStack pushStack) =
  case popStack of
    [] ->
      let
        newPopStack = reverse pushStack
      in
        (head newPopStack, Queue (tail newPopStack) [])
    (x:xs) ->
      (x, Queue xs pushStack)

nullQ :: Queue a -> Bool
nullQ (Queue [] []) = True
nullQ _             = False

newQ :: Queue a
newQ = Queue [] []

{- Popping will get the elements in the same order as in the original list -}
fromListQ :: [a] -> Queue a
fromListQ xs = Queue xs []

instance Show a => Show (Queue a) where
  show (Queue popStack pushStack) = "Queue <" ++ (show (popStack ++ pushStack)) ++ "<"

{- Parsing -}

type Hold  = Map MIdx (Queue Item)

type Troop = [Monkey]

type MIdx  = Int
type Item  = Worry
type Worry = Int

parse :: String -> (Troop, Hold)
parse input = intoTroopAndHold $ parseMonkeyAndItems <$> (splitSep [] $ lines input)

intoTroopAndHold :: [(MIdx, Monkey, [Item])] -> (Troop, Hold)
intoTroopAndHold parseOutput =
  let
    troop = intoTroop $ map (\(idx, monkey, _) -> (idx, monkey)) parseOutput
    hold  = intoHold  $ map (\(idx, _, items)  -> (idx, items))  parseOutput
  in
    (troop, hold)

parseMonkeyAndItems :: [String] -> (MIdx, Monkey, [Item])
parseMonkeyAndItems (l1:l2:ls) =
  let
    (idx, monkey) = parseMonkey (l1:ls)
    items = parseItems l2
  in
    (idx, monkey, items)

parseMonkey :: [String] -> (MIdx, Monkey)
parseMonkey (idxLine:opLine:testLine:trueLine:falseLine:[]) =
  let
    idx = parseMIdx idxLine
    op = parseOp opLine
    divTest = parseDivTest testLine
    ifTrue = parseTrueCase trueLine
    ifFalse = parseFalseCase falseLine
  in
    (idx, makeMonkey idx op divTest ifTrue ifFalse)

parseMIdx :: String -> MIdx
parseMIdx = read . stripSuffix ":" . stripPrefix "Monkey "

type Op = (Worry -> Worry)

parseOp :: String -> Op
parseOp = evalState $
  do
    parserStrip "  Operation: new = "
    leftOperand  <- parseOperand
    parserStrip " "
    arithmetic   <- parseArithmetic
    parserStrip " "
    rightOperand <- parseOperand
    return $ makeOp leftOperand arithmetic rightOperand

parserStrip :: String -> State String ()
parserStrip prefix = state (\s -> ((), stripPrefix prefix s))

type Operand = (Worry -> Worry)

parseOperand :: State String Operand
parseOperand =
  do
    s <- get
    case head s of
      'o' ->
        do
          parserStrip "old"
          return oldOperand
      _   ->
        do
          let (digits, remaining) = span isDigit s
          put remaining
          return $ numOperand $ read digits

oldOperand :: Operand
oldOperand = id

numOperand :: Worry -> Operand
numOperand = const

type Arithmetic = (Worry -> Worry -> Worry)

parseArithmetic :: State String Arithmetic
parseArithmetic =
  do
    s <- get
    put $ tail s
    return $ case head s of
      '+' -> (+)
      '*' -> (*)

makeOp :: Operand -> Arithmetic -> Operand -> Op
makeOp leftOperand arithmetic rightOperand oldValue =
  (leftOperand oldValue) `arithmetic` (rightOperand oldValue)

type DivTest = (Worry -> Bool)

parseDivTest :: String -> DivTest
parseDivTest testLine x =
  let
    y = read $ stripPrefix "  Test: divisible by " testLine
  in
    (x `mod` y) == 0

type Outcome = MIdx

parseTrueCase :: String -> Outcome
parseTrueCase = parseThrow . stripPrefix "    If true: "

parseFalseCase :: String -> Outcome
parseFalseCase = parseThrow . stripPrefix "    If false: "

parseThrow :: String -> Outcome
parseThrow = read . stripPrefix "throw to monkey "

intoTroop :: [(MIdx, Monkey)] -> Troop
intoTroop = map snd

parseItems :: String -> [Item]
parseItems itemLine = map read $ splitTwoChar ", " $ stripPrefix "  Starting items: " itemLine
  where splitTwoChar (c1:c2:[]) = splitSep c1 . filter ((/=) c2)
  {- this doesn't work in general, but will work in this instance -}

{- See the note above Hold. We need to reverse the Item list in the Hold. -}
intoHold :: [(MIdx, [Item])] -> Hold
intoHold = Map.fromList . map (second fromListQ)

{- Monkey business -}

{- A monkey is only defined by its inspection method
   Given an item, it knows how to compute the new worry for this item,
   and which monkey to throw it to.
-}
data Monkey = Monkey {
  selfIdx :: MIdx,
  inspect :: (Relief -> Item -> (Worry, MIdx))
  }

assertMessage :: Bool -> a -> String -> a
assertMessage cond val msg =
  if not cond
    then error msg
    else val

makeMonkey :: MIdx -> Op -> DivTest -> Outcome -> Outcome -> Monkey
makeMonkey idx op divTest ifTrue ifFalse = Monkey idx buildInspect
  where buildInspect relief item =
          let
            iw = op item
            increasedWorry = assertMessage (iw > item) iw $ unlines [
                                                              "Integer overflow detected: " ++ (show item) ++ " -> " ++ (show iw),
                                                              "op 0 = " ++ (show $ op 0),
                                                              "op 1 = " ++ (show $ op 1)
                                                              ]

            reducedWorry = relief increasedWorry

            recipientMonkey =
              if divTest reducedWorry
                then ifTrue
                else ifFalse
          in
            (reducedWorry, recipientMonkey)

type Relief = Worry -> Worry

inspectOnce :: Relief -> Monkey -> State Hold ()
inspectOnce relief monkey =
  do
    hold <- get
    currItem <- grabItem (selfIdx monkey)
    let (newItemWorry, recipientMonkey) = inspect monkey relief currItem
    throwItem newItemWorry recipientMonkey

itemsHeldBy :: MIdx -> Hold -> Queue Item
itemsHeldBy = flip (Map.!)

grabItem :: MIdx -> State Hold Item
grabItem monkeyIdx =
  do
    hold <- get
    let itemsHeld = itemsHeldBy monkeyIdx hold
    let (currItem, itemsHeldAfter) = popQ itemsHeld
    let newHold = Map.insert monkeyIdx itemsHeldAfter hold
    put newHold
    return currItem

throwItem :: Item -> MIdx -> State Hold ()
throwItem item recipient =
  do
    hold <- get
    put $ Map.adjust (pushQ item) recipient hold

{- Make one monkey inspect all of its items
   At the same time, we count how many items it's going through
-}
turn :: Relief -> Monkey -> State Hold Int
turn relief monkey =
  do
    hold <- get
    if hasItems (selfIdx monkey) hold
      then return 0
      else
        do
          inspectOnce relief monkey
          countItemsInspected <- turn relief monkey
          return (countItemsInspected + 1)

hasItems :: MIdx -> Hold -> Bool
hasItems monkeyIdx = nullQ . itemsHeldBy monkeyIdx

type MonkeyBusiness = [Int]

{- Make every monkey inspect every item it has, once per monkey, in order
   Count how many items each monkey went through along the way
-}
round :: Relief -> Troop -> State Hold MonkeyBusiness
round relief = traverse (turn relief)

repeatS :: Int -> State s a -> State s [a]
repeatS = replicateM

{- Task 1 -}

withRelief :: Relief
withRelief worry = worry `div` 3

task1 :: Troop -> Hold -> Int
task1 troop = monkeyBusinessLevel 20 (round withRelief troop)

monkeyBusinessLevel :: Int -> State Hold MonkeyBusiness -> Hold -> Int
monkeyBusinessLevel nrounds singleRound hold =
  product $ top 2 $ totalMonkeyBusiness nrounds singleRound hold

totalMonkeyBusiness :: Int -> State Hold MonkeyBusiness -> Hold -> [Int]
totalMonkeyBusiness nrounds singleRound hold =
  map sum $ transpose $ (flip evalState) hold $ repeatS nrounds singleRound

{- Task 2 -}

type Magic = [Worry]

task2Magic :: Magic
task2Magic = [3, 13, 2, 11, 5, 17, 19, 7]

reduceNumber :: Magic -> Worry -> Worry
reduceNumber magicList w = validate (w `mod` magicNumber)
  where magicNumber = product magicList
        validate z = assertMessage (all (\m -> (w `mod` m) == (z `mod` m)) magicList) z $ ("The magic doesn't work on " ++ (show w))

noReliefButManage :: Magic -> Relief
noReliefButManage = reduceNumber

task2 :: Troop -> Hold -> Int
task2 = task2Param task2Magic 10000

task2Param :: Magic -> Int -> Troop -> Hold -> Int
task2Param magic nrounds troop = monkeyBusinessLevel nrounds (round (noReliefButManage magic) troop)

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    example <- exampleInput
    let (troop, hold) = parse example
    debug1 troop hold
    validateExample troop hold

exampleInput = readFile "../../data/day11.example.txt"

exampleMagic = [23, 19, 13, 17]

validateExample :: Troop -> Hold -> IO ()
validateExample troop hold =
  do
    test "task1 example" 10605 (task1 troop hold)
    test "task2 example (1 round)" (4*6) (task2Param exampleMagic 1 troop hold)
    test "task2 example (20 rounds)" (99*103) (task2Param exampleMagic 20 troop hold)
    test "task2 example (1000 rounds)" (5204*5192) (task2Param exampleMagic 1000 troop hold)
    test "task2 example (10'000 rounds)" 2713310158 (task2Param exampleMagic 10000 troop hold)

debug1 :: Troop -> Hold -> IO ()
debug1 troop hold =
  do
    mbll [2, 4, 3, 6] 1
    mbll [99, 97, 8, 103] 20
    mbll [5204, 4792, 199, 5192] 1000
    testRandomItems 100
  where
    mbll expected nrounds =
      let
        msg = "totalMonkeyBusiness after " ++ (show nrounds) ++ " rounds"
        found = totalMonkeyBusiness nrounds (round (noReliefButManage exampleMagic) troop) hold
      in
        test msg expected found

    testRandomItems nitems = replicateM_ nitems $
                                do
                                  let positive = (0, floor $ sqrt $ fromIntegral (maxBound :: Worry))
                                  randItem <- randomRIO positive
                                  traverse_ (\monkey ->
                                    let
                                      msg = "inspect (Monkey " ++ (show $ selfIdx monkey) ++ ") " ++ (show randItem)
                                      expected = first (reduceNumber exampleMagic) (inspect monkey id randItem)
                                      found = inspect monkey (noReliefButManage exampleMagic) randItem
                                    in
                                      test msg expected found
                                    ) troop

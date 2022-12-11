module Dec11 (run) where

import Prelude hiding (round)

import Utils(arrayFromIndexedList, splitSep, stripPrefix, stripSuffix, top)
import Test(test)

import Control.Monad.State(State, evalState, state, get, put)
import Data.Char(isDigit)
import Data.Bifunctor(second)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.List(transpose)

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

type Op = (Int -> Int)

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

type Operand = (Int -> Int)

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

numOperand :: Int -> Operand
numOperand = const

type Arithmetic = (Int -> Int -> Int)

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

type DivTest = (Int -> Bool)

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

makeMonkey :: MIdx -> Op -> DivTest -> Outcome -> Outcome -> Monkey
makeMonkey idx op divTest ifTrue ifFalse = Monkey idx buildInspect
  where buildInspect relief item =
          let
            increasedWorry = op item
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
repeatS n st = sequence $ take n $ repeat st

{- Task 1 -}

withRelief :: Relief
withRelief worry = worry `div` 3

task1 :: Troop -> Hold -> Int
task1 troop = monkeyBusinessLevel 20 (round withRelief troop)

monkeyBusinessLevel :: Int -> State Hold MonkeyBusiness -> Hold -> Int
monkeyBusinessLevel nrounds singleRound hold =
  product $ top 2 $ map sum $ transpose $ (flip evalState) hold $ repeatS nrounds singleRound

{- Task 2 -}

noRelief :: Relief
noRelief = id

task2 :: Troop -> Hold -> Int
task2 troop = monkeyBusinessLevel 1000 (round noRelief troop)

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    validateExample

exampleInput = readFile "../../data/day11.example.txt"

validateExample :: IO ()
validateExample =
  do
    example <- exampleInput
    let (troop, hold) = parse example
    test "task1 example" 10605 (task1 troop hold)
    test "task2 example" 2713310158 (task2 troop hold)

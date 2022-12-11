module Dec11 (run) where

import Prelude hiding (Left, Right)

import Utils(arrayFromIndexedList, splitSep, stripPrefix, stripSuffix)
import Test(test)

import Data.Array.IArray(Array)
import Control.Monad.State(State, evalState, state, get, put)
import Data.Char(isDigit)
import Data.Bifunctor(second)

run :: String -> IO ()
run input =
  do
    unitTest
    let (troop, hold) = parse input
    putStrLn "Task 1:"
    print $ task1 troop hold
    putStrLn "Task 2:"
    putStrLn $ task2 troop hold

{- Parsing -}

{- Warning! we use the Item list as a queue for efficient push
   When reading it (for inspection), it needs to be reversed first
-}
type Queue a = [Item]
type Hold  = Array MIdx (Queue Item)

type Troop = Array MIdx Monkey

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
    (idx, makeMonkey op divTest ifTrue ifFalse)

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
parseFalseCase = parseThrow . stripPrefix "    If true: "

parseThrow :: String -> Outcome
parseThrow = read . stripPrefix "throw to monkey "

intoTroop :: [(MIdx, Monkey)] -> Troop
intoTroop = arrayFromIndexedList

parseItems :: String -> [Item]
parseItems itemLine = map read $ splitTwoChar ", " $ stripPrefix "  Starting items: " itemLine
  where splitTwoChar (c1:c2:[]) = splitSep c1 . filter ((/=) c2)
  {- this doesn't work in general, but will work in this instance -}

{- See the note above Hold. We need to reverse the Item list in the Hold. -}
intoHold :: [(MIdx, [Item])] -> Hold
intoHold = arrayFromIndexedList . map (second reverse)

{- Monkey business -}

{- A monkey is only defined by its inspection method
   Given an item, it knows how to compute the new worry for this item,
   and which monkey to throw it to.
-}
type Monkey = (Item -> (Worry, MIdx))

makeMonkey :: Op -> DivTest -> Outcome -> Outcome -> Monkey
makeMonkey op divTest ifTrue ifFalse item =
  let
    increasedWorry = op item
    reducedWorry = relief increasedWorry
    recipientMonkey =
      if divTest reducedWorry
        then ifTrue
        else ifFalse
  in
    (reducedWorry, recipientMonkey)

relief :: Worry -> Worry
relief worry = worry `div` 3

{- Task 1 -}

-- task1 :: Troop -> Hold -> Int
task1 _ hold = hold

{- Task 2 -}

-- task2 :: [Inst] -> String
task2 _ _ = "not implemented"

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    validateExample

example = readFile "../../data/day10.example.txt"

exampleInsts = parse <$> example

validateExample :: IO ()
validateExample = return ()
  -- do
    -- insts <- exampleInsts
    -- test "task1 example" 13140 (task1 insts)

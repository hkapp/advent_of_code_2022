module Dec10 (run) where

import Prelude hiding (Left, Right)

import Test(test)
import Utils(zipWithIndexStarting, evalStarting, chunksOf)

import Control.Monad.State(State, state)

run :: String -> IO ()
run input =
  do
    unitTest
    let inst = parse input
    putStrLn "Task 1:"
    print $ task1 inst
    putStrLn "Task 2:"
    putStrLn $ task2 inst

{- Parsing -}

data Inst = Noop | Addx Int
  deriving Show

parse :: String -> [Inst]
parse input = parseInst <$> lines input

parseInst :: String -> Inst
parseInst = intoInst . splitAt 5

intoInst :: (String, String) -> Inst
intoInst ("noop", _)  = Noop
intoInst ("addx ", n) = Addx $ read n

{- Processor -}

type RegVal = Int
type Register = Int

execInst :: Inst -> Register -> [RegVal]
execInst Noop     reg = [reg]
execInst (Addx n) reg = [reg, reg+n]

simInst :: Inst -> State Register [RegVal]
simInst inst = state (\r ->
  let
    stateSeq = execInst inst r
  in
    (stateSeq, last stateSeq))

simulate :: [Inst] -> State Register [RegVal]
simulate = fmap ((=<<) id) . sequence . map simInst

{- Task 1 -}

task1 :: [Inst] -> Int
task1 = sum . interestingSignals

interestingSignals :: [Inst] -> [Int]
interestingSignals = signalStrengthEvery 20 40 . exec

exec :: [Inst] -> [RegVal]
exec insts = evalStarting (simulate insts) initReg

initReg :: Register
initReg = 1

type Clock = Int

signalStrength :: (Clock, RegVal) -> Int
signalStrength (c, r) = c * r

signalStrengthEvery :: Int -> Int -> [RegVal] -> [Int]
signalStrengthEvery n1 nrep = map signalStrength . cpuStateEvery n1 nrep

cpuStateEvery :: Int -> Int -> [RegVal] -> [(Clock, RegVal)]
cpuStateEvery n1 nrep = filter (clockTime n1 nrep . fst) . zipWithIndexStarting 1

clockTime :: Int -> Int -> Clock -> Bool
clockTime n1 nrep clock = (clock >= n1) && ((clock - n1) `mod` nrep == 0)

{- Task 2 -}

task2 :: [Inst] -> String
task2 insts = showScreen $ firstScreen $ map (uncurry draw) $ zipWithIndexStarting 1 $ exec insts

data Pixel = Lit | Dark
type PixelPos = Int

draw :: Clock -> RegVal -> Pixel
draw clock spritePos = if spriteIsVisible (clockToPixelPos clock) spritePos then Lit else Dark

clockToPixelPos :: Clock -> PixelPos
clockToPixelPos clock = (clock - 1) `mod` rowLength

rowLength = 40

spriteIsVisible :: PixelPos -> PixelPos -> Bool
spriteIsVisible pixelPos spriteMid = (pixelPos == (spriteMid - 1))
                                     || (pixelPos == spriteMid)
                                     || (pixelPos == (spriteMid + 1))

type Screen = [Row]
type Row    = [Pixel]

firstScreen :: [Pixel] -> Screen
firstScreen = head . intoScreens

intoScreens :: [Pixel] -> [Screen]
intoScreens = chunksOf 6 . intoRows

intoRows :: [Pixel] -> [Row]
intoRows = chunksOf rowLength

showScreen :: Screen -> String
showScreen = unlines . map showRow

showRow :: Row -> String
showRow pxs = pxs >>= show

instance Show Pixel where
  show Lit = "#"
  show Dark = "."

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    debug1
    invariants
    validateExample

example = readFile "../../data/day10.example.txt"

exampleInsts = parse <$> example

validateExample :: IO ()
validateExample =
  do
    insts <- exampleInsts
    test "task1 example" 13140 (task1 insts)

debug1 :: IO ()
debug1 =
  do
    insts <- exampleInsts
    let regs = exec insts
    test "cpuStateEvery example" [21, 19, 18, 21, 16, 18] (map snd $ cpuStateEvery 20 40 regs)
    test "interestingSignals example" [420, 1140, 1800, 2940, 2880, 3960] (interestingSignals insts)

invariants :: IO ()
invariants =
  do
    test "visible 0 1" True  (spriteIsVisible 0 1)
    test "visible 0 2" False (spriteIsVisible 0 2)

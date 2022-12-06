module Dec5 (run) where

import Data.Bifunctor(bimap)
import Data.List(unfoldr, transpose, foldl')
import Data.Char(isSpace, isDigit)
import Data.Maybe(maybeToList)
import Control.Exception(assert)
import Data.Map(Map, (!))
import qualified Data.Map as Map
import Control.Monad.State(State, get, put, evalState)

run :: String -> IO ()
run input =
  do
    unitTest
    let gameInput = parse $ lines input
    putStrLn "Task 1:"
    print $ uncurry task1 gameInput
    putStrLn "Task 2:"
    print $ uncurry task2 gameInput


parse lines = bimap parseDocks parseMoves $ splitFirstSep [] lines

splitFirstSep :: (Eq a) => a -> [a] -> ([a], [a])
splitFirstSep elem xs =
  let (prefix, suffix) = span ((/=) elem) xs
  in (prefix, tail suffix)

type Harbour = Map Int Docks
type Docks = Stack Crate
type Stack a = [a]
type Crate = Char

{-
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3
-}
parseDocks :: [String] -> Harbour
parseDocks lines = intoHarbour $ intoDocks $ parseDockLine <$> lines
  where parseDockLine l = extractCrate <$> makeDockCols l

{- "[N] [C]" => ["[N]","[C]"] -}
makeDockCols :: String -> [String]
makeDockCols line = take 3 <$> splitEvery 4 line

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = unfoldr f
  where f [] = Nothing
        f xs = Just $ splitAt n xs

{- "[Z]" -> Just 'Z'
   " 1 " -> Nothing
   "   " -> Nothing
-}
extractCrate :: String -> Maybe Crate
extractCrate (' ':n:' ':[]) = Nothing
extractCrate (' ':n:[])     = Nothing
extractCrate ('[':c:']':[]) = Just c
extractCrate s = error s

{- [[Nothing, Just D ]
    [Just N , Just C ]
    [Just Z , Just M , Just P]
    [Nothing, Nothing, Nothing]]
   =>
   [[Z, N],
    [M, C, D],
    [P]]
-}
intoDocks :: [[Maybe Crate]] -> [Docks]
intoDocks rows = map ((=<<) maybeToList) $ transpose rows

intoHarbour :: [Docks] -> Harbour
intoHarbour = fst . foldl' f (Map.empty, 1)
  where f (harbour, index) dock = (Map.insert index dock harbour, index + 1)

data Move = Move
  {
    quantity :: Int,
    srcDock  :: Int,
    dstDock  :: Int
  }
  deriving (Show)

{- move 1 from 2 to 1 -}
parseMoves :: [String] -> [Move]
parseMoves = map (evalState parseMove)
  where
    parseMove :: State String Move
    parseMove =
      do
        skipStr "move "
        c    <- takeDigits
        skipStr " from "
        src  <- takeDigits
        skipStr " to "
        dst  <- takeDigits
        return $ Move c src dst

    skipStr :: String -> State String ()
    skipStr toSkip =
      do
        s <- get
        let (prefix, remain) = splitAt (length toSkip) s
        put remain
        assert (toSkip == prefix) $ return ()

    takeDigits :: State String Int
    takeDigits =
      do
        s <- get
        let (digStr, rem) = span isDigit s
        put rem
        return $ read digStr

{- Task 1 -}

processWith :: Crane -> Harbour -> [Move] -> [Crate]
processWith crane docks moves =
  map head $ Map.elems $ foldl' (applyMove crane) docks moves

task1 :: Harbour -> [Move] -> [Crate]
task1 = processWith pushCrates1

type Crane = [Crate] -> Docks -> Docks

pushCrates1 :: [Crate] -> Docks -> Docks
pushCrates1 movedCrates dstDock = (reverse movedCrates) ++ dstDock

applyMove :: Crane -> Harbour -> Move -> Harbour
applyMove pushCrates harbour (Move count srcIdx dstIdx) =
  let
    srcDock               = harbour ! srcIdx
    (movedCrates, remSrc) = splitAt count srcDock
    harbour2              = Map.insert srcIdx remSrc harbour
  in
    Map.adjust (pushCrates movedCrates) dstIdx harbour2

{- Task 2 -}

pushCrates2 :: [Crate] -> Docks -> Docks
pushCrates2 movedCrates dstDock = movedCrates ++ dstDock

task2 :: Harbour -> [Move] -> [Crate]
task2 = processWith pushCrates2

{- Unit test -}

unitTest :: IO ()
unitTest =
  do
    validateExample

test :: (Eq a, Show a) => String -> a -> a -> IO ()
test message expected found =
  if expected == found
    then return ()
    else
      do
        putStrLn $ "[FAIL] " ++ message
        putStrLn $ "Expected: " ++ (show expected)
        putStrLn $ "Found:    " ++ (show found)

example = [
  "    [D]    ",
  "[N] [C]    ",
  "[Z] [M] [P]",
   "1   2   3 ",
  "",
  "move 1 from 2 to 1",
  "move 3 from 1 to 3",
  "move 2 from 2 to 1",
  "move 1 from 1 to 2"
  ]

validateExample =
  do
    test "task1 example" "CMZ" (uncurry task1 (parse example))
    test "task2 example" "MCD" (uncurry task2 (parse example))

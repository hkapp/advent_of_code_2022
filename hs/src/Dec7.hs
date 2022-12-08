module Dec7 (run) where

import Utils(splitSep)
import Test(test)

import Data.Char(isDigit)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.List(foldl')
import Control.Exception(assert)

run :: String -> IO ()
run input =
  do
    unitTest
    let parsed = parse input
    let fs = buildFs parsed
    putStrLn "Task 1:"
    print $ task1 fs
    putStrLn "Task 2:"
    print $ task2 fs

{- Parsing -}

parse input = parseLine <$> lines input

parseLine :: String -> BashLine
parseLine l =
  let
    chunks = splitSep ' ' l
  in
    case head l of
      '$'           -> parseCommand (tail chunks)
      'd'           -> parseDir chunks
      c | isDigit c -> parseFile chunks

data BashLine =
  BashCd CdCmd
  | BashLs
  | BashDir String
  | BashFile Int String
  deriving Show

data CdCmd =
  CdUp
  | CdDown String
  | CdRoot
  deriving Show

parseCommand :: [String] -> BashLine

parseCommand ("cd":arg:[]) = BashCd $
  case arg of
    ".."   -> CdUp
    "/"    -> CdRoot
    dirnam -> CdDown dirnam

parseCommand ("ls":[]) = BashLs

parseCommand xs = error $ "Unmatched command: " ++ (show xs)


parseDir :: [String] -> BashLine
parseDir ("dir":dirnam:[]) = BashDir dirnam

parseFile :: [String] -> BashLine
parseFile (szStr:filename:[]) = BashFile (read szStr) filename

{- File system -}

type Fs = Map Path INode
{- Careful! We use the list as a stack here, so the bash path is given by reverse Path -}
newtype Path = Path [String]
  deriving (Ord, Eq, Show)
data INode = Dir | File Int
  deriving Show

buildFs :: [BashLine] -> Fs
buildFs = snd . foldl' interpret (root, Map.empty)

interpret :: (Path, Fs) -> BashLine -> (Path, Fs)
interpret (currPath, currFs) bash =
  case bash of
    BashCd CdUp         -> (parent currPath,   currFs)
    BashCd (CdDown dir) -> (push dir currPath, currFs)
    BashCd CdRoot       -> (root,              currFs)

    BashLs -> (currPath, currFs)

    BashDir dirname -> (currPath, newDir currPath dirname currFs)

    BashFile size filename -> (currPath, newFile currPath filename size currFs)

root :: Path
root = Path []

parent :: Path -> Path
parent (Path xs) = Path (tail xs)

push :: String -> Path -> Path
push new (Path path) = Path $ new:path

newDir :: Path -> String -> Fs -> Fs
newDir parentDir dirname = Map.insert (push dirname parentDir) Dir

newFile :: Path -> String -> Int -> Fs -> Fs
newFile dir filename size = Map.insert (push filename dir) (File size)

{- Task 1 -}

allDirs :: Fs -> [Path]
allDirs = Map.keys . Map.filter isDir

isDir :: INode -> Bool
isDir Dir = True
isDir _   = False

task1 :: Fs -> Int
task1 = sum . filter (atMost 100000) . allDirSizes
  where atMost n m = m <= n

allDirSizes :: Fs -> [Int]
allDirSizes fs = dirSize fs <$> allDirs fs

dirSize :: Fs -> Path -> Int
dirSize fs dir = sum $ Map.map inodeSize $ dirFiles fs dir

dirFiles :: Fs -> Path -> Fs
dirFiles fs dir = filterKeys (under dir) fs
  where filterKeys f = Map.filterWithKey (\k a -> f k)

under :: Path -> Path -> Bool
under (Path parent) (Path child) =
  let
    plen = length parent
    clen = length child
  in
    (plen < clen) && (drop (clen - plen) child == parent)

inodeSize :: INode -> Int
inodeSize Dir       = 0
inodeSize (File sz) = sz

{- Task 2 -}

task2 :: Fs -> Int
task2 fs =
  let
    requiredFree = 30000000
    totSize = 70000000

    totUsed = sum $ Map.map inodeSize fs
    currFree = totSize - totUsed
    needToFree = assert (currFree < requiredFree) (requiredFree - currFree)

    bigEnough dirsz = dirsz >= needToFree
  in
    minimum $ filter bigEnough $ allDirSizes fs

{- Unit Test -}

unitTest :: IO ()
unitTest =
  do
    testSplitSep
    validateExample

example = unlines [
  "$ cd /",
  "$ ls",
  "dir a",
  "14848514 b.txt",
  "8504156 c.dat",
  "dir d",
  "$ cd a",
  "$ ls",
  "dir e",
  "29116 f",
  "2557 g",
  "62596 h.lst",
  "$ cd e",
  "$ ls",
  "584 i",
  "$ cd ..",
  "$ cd ..",
  "$ cd d",
  "$ ls",
  "4060174 j",
  "8033020 d.log",
  "5626152 d.ext",
  "7214296 k"
  ]

exampleFs = buildFs $ parse example

validateExample :: IO ()
validateExample =
  do
    test "task1 example" 95437 (task1 exampleFs)
    test "task2 example" 24933642 (task2 exampleFs)

testSplitSep :: IO ()
testSplitSep =
  do
    ts ' ' "ab c" ["ab", "c"]
    ts ' ' "$ cd /" ["$", "cd", "/"]
  where
    ts sep inp expected = test (message sep inp) expected (splitSep sep inp)
    message sep inp = "splitSep '" ++ (show sep) ++ "' \"" ++ inp ++ "\""

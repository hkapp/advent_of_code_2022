module Utils where

import Data.Array.IArray(Array, array)
import qualified Data.Array.IArray as Array
import qualified Data.Char
import Data.List(unfoldr, sort, sortOn)
import Data.Ord(Down(..))
import qualified Data.Ix
import Data.Maybe(maybeToList)

import Control.Monad.State(State, evalState, get)
import Control.Monad(join)

splitFirstSep :: (Eq a) => a -> [a] -> ([a], [a])
splitFirstSep elem xs =
  let
    (prefix, suffix) = span ((/=) elem) xs

    safeTail [] = []
    safeTail xs = tail xs
  in
    (prefix, safeTail suffix)

splitSep :: (Eq a) => a -> [a] -> [[a]]
splitSep sep = unfoldr f
  where f [] = Nothing
        f xs = Just $ splitFirstSep sep xs

(<$$>) :: (a -> b) -> [[a]] -> [[b]]
(<$$>) f = map (map f)

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zipWithIndexStarting 0

zipWithIndexStarting :: Int -> [a] -> [(Int, a)]
zipWithIndexStarting n (x:xs) = (n, x):(zipWithIndexStarting (n+1) xs)
zipWithIndexStarting _ []     = []

zipSquareWithIndex :: [[a]] -> [(Int, Int, a)]
zipSquareWithIndex xs =
  let
    withColIdx = map zipWithIndex xs
    withOuterRowIdx = zipWithIndex withColIdx
  in
    (\(ri, rx) -> map (\(ci, y) -> (ri, ci, y)) rx) =<< withOuterRowIdx

evalStarting :: State a [a] -> a -> [a]
evalStarting st x = x:(evalState st x)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr f
  where f [] = Nothing
        f xs = Just $ splitAt n xs

flatten :: (Monad m) => m (m a) -> m a
flatten = join

arrayFromIndexedList :: [(Int, a)] -> Array Int a
arrayFromIndexedList xs =
  let
    indexes = fst <$> xs
  in
    array (minimum indexes, maximum indexes) xs

{- First returned string is the stripped bit to compare
   Second String argument is the expected stripped value
-}
stripBy :: (String -> (String, String)) -> String -> String -> String
stripBy splitStr expected input =
  let
    (toCompare, remaining) = splitStr input
  in
    if (toCompare /= expected)
      then error $ "stripBy: stripped bit " ++ (show toCompare) ++ " did not match expected " ++ (show expected)
      else remaining

stripPrefix :: String -> String -> String
stripPrefix toStrip = stripBy splitStr toStrip
  where splitStr = splitAt (length toStrip)

stripSuffix :: String -> String -> String
stripSuffix toStrip = stripBy splitStr toStrip
  where splitStr s =
          let
            stripLen = length toStrip
            splitLen = (length s) - stripLen
            (remaining, suffix) = splitAt splitLen s
          in
            (suffix, remaining)

top :: (Ord a) => Int -> [a] -> [a]
top n = take n . sortOn Down

bot :: (Ord a) => Int -> [a] -> [a]
bot n = take n . sort

ascii :: Char -> Int
ascii = Data.Char.ord

arrayFromNestedList :: [[a]] -> Array (Int, Int) a
arrayFromNestedList xs =
  let
    lowestIdx = (0, 0)

    highestListIdx ys = (length ys) - 1

    highestRowIdx = highestListIdx xs
    highestColIdx = maximum $ highestListIdx <$> xs
    highestIdx = (highestRowIdx, highestColIdx)

    withIndex = map (\(r, c, x) -> ((r, c), x)) $ zipSquareWithIndex xs
  in
    array (lowestIdx, highestIdx) withIndex

withinBounds :: (Data.Ix.Ix i) => Array i e -> i -> Bool
withinBounds arr = Data.Ix.inRange (Array.bounds arr)

findIndexesWhere :: (Data.Ix.Ix i) => (e -> Bool) -> Array i e -> [i]
findIndexesWhere pred = map fst . filter (pred . snd) . Array.assocs

flattenMaybe :: [Maybe a] -> [a]
flattenMaybe xs = xs >>= maybeToList

repeatUntil :: ((a, s) -> Bool) -> State s a -> State s a
repeatUntil cond st =
  do
    x <- st
    s <- get
    if cond (x, s)
      then return x
      else repeatUntil cond st

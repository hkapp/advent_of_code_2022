module Utils where

import Data.List(unfoldr)
import Data.Array.IArray(Array, array)
import Control.Monad.State(State, evalState)
import Control.Monad(join)
import Control.Exception(assert)

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

stripPrefix :: String -> String -> String
stripPrefix toStrip input =
  let
    stripLen = length toStrip
    (prefix, remaining) = splitAt stripLen input
  in
    assert (prefix == toStrip) remaining

stripSuffix :: String -> String -> String
stripSuffix toStrip input =
  let
    stripLen = length toStrip
    splitLen = (length input) - stripLen
    (remaining, suffix) = splitAt splitLen input
  in
    assert (suffix == toStrip) remaining

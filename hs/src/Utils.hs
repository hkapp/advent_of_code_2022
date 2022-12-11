module Utils where

import Data.Array.IArray(Array, array)
import Data.List(unfoldr, sort, sortOn)
import Data.Ord(Down(..))

import Control.Monad.State(State, evalState)
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

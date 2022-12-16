module Utils (module Utils, module Utils.State) where

import Utils.State

import Data.Array.IArray(Array, array)
import qualified Data.Array.IArray as Array
import qualified Data.Char
import Data.List(unfoldr, sort, sortOn, groupBy)
import Data.Ord(Down(..))
import qualified Data.Ix
import Data.Maybe(maybeToList)
import Data.Bifunctor(Bifunctor, bimap, second)
import qualified Data.Set as Set

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

splitSubSeq :: (Eq a) => [a] -> [a] -> [[a]]
splitSubSeq sep xs = reverse $ rec [] [] xs
  where
    rec res []   [] = res
    rec res curr [] = (reverse curr):res
    rec res curr ys =
      if (take (length sep) ys) == sep
       then rec ((reverse curr):res) [] (drop (length sep) ys)
       else rec res ((head ys):curr) (tail ys)

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

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr f
  where f [] = Nothing
        f xs = Just $ splitAt n xs

flatten :: (Monad m) => m (m a) -> m a
flatten = join

mflatten :: (Monad m) => m (m a) -> m a
mflatten = join

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

parserStrip :: String -> State String ()
parserStrip prefix = state (\s -> ((), stripPrefix prefix s))

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

intoPairs :: [a] -> [(a, a)]
intoPairs (x:y:zs) = (x, y):(intoPairs zs)
intoPairs []       = []

both :: (Bifunctor f) => (a -> b) -> f a a -> f b b
both f = bimap f f

toMaybe :: Bool -> a -> Maybe a
toMaybe True  x = Just x
toMaybe False _ = Nothing

fromSingleton :: [a] -> a
fromSingleton (x:[]) = x
fromSingleton xs     = error "Not a singleton list"

distinct :: (Ord a) => [a] -> [a]
distinct = Set.toList . Set.fromList

{- As opposed to groupBy, this is full group by.
   The position of the same items doesn't matter (which is why we need sort).
-}
groupByKey :: (Ord k) => (a -> k) -> [a] -> [(k, [a])]
groupByKey key xs =
  map (\ys -> (fst $ head ys, map snd ys)) $
    groupBy (\(k1, _) (k2, _) -> k1 == k2) $
      sortOn fst $
        map (\x -> (key x, x)) xs

-- groupByKey :: (Ord k) => (a -> k) -> [a] -> [(k, [a])]
-- groupByKey key xs =
  -- groupBy (\x y -> (key x) == (key y)) $ sortOn key xs

groupKeyPairs :: (Ord k) => [(k, a)] -> [(k, [a])]
groupKeyPairs = map (second $ map snd) . groupByKey fst

maxBy :: (Ord b) => (a -> b) -> a -> a -> a
maxBy f x y =
  if (f x) >= (f y)
    then x
    else y

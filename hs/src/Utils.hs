module Utils where

import Data.List(unfoldr)

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
zipWithIndex = z 0
  where z n (x:xs) = (n, x):(z (n+1) xs)
        z _ []     = []

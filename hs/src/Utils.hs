module Utils where

splitFirstSep :: (Eq a) => a -> [a] -> ([a], [a])
splitFirstSep elem xs =
  let (prefix, suffix) = span ((/=) elem) xs
  in (prefix, tail suffix)

{- A Priority Queue -}
module Utils.PQueue (
  {- Directly re-exported from Map -}
  Map.null,
  {- Everything else in this file -}
  module Utils.PQueue
) where

import Utils(groupKeyPairs)

import Data.Map (Map, (!))
import qualified Data.Map as Map

-- A given key can have more than one value
-- On pop, any value can be returned
type PQueue k v = Map k [v]

singleton :: k -> v -> PQueue k v
singleton k v = Map.singleton k [v]

popMax :: (Ord k) => PQueue k v -> ((k, v), PQueue k v)
popMax pq =
  let
    ((k, vs), mapDel) = Map.deleteFindMax pq

    newMap =
      case vs of
        (v:[]) -> mapDel
        _      -> Map.insert k (tail vs) mapDel
  in
    ((k, head vs), newMap)

fromList :: (Ord k) => [(k, v)] -> PQueue k v
fromList = Map.fromList . groupKeyPairs

pushAll :: (Ord k) => [(k, v)] -> PQueue k v -> PQueue k v
pushAll xs pq = Map.unionWith (++) (fromList xs) pq

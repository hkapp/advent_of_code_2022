module Utils.Queue where

import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq

type Queue a = Seq a

push :: a -> Queue a -> Queue a
push = flip (Seq.|>)

pop :: Queue a -> (a, Queue a)
pop (x:<|xs) = (x, xs)

null :: Queue a -> Bool
null = Seq.null

{- Popping will get the elements in the same order as in the original list -}
fromList :: [a] -> Queue a
fromList = Seq.fromList

pushAll :: Queue a -> [a] -> Queue a
pushAll queue newElems = queue >< (fromList newElems)

singleton :: a -> Queue a
singleton = Seq.singleton

-- The next element in the queue gets pushed to the end and returned outside
rotate :: Queue a -> (a, Queue a)
rotate q =
  let
    (elem, poppedQ) = pop q
  in
    (elem, push elem poppedQ)

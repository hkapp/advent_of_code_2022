module Utils.Queue (
  -- Re-exports from Data.Seq
  Seq.null,
  Seq.fromList,
  Seq.length,
  Seq.singleton,
  -- Everything in this file
  module Utils.Queue
)
where

import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq

type Queue a = Seq a

push :: a -> Queue a -> Queue a
push = flip (Seq.|>)

pop :: Queue a -> (a, Queue a)
pop (x:<|xs) = (x, xs)

pushAll :: Queue a -> [a] -> Queue a
pushAll queue newElems = queue >< (Seq.fromList newElems)

-- The next element in the queue gets pushed to the end and returned outside
rotate :: Queue a -> (a, Queue a)
rotate q =
  let
    (elem, poppedQ) = pop q
  in
    (elem, push elem poppedQ)

peek :: Queue a -> a
peek (x:<|xs) = x

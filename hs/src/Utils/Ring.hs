module Utils.Ring where

import Data.Array.IArray(Array, (!))

data Ring a = Ring Int (Array Int a)

next :: Ring a -> (a, Ring a)
next (Ring armPos buffer) =
  let
    value = buffer ! armPos
    newArmPos = (armPos + 1) `mod` nvalues
    nvalues = length buffer
  in
    (value, Ring newArmPos buffer)

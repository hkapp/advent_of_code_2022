module Utils.Ring where

import Utils(joinSep)

import Data.Array.IArray(Array, (!))
import qualified Data.Array.IArray as Array

data Ring a = Ring Int (Array Int a)

next :: Ring a -> (a, Ring a)
next (Ring armPos buffer) =
  let
    value = buffer ! armPos
    newArmPos = (armPos + 1) `mod` nvalues
    nvalues = length buffer
  in
    (value, Ring newArmPos buffer)

reset :: Ring a -> Ring a
reset (Ring armPos buffer) = Ring 0 buffer

instance Show a => Show (Ring a) where
  show (Ring armPos buffer) =
    "Ring (" ++
    showBeforeArm ++
    showAtArm ++
    showAfterArm ++
    ")"
    where
      showBeforeArm =
        let
          mainStr = showNoArm $ filter (\i -> i < armPos) $ Array.indices buffer
        in
          mainStr ++ (addSep mainStr)

      showAfterArm =
        let
          mainStr = showNoArm $ filter (\i -> i > armPos) $ Array.indices buffer
        in
          (addSep mainStr) ++ mainStr

      addSep s = if not $ null s then sep else ""
      sep = ", "

      showNoArm is = joinSep ", " $ map (\i -> show (buffer ! i)) is

      showAtArm = ">" ++ (show $ buffer ! armPos) ++ "<"

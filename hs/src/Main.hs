import System.Environment(getArgs)

import qualified Dec5
import qualified Dec6
import qualified Dec7
import qualified Dec8
import qualified Dec9
import qualified Dec10
import qualified Dec11
import qualified Dec12
import qualified Dec13
import qualified Dec14
import qualified Dec15

main :: IO ()
main =
  do
    args <- getArgs
    if null args
      then error "Usage: ./Main <day##>"
      else return ()

    let day = head args
    input <- inputFile day

    case day of
      "day5"  -> Dec5.run input
      "day6"  -> Dec6.run input
      "day7"  -> Dec7.run input
      "day8"  -> Dec8.run input
      "day9"  -> Dec9.run input
      "day10" -> Dec10.run input
      "day11" -> Dec11.run input
      "day12" -> Dec12.run input
      "day13" -> Dec13.run input
      "day14" -> Dec14.run input
      "day15" -> Dec15.run input
      other   -> error $ "Not implemented: " ++ other

inputFile :: String -> IO String
inputFile day = readFile filename
  where filename = "../../data/" ++ day ++ ".data.txt"

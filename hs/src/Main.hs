import System.Environment(getArgs)

import qualified Dec5
import qualified Dec6

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
      "day5" -> Dec5.run input
      "day6" -> Dec6.run input
      other  -> error $ "Not implemented: " ++ other

inputFile :: String -> IO String
inputFile day = readFile filename
  where filename = "../../data/" ++ day ++ ".data.txt"

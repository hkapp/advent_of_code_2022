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
    case day of
      "day5" -> Dec5.run
      "day6" -> Dec6.run
      other  -> error $ "Not implemented: " ++ other

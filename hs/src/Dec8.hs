module Dec8 (run) where

import qualified Dec8.FlatList    as FlatList
import qualified Dec8.ListOfList  as ListOfList
import qualified Dec8.ArrayOfList as ArrayOfList
import qualified Dec8.Array       as Array

import Data.Time.Clock(getCurrentTime, diffUTCTime)

run :: String -> IO ()
run = measureAll

measureAll :: String -> IO ()
measureAll filename =
  do
    measure "FlatList"    $ FlatList.run filename
    measure "ListOfList"  $ ListOfList.run filename
    measure "ArrayOfList" $ ArrayOfList.run filename
    measure "Array"       $ Array.run filename

measure :: String -> IO () -> IO ()
measure name compute =
  do
    putStrLn $ name
    let sep = take (length name) $ repeat '-'
    putStrLn sep
    before <- getCurrentTime
    compute
    after  <- getCurrentTime
    let duration = toMillis $ diffUTCTime after before
    putStrLn $ "Duration: " ++ show duration ++ "ms"
    putStrLn sep

toMillis dt = dt * 1000

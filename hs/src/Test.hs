module Test where

test :: (Eq a, Show a) => String -> a -> a -> IO ()
test message expected found =
  if expected == found
    then return ()
    else
      do
        putStrLn $ "[FAIL] " ++ message
        putStrLn $ "Expected: " ++ (show expected)
        putStrLn $ "Found:    " ++ (show found)

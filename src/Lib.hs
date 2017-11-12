module Lib
    ( someFunc
    ) where

import           System.Directory (getCurrentDirectory)



someFunc :: IO ()
someFunc = do
  currentDir <- getCurrentDirectory
  putStrLn currentDir
  let filePath = currentDir ++ "/harambee_dev/config.yaml"
  putStrLn filePath
  -- fileContents <- readFile filePath
  -- putStrLn fileContents

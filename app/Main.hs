module Main where

import           ProjectConfig (configFilePath, load)

main :: IO ()
main = do
  filePath <- configFilePath
  load filePath

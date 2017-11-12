module Main where

import           BasketCase.Config (configFilePath, load)

main :: IO ()
main = do
  filePath <- configFilePath
  load filePath

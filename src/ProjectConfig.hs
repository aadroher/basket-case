{-# LANGUAGE DeriveGeneric #-}
module ProjectConfig (
  load,
  configFilePath
) where

  import GHC.Generics
  import qualified Data.ByteString.Char8 as BS
  import Data.Yaml (decode, FromJSON)
  import System.Directory (getCurrentDirectory)

  data Config = Config
    { host  :: String
    , name  :: String
    , token :: String
    } deriving(Show, Generic)

  instance FromJSON Config

  configLocalFilePath = "/harambee_dev/config.yml"

  configFilePath :: IO String
  configFilePath = do
    currentDir <- getCurrentDirectory
    let filePath = currentDir ++ configLocalFilePath
    return filePath

  load :: String -> IO ()
  load filePath = do
    fileContents <- BS.readFile filePath
    let parsedContents = decode fileContents :: Maybe Config
    case parsedContents of
      Nothing -> error "Could not parse config file."
      Just config -> putStrLn $ show config

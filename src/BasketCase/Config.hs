{-# LANGUAGE DeriveGeneric #-}
module BasketCase.Config (
  Config(),
  load,
  configFilePath
) where

  import GHC.Generics
  import qualified Data.ByteString.Char8 as BS
  import Data.Yaml (FromJSON, decode)
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

  load :: String -> IO Config
  load filePath = do
    fileContents <- BS.readFile filePath
    let parsedContents = decode fileContents :: (Maybe Config)
    case parsedContents of
      Nothing -> do
        let errorMessage = "Could not parse config file in " ++ filePath ++ "."
        error errorMessage
      Just config -> do
        let successMessage = "Config in " ++ filePath ++ " loaded."
        putStrLn successMessage
        return config

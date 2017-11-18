{-# LANGUAGE DeriveGeneric #-}
module BasketCase.Config (
  Config (),
  loadConfig
) where

import           BasketCase.Serializable (loader)
import           Data.Aeson.Types        (camelTo2, defaultOptions,
                                          fieldLabelModifier, genericParseJSON)
import           Data.Yaml               (FromJSON (), parseJSON)
import           GHC.Generics

data Config = Config
  { host  :: String
  , name  :: String
  , token :: String
  } deriving(Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_'
  }

fileName :: String
fileName = "config.yaml" 

loadConfig :: String -> IO Config
loadConfig projectDirPath = loader filePath "Config"
  where filePath = projectDirPath ++ "/" ++ fileName

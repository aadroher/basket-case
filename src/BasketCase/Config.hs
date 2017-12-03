{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BasketCase.Config (
  Config (),
  loadConfig
) where

import           BasketCase.Serialization (LoadMethod (..), loader)
import           Data.Aeson.Types         (camelTo2, defaultOptions,
                                           fieldLabelModifier, genericParseJSON)
import           Data.Text                (Text ())
import qualified Data.Text.IO             as T
import           Data.Yaml                (FromJSON (), parseJSON)
import           GHC.Generics

data Config = Config
  { host  :: Text
  , name  :: Text
  , token :: Text
  } deriving(Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_'
  }

fileName :: String
fileName = "config.yaml"

loadConfig :: String -> IO Config
loadConfig projectDirPath = loader File filePath "Config"
  where filePath = projectDirPath ++ "/" ++ fileName

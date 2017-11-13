{-# LANGUAGE DeriveGeneric #-}
module BasketCase.Team (
  Team(),
  load
) where

import           BasketCase.Config       (projectFolder)
import           BasketCase.Serializable (Serializable (load), loader)
import           Data.Aeson.Types        (camelTo2, defaultOptions,
                                          fieldLabelModifier, genericParseJSON)
import qualified Data.ByteString.Char8   as BS
import           Data.Yaml               (FromJSON (), decode, parseJSON)
import           GHC.Generics
import           System.Directory        (getCurrentDirectory)

data Team = Team
  { id                      :: Int
  , displayName             :: String
  , excludeFromDistribution :: Bool
  , includeInDistribution   :: Bool
  , teamScreenEnabled       :: Bool
  } deriving(Show, Generic)

instance FromJSON Team where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_'
  }

teamsLocalFilePath = projectFolder ++ "/teams"

instance Serializable Team where
  load filePath = loader filePath "Team"

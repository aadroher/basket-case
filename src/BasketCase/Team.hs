{-# LANGUAGE DeriveGeneric #-}
module BasketCase.Team (
  Team(),
  loadTeams
) where

import           BasketCase.Serialization (LoadMethod (..), loader)
import           Control.Monad            (filterM, mapM)
import           Data.Aeson.Types         (camelTo2, defaultOptions,
                                           fieldLabelModifier, genericParseJSON)
import           Data.Text                (Text ())
import qualified Data.Text.IO             as T
import           Data.Yaml                (FromJSON (), parseJSON)
import           GHC.Generics
import           System.Directory
import           System.FilePath

data Team = Team
  { displayName             :: Text
  , excludeFromDistribution :: Bool
  , includeInDistribution   :: Bool
  , teamScreenEnabled       :: Bool
  } deriving(Show, Generic)

instance FromJSON Team where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_'
  }

dirName :: String
dirName = "teams"

teamsDirPath :: FilePath -> FilePath
teamsDirPath projectDirPath = projectDirPath </> dirName

teamDirPath :: FilePath -> FilePath -> FilePath
teamDirPath projectDirPath teamDirName =
  teamsDirPath projectDirPath </> teamDirName

indexFilePath :: FilePath -> FilePath
indexFilePath filePath = filePath </> "index.yaml"

teamDirPaths :: FilePath -> IO [FilePath]
teamDirPaths projectDirPath = do
  contentNames <- listDirectory $ teamsDirPath projectDirPath
  let contentPaths = map (teamDirPath projectDirPath) contentNames
  filterM doesDirectoryExist contentPaths

loadTeam :: FilePath -> IO Team
loadTeam filePath = loader File filePath "Team"

-- Public functions
loadTeams :: FilePath -> IO [Team]
loadTeams projectDirPath = do
  dirPaths <- teamDirPaths projectDirPath
  mapM (loadTeam . indexFilePath) dirPaths

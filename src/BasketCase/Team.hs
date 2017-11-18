{-# LANGUAGE DeriveGeneric #-}
module BasketCase.Team (
  Team(),
  loadTeams
) where

import           BasketCase.Serializable (loader)
import           Data.Aeson.Types        (camelTo2, defaultOptions,
                                          fieldLabelModifier, genericParseJSON)
import           Data.Yaml               (FromJSON (), parseJSON)
import           GHC.Generics
import           System.FilePath
import           System.Directory
import Control.Monad (mapM, filterM)

data Team = Team
  { displayName             :: String
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
loadTeam filePath = loader filePath "Team"

loadTeams :: FilePath -> IO [Team]
loadTeams projectDirPath = do
  dirPaths <- teamDirPaths projectDirPath
  mapM loadTeam $ map indexFilePath dirPaths
  
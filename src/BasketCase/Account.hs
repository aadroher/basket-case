{-# LANGUAGE DeriveGeneric #-}
module BasketCase.Account where

import           BasketCase.Config (Config (),loadConfig)
import           BasketCase.Team   (Team (), loadTeams)
import           GHC.Generics

data Account = Account
  { config :: Config
  , teams  :: [Team]
  } deriving (Show, Generic)

load :: String -> IO Account
load projectDirectoryPath = do
  c <- loadConfig projectDirectoryPath
  ts <- loadTeams projectDirectoryPath
  return Account {config = c, teams = ts} 
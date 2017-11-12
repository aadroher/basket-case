{-# LANGUAGE DeriveGeneric #-}

module BasketCase.Team (
  Team()
) where

import qualified Data.ByteString.Char8 as BS
import           Data.Yaml             (FromJSON, decode)
import           GHC.Generics
import           System.Directory      (getCurrentDirectory)

data Team = Team
  { id :: Int
  }

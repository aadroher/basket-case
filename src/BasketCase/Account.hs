{-# LANGUAGE DeriveGeneric #-}
module BasketCase.Account where

import           BasketCase.Config (Config ())
import           BasketCase.Team   (Team ())


data Account = Account
  { config :: Config
  , teams  :: [Team]
  }

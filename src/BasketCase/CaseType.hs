{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BasketCase.CaseType where

import           Data.Aeson
import           Data.Aeson.Types     (camelTo2, fieldLabelModifier)
import qualified Data.ByteString.Lazy as B
import           Data.Text            (Text ())
-- import qualified Data.Text            as T
import           GHC.Generics
import           Network.HTTP.Simple
import           Network.URL
import           Text.Pretty.Simple   (pPrint)

-- data CaseType a = Case a | Person a | Organization a

data CaseTypeClass = Case | Person | Organization


data CaseTypes = CaseTypes
  { caseTypes :: [CaseType]
  } deriving (Show, Generic)

instance FromJSON CaseTypes where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_'
  }

data CaseType = CaseType
  { name                             :: Text
  , displayName                      :: Text
  , code                             :: Text
  , description                      :: Text
  , excludeContactsFromConversations :: Bool
  } deriving (Show, Generic)

instance FromJSON CaseType where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_'
  }

fetchCaseTypes :: IO ()
fetchCaseTypes = do
  -- request <- parseRequest "GET https://login.caseblocks.com/case_blocks/case_types.json?auth_token=dwcAYngdQDzZEU58Ym26"
  let request = setRequestPath "/case_blocks/case_types.json"
              $ setRequestMethod "GET"
              $ setRequestSecure True
              $ setRequestPort 443
              $ setRequestHost "login.caseblocks.com"
              $ setRequestQueryString [("auth_token", Just "dwcAYngdQDzZEU58Ym26")]
              $ defaultRequest
  response <- httpJSON request
  -- let responseBody = getResponseBody response
  let parsedContents = (getResponseBody response :: CaseTypes)
  pPrint parsedContents
  -- case parsedContents of
  --   Nothing        -> error "Could not parse contents"
  --   Just caseTypes -> print (caseTypes :: CaseTypes)

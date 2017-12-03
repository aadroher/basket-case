{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module BasketCase.CaseType where

import           BasketCase.Config   (Config)
import qualified BasketCase.Reader   as R
import           Data.Aeson
import           Data.Aeson.Types    (camelTo2, fieldLabelModifier)
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Set            (Set)
import           Data.Text           (Text ())
import qualified Data.Yaml           as Y
import           GHC.Generics
import           Network.HTTP.Simple
import           Network.URL
import           Text.Pretty.Simple  (pPrint, pShow)

data CaseTypeClass = Case | Person | Organization

urlFragment :: CaseTypeClass -> BS.ByteString
urlFragment ctc = case ctc of
  Case         -> "case_types"
  Person       -> "people_types"
  Organization -> "organization_types"

data CaseTypes = CaseTypes
  { caseTypes         :: Maybe (Set CaseType)
  , peopleTypes       :: Maybe (Set CaseType)
  , organizationTypes :: Maybe (Set CaseType)
  } deriving (Show, Generic)

instance FromJSON CaseTypes where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_'
  }

urlPath :: CaseTypeClass -> ByteString
urlPath ctc = BS.concat
  [ "/case_blocks/"
  , urlFragment ctc
  , ".json"
  ]

data CaseType = CaseType
  { name                             :: Text
  , displayName                      :: Text
  , code                             :: Text
  , description                      :: Text
  , excludeContactsFromConversations :: Bool
  , schema                           :: Value
  } deriving (Eq, Generic, ToJSON)

instance Show CaseType where
  show ct = show $ Y.encode ct
    -- ( name ct
    -- , displayName ct
    -- , code ct
    -- , Y.encode $ schema ct
    -- )

instance Ord CaseType where
  compare a b = compare (name a) (name b)

instance FromJSON CaseType where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_'
  }

fetchCaseTypes :: Config -> CaseTypeClass -> IO CaseTypes
fetchCaseTypes c ctc = do
  request <- R.request c R.GET (urlPath ctc) R.JSON
  response <- httpJSON request
  return (getResponseBody response :: CaseTypes)
  -- return responseBody
  -- pPrint parsedContents
  -- case parsedContents of
  --   Nothing        -> error "Could not parse contents"
  --   Just caseTypes -> print (caseTypes :: CaseTypes)

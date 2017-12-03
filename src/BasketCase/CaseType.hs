{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module BasketCase.CaseType where

import           BasketCase.Config   (Config)
import qualified BasketCase.Reader   as R
import           Data.Aeson          (Value, defaultOptions)
import           Data.Aeson.TH       (deriveJSON)
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

data CaseType = CaseType
    { name                             :: Text
    , displayName                      :: Text
    , code                             :: Text
    , description                      :: Text
    , excludeContactsFromConversations :: Bool
    , schema                           :: Value
    } deriving (Eq, Show, Generic)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''CaseType

instance Ord CaseType where
    compare a b = compare (name a) (name b)

data CaseTypes = CaseTypes
  { caseTypes         :: Maybe (Set CaseType)
  , peopleTypes       :: Maybe (Set CaseType)
  , organizationTypes :: Maybe (Set CaseType)
  } deriving (Eq, Show, Generic)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''CaseTypes

urlPath :: CaseTypeClass -> ByteString
urlPath ctc = BS.concat
  [ "/case_blocks/"
  , urlFragment ctc
  , ".json"
  ]

fetchCaseTypes :: Config -> CaseTypeClass -> IO CaseTypes
fetchCaseTypes c ctc = do
  request <- R.request c R.GET (urlPath ctc) R.JSON
  response <- httpJSON request
  return (getResponseBody response :: CaseTypes)

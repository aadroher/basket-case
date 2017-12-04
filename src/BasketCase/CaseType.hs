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
import           Data.Set            (Set, empty, fromList, union)
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
    -- , schema                           :: [Maybe Value]
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

getCaseTypes :: CaseTypes -> Set CaseType
getCaseTypes ct =
  let attrs = [caseTypes, peopleTypes, organizationTypes]
      r attr caseTypesSet =
        case attr ct of
          Nothing          -> caseTypesSet
          Just caseTypeSet -> caseTypeSet `union` caseTypesSet
  in foldr r empty attrs

flattenCaseTypes :: [Set CaseType] -> Set CaseType
flattenCaseTypes = foldr union empty

fetchClassCaseTypes :: Config -> CaseTypeClass -> IO CaseTypes
fetchClassCaseTypes c ctc = do
  request <- R.request c R.GET (urlPath ctc) R.JSON
  response <- httpJSON request
  return (getResponseBody response :: CaseTypes)

fetchCaseTypes :: Config -> IO (Set CaseType)
fetchCaseTypes c = do
  ct <- fetchClassCaseTypes c Case
  pt <- fetchClassCaseTypes c Person
  ot <- fetchClassCaseTypes c Organization
  let caseTypesSetList = map getCaseTypes [ct, pt, ot]
  return $ flattenCaseTypes caseTypesSetList

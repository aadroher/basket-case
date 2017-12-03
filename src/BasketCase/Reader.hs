{-# LANGUAGE OverloadedStrings #-}

module BasketCase.Reader
  ( request, HTTPVerb(..), ContentType(..)
  ) where

import           BasketCase.Config   (Config (..), authToken)
import           Data.Aeson          (FromJSON)
import           Data.ByteString     (ByteString)
import           Data.Text           (Text ())
import           Network.HTTP.Simple

data HTTPVerb = GET | POST | PUT | DELETE

httpVerbBS :: HTTPVerb -> ByteString
httpVerbBS v = case v of
  GET    -> "GET"
  POST   -> "POST"
  PUT    -> "PUT"
  DELETE -> "DELETE"

type URLPath = ByteString

data ContentType = JSON

request :: Config -> HTTPVerb -> URLPath -> ContentType -> IO Request
request c v p JSON = do
  partialRequest <- parseRequest (host c)
  let r = (setRequestMethod . httpVerbBS) v
            $ setRequestPath p
            $ setRequestQueryString
              [("auth_token", Just $ authToken c)]
            $ partialRequest
  return r

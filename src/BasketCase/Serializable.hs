{-# LANGUAGE DeriveGeneric #-}
module BasketCase.Serializable (
  Serializable (load),
  loader
) where

import           Data.Aeson.Types      (GFromJSON (), Parser (), Value (),
                                        camelTo2, defaultOptions,
                                        fieldLabelModifier, genericParseJSON)
import qualified Data.ByteString.Char8 as BS
import           Data.Yaml             (FromJSON (), decode, parseJSON)
import           GHC.Generics          (Generic ())

loader :: FromJSON a => String -> String -> IO a
loader filePath typeName = do
  putStrLn $ "Loading file: " ++ filePath
  fileContents <- BS.readFile filePath
  let parsedContents = decode fileContents :: FromJSON a => (Maybe a)
  case parsedContents of
    Nothing -> do
      let errorMessage = "Could not parse " ++ typeName ++ "file in " ++ filePath ++ "."
      error errorMessage
    Just team -> do
      let successMessage = typeName ++ " data in " ++ filePath ++ " loaded."
      putStrLn successMessage
      return team

class (FromJSON a, Generic a) => Serializable a where
  load :: String -> IO a

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           BasketCase.Account     (Account, load)
import           Path                   (Abs, Dir, Path, fromAbsDir,
                                         parseAbsDir, toFilePath)
import           System.Console.CmdArgs
import           System.Directory       (getCurrentDirectory,
                                         setCurrentDirectory)
import           System.Environment     (getArgs)

-- import BasketCase.Team (loadTeams)

data Options = Options
  { project :: FilePath
  } deriving (Show, Data, Typeable)

defaultOptions = Options
  { project = "./"
      &= help "Path to the project directory"
      &= typDir
      &= opt "."
  } &= program "basketcase"

getProjectDirectory :: IO (Path Abs Dir)
getProjectDirectory = do
  options <- cmdArgs defaultOptions
  let projectDirPath = project options
  setCurrentDirectory projectDirPath
  currentDirectory <- getCurrentDirectory
  let msg = "Working directory set to " ++ currentDirectory
  putStrLn msg
  parseAbsDir currentDirectory

loadAccountData :: FilePath -> IO Account
loadAccountData dirFilePath = do
  let message = "Loading data from: " ++ dirFilePath
  putStrLn message
  load dirFilePath

main :: IO ()
main = do
  projectAbsDirPath <- getProjectDirectory
  account <- loadAccountData (fromAbsDir projectAbsDirPath)
  print account

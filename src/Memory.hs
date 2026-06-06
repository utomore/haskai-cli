{-# LANGUAGE OverloadedStrings #-}

module Memory
  ( ensureConfigDirExists
  , loadMemories
  , saveMemories
  , loadSessions
  , saveSessions
  ) where

import Types (Session)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

-- Ensure that the directory containing the file path exists
ensureConfigDirExists :: FilePath -> IO ()
ensureConfigDirExists path = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir

-- Load memories from JSON file. If the file doesn't exist or is invalid, return empty list.
loadMemories :: FilePath -> IO [Text]
loadMemories path = do
  exists <- doesFileExist path
  if not exists
    then return []
    else do
      contentData <- BL.readFile path
      case Aeson.decode contentData of
        Just mems -> return mems
        Nothing   -> return []

-- Save memories to JSON file
saveMemories :: FilePath -> [Text] -> IO ()
saveMemories path mems = do
  ensureConfigDirExists path
  BL.writeFile path (Aeson.encode mems)

-- Load sessions from JSON file. If the file doesn't exist or is invalid, return empty list.
loadSessions :: FilePath -> IO [Session]
loadSessions path = do
  exists <- doesFileExist path
  if not exists
    then return []
    else do
      contentData <- BL.readFile path
      case Aeson.decode contentData of
        Just ss -> return ss
        Nothing -> return []

-- Save sessions to JSON file
saveSessions :: FilePath -> [Session] -> IO ()
saveSessions path ss = do
  ensureConfigDirExists path
  BL.writeFile path (Aeson.encode ss)

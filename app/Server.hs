{-# LANGUAGE OverloadedStrings #-}

-- | JSON stdio server for the Tauri desktop frontend.
--
-- Protocol (newline-delimited JSON):
--   stdin  <- {"message": "user text"}
--   stdout -> {"response": "ai text"}    on success
--   stdout -> {"error": "message"}       on failure

module Main where

import AppConfig (loadDevConfig)
import Memory (loadMemories)
import Ollama (fetchChatResponse)
import Types

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>))

newtype Request = Request { reqMessage :: Text }

instance Aeson.FromJSON Request where
  parseJSON = Aeson.withObject "Request" $ \v ->
    Request <$> v Aeson..: "message"

writeJSON :: Aeson.Value -> IO ()
writeJSON v = TIO.putStrLn (TE.decodeUtf8 (BL.toStrict (Aeson.encode v)))

writeResponse :: Text -> IO ()
writeResponse t = writeJSON (Aeson.object [("response", Aeson.toJSON t)])

writeError :: String -> IO ()
writeError e = writeJSON (Aeson.object [("error", Aeson.toJSON e)])

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering

  appDir <- getAppUserDataDirectory "haskai"
  let memPath  = appDir </> "memory.json"
      sessPath = appDir </> "sessions.json"

  config   <- loadDevConfig "dev.config" memPath sessPath
  memories <- loadMemories memPath
  histRef  <- newIORef ([] :: [Message])

  loop config (fallbackModel config) memories histRef

loop :: Config -> ModelId -> [Text] -> IORef [Message] -> IO ()
loop config model memories histRef = do
  line <- TIO.getLine
  case Aeson.decodeStrict (TE.encodeUtf8 line) of
    Nothing  -> writeError "Invalid JSON" >> loop config model memories histRef
    Just req -> do
      hist <- readIORef histRef
      let sysMsg   = systemMessage memories
          userMsg  = Message "user" (reqMessage req)
          fullMsgs = sysMsg : hist ++ [userMsg]
      result <- fetchChatResponse config model fullMsgs
      case result of
        Left err -> writeError err
        Right reply -> do
          writeIORef histRef (hist ++ [userMsg, Message "assistant" reply])
          writeResponse reply
      loop config model memories histRef

systemMessage :: [Text] -> Message
systemMessage mems =
  let memBlock = if null mems then ""
        else "\n\nLong-term memories:\n"
          <> T.intercalate "\n" (map ("- " <>) mems)
  in Message "system" ("You are Haskai, a helpful AI assistant." <> memBlock)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ollama
  ( fetchModels
  , streamChat
  , splitBytes
  , ModelItem(..)
  , ModelsResponse(..)
  , ChoiceDelta(..)
  , ChoiceItem(..)
  , ChatCompletionChunk(..)
  ) where

import Types
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), withObject)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word (Word8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Status (statusCode)
import Control.Exception (try, SomeException, finally)
import System.IO (hFlush, stdout)
import Data.Text.Encoding.Error (lenientDecode)
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..), setCursorPosition)
import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Data.Maybe (fromMaybe)

-- ---------------------------------------------------------------------------
-- /v1/models response types
-- ---------------------------------------------------------------------------

data ModelItem = ModelItem
  { modelId :: String
  } deriving (Show, Eq)

instance FromJSON ModelItem where
  parseJSON = withObject "ModelItem" $ \v ->
    ModelItem <$> v .: "id"

data ModelsResponse = ModelsResponse
  { modelsList :: [ModelItem]
  } deriving (Show, Eq)

instance FromJSON ModelsResponse where
  parseJSON = withObject "ModelsResponse" $ \v ->
    ModelsResponse <$> v .: "data"

-- | Fetch available model IDs.
-- Returns an empty list for remote backends (model names entered manually).
fetchModels :: Config -> IO [String]
fetchModels config = case modelBackend config of
  RemoteOpenAI _ _ -> return []
  LocalOllama baseUrl -> do
    manager <- newManager defaultManagerSettings
    let url = baseUrl ++ "/v1/models"
    result <- try $ do
      req <- parseRequest url
      resp <- httpLbs (req { method = "GET" }) manager
      case Aeson.decode (responseBody resp) of
        Just (ModelsResponse items) -> return (map modelId items)
        Nothing                     -> return []
    case result of
      Left (_ :: SomeException) -> return []
      Right models              -> return models

-- ---------------------------------------------------------------------------
-- Chat request / SSE response types
-- ---------------------------------------------------------------------------

data ChatRequest = ChatRequest
  { reqModel    :: String
  , reqMessages :: [Message]
  , reqStream   :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ChatRequest where
  toJSON (ChatRequest m ms s) = Aeson.object
    [ "model"    Aeson..= m
    , "messages" Aeson..= ms
    , "stream"   Aeson..= s
    ]

data ChoiceDelta = ChoiceDelta
  { deltaContent   :: Maybe Text
  , deltaReasoning :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON ChoiceDelta where
  parseJSON = withObject "ChoiceDelta" $ \v ->
    ChoiceDelta <$> v .:? "content" <*> v .:? "reasoning"

data ChoiceItem = ChoiceItem
  { choiceDelta :: ChoiceDelta
  } deriving (Show, Eq)

instance FromJSON ChoiceItem where
  parseJSON = withObject "ChoiceItem" $ \v ->
    ChoiceItem <$> v .: "delta"

data ChatCompletionChunk = ChatCompletionChunk
  { choices :: [ChoiceItem]
  } deriving (Show, Eq)

instance FromJSON ChatCompletionChunk where
  parseJSON = withObject "ChatCompletionChunk" $ \v ->
    ChatCompletionChunk <$> v .: "choices"

-- ---------------------------------------------------------------------------
-- Spinner
-- ---------------------------------------------------------------------------

startSpinner :: Int -> Int -> Int -> IO ThreadId
startSpinner rows promptCol delayMicros = forkIO spinnerLoop
  where
    frames :: [String]
    frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
    spinnerLoop = mapM_ (\f -> do
        putStr "\ESC[s"
        setCursorPosition (rows - 1) promptCol
        putStr "\ESC[K"
        setSGR [SetColor Foreground Vivid Yellow]
        putStr (f ++ " Thinking...")
        setSGR [Reset]
        putStr "\ESC[u"
        hFlush stdout
        threadDelay delayMicros
        ) (cycle frames)

stopSpinner :: Int -> Int -> ThreadId -> IO ()
stopSpinner rows promptCol tid = do
  killThread tid
  putStr "\ESC[s"
  setCursorPosition (rows - 1) promptCol
  putStr "\ESC[K"
  putStr "\ESC[u"
  hFlush stdout

-- ---------------------------------------------------------------------------
-- Stream state
-- ---------------------------------------------------------------------------

data StreamState = StateInit | StateReasoning | StateContent deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- streamChat
-- ---------------------------------------------------------------------------

-- | Build the HTTP manager and auth headers from the backend config.
backendManagerAndHeaders :: ModelBackend -> IO (Manager, [Header])
backendManagerAndHeaders (LocalOllama _) = do
  mgr <- newManager defaultManagerSettings
  return (mgr, [])
backendManagerAndHeaders (RemoteOpenAI _ apiKey) = do
  mgr <- newManager tlsManagerSettings
  let authHeader = ("Authorization", BC.pack ("Bearer " ++ apiKey))
  return (mgr, [authHeader])

-- | Base chat-completions URL for a backend.
backendChatUrl :: ModelBackend -> String
backendChatUrl (LocalOllama baseUrl)      = baseUrl ++ "/v1/chat/completions"
backendChatUrl (RemoteOpenAI baseUrl _)   = baseUrl ++ "/chat/completions"

streamChat :: Config -> ModelId -> [Message] -> Int -> Int -> IO (Either AppError Text)
streamChat config (ModelId modelStr) msgs rows promptCol = do
  let backend = modelBackend config
      url     = backendChatUrl backend
      timeoutUs = httpTimeoutMicros config
      spinnerUs = spinnerDelayMicros config

  (manager, extraHeaders) <- backendManagerAndHeaders backend
  spinnerTid <- startSpinner rows promptCol spinnerUs

  let action = do
        initialRequest <- parseRequest url
        let reqBody = ChatRequest modelStr msgs True
            request = initialRequest
              { method         = "POST"
              , requestBody    = RequestBodyLBS (Aeson.encode reqBody)
              , requestHeaders = [("Content-Type", "application/json")] ++ extraHeaders
              , responseTimeout = responseTimeoutMicro timeoutUs
              }
        withResponse request manager $ \response -> do
          let sc = statusCode (responseStatus response)
          if sc /= 200
            then return (Left $ HttpStatusError sc)
            else do
              stopSpinner rows promptCol spinnerTid
              setSGR [SetColor Foreground Vivid Cyan]
              putStr "Assistant: "
              setSGR [Reset]
              hFlush stdout
              accText <- processStream (responseBody response) B.empty StateInit T.empty
              return (Right accText)

  result <- try action `finally` stopSpinner rows promptCol spinnerTid
  case result of
    Left (err :: SomeException) -> return (Left $ NetworkError (show err))
    Right val                   -> return val

-- ---------------------------------------------------------------------------
-- Stream processing
-- ---------------------------------------------------------------------------

processStream :: BodyReader -> B.ByteString -> StreamState -> Text -> IO Text
processStream reader buffer state acc = do
  chunk <- reader
  if B.null chunk
    then do
      (_, contentText) <- processFinalBuffer state buffer
      return (acc <> contentText)
    else do
      let newBuffer = buffer <> chunk
      (leftover, nextState, _, contentText) <- processLines state newBuffer
      processStream reader leftover nextState (acc <> contentText)

processLines :: StreamState -> B.ByteString -> IO (B.ByteString, StreamState, Text, Text)
processLines state bs = do
  let linesList = splitBytes 10 bs
  case linesList of
    []        -> return (B.empty, state, T.empty, T.empty)
    [lastLine] -> return (lastLine, state, T.empty, T.empty)
    allLines  -> do
      let foldLines currState [] = return (currState, T.empty, T.empty)
          foldLines currState (l:ls) = do
            (s1, r1, c1) <- processLine currState l
            (s2, r2, c2) <- foldLines s1 ls
            return (s2, r1 <> r2, c1 <> c2)
      (nextState, rText, cText) <- foldLines state (init allLines)
      return (last allLines, nextState, rText, cText)

processFinalBuffer :: StreamState -> B.ByteString -> IO (StreamState, Text)
processFinalBuffer state bs = do
  let linesList = splitBytes 10 bs
  case linesList of
    [] -> return (state, T.empty)
    allLines -> do
      let foldLines currState [] = return (currState, T.empty)
          foldLines currState (l:ls) = do
            (s1, _, c1) <- processLine currState l
            (s2, c2)    <- foldLines s1 ls
            return (s2, c1 <> c2)
      foldLines state allLines

processLine :: StreamState -> B.ByteString -> IO (StreamState, Text, Text)
processLine state lineBytes = do
  let lineText = T.strip (TE.decodeUtf8With lenientDecode lineBytes)
  if T.null lineText
    then return (state, T.empty, T.empty)
    else if T.isPrefixOf "data: " lineText
      then case T.stripPrefix "data: " lineText of
        Just "[DONE]"  -> return (state, T.empty, T.empty)
        Just jsonText  ->
          case Aeson.decode (BL.fromStrict (TE.encodeUtf8 jsonText)) of
            Just chunk -> do
              let deltas = map choiceDelta (choices chunk)
              let processDelta s delta = do
                    let rText = fromMaybe T.empty (deltaReasoning delta)
                        cText = fromMaybe T.empty (deltaContent delta)
                    if not (T.null rText)
                      then do
                        s' <- case s of
                          StateInit -> do
                            setSGR [SetColor Foreground Dull White]
                            putStrLn "\n[Thinking]"
                            setSGR [Reset]
                            hFlush stdout
                            return StateReasoning
                          _ -> return s
                        setSGR [SetColor Foreground Dull White]
                        putStr (T.unpack rText)
                        setSGR [Reset]
                        hFlush stdout
                        return (s', rText, T.empty)
                      else if not (T.null cText)
                        then do
                          s' <- case s of
                            StateReasoning -> do
                              putStrLn ""
                              hFlush stdout
                              return StateContent
                            StateInit    -> return StateContent
                            StateContent -> return s
                          putStr (T.unpack cText)
                          hFlush stdout
                          return (s', T.empty, cText)
                        else return (s, T.empty, T.empty)
              let foldDeltas currState [] = return (currState, T.empty, T.empty)
                  foldDeltas currState (d:ds) = do
                    (s1, r1, c1) <- processDelta currState d
                    (s2, r2, c2) <- foldDeltas s1 ds
                    return (s2, r1 <> r2, c1 <> c2)
              foldDeltas state deltas
            Nothing -> return (state, T.empty, T.empty)
        Nothing -> return (state, T.empty, T.empty)
      else return (state, T.empty, T.empty)

-- ---------------------------------------------------------------------------
-- Utility
-- ---------------------------------------------------------------------------

splitBytes :: Word8 -> B.ByteString -> [B.ByteString]
splitBytes w bs
  | B.null bs = []
  | otherwise = go bs
  where
    go xs =
      let (prefix, suffix) = B.break (== w) xs
      in if B.null suffix
           then [prefix]
           else prefix : go (B.drop 1 suffix)

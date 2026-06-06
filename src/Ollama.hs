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
import Data.Word (Word8)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Control.Exception (try, SomeException, finally)
import System.IO (hFlush, stdout)
import Data.Text.Encoding.Error (lenientDecode)
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..), setCursorPosition)
import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Data.Maybe (fromMaybe)

-- Data structures for parsing /v1/models
data ModelItem = ModelItem
  { modelId :: String
  } deriving (Show, Eq)

instance FromJSON ModelItem where
  parseJSON = withObject "ModelItem" $ \v -> do
    mid <- v .: "id"
    return $ ModelItem mid

data ModelsResponse = ModelsResponse
  { modelsList :: [ModelItem]
  } deriving (Show, Eq)

instance FromJSON ModelsResponse where
  parseJSON = withObject "ModelsResponse" $ \v -> do
    items <- v .: "data"
    return $ ModelsResponse items

-- Fetch available models from Ollama /v1/models
fetchModels :: String -> IO [String]
fetchModels baseUrl = do
  manager <- newManager defaultManagerSettings
  let url = baseUrl ++ "/v1/models"
  result <- try $ do
    initialRequest <- parseRequest url
    let request = initialRequest { method = "GET" }
    response <- httpLbs request manager
    case Aeson.decode (responseBody response) of
      Just (ModelsResponse items) -> return (map modelId items)
      Nothing -> return []
  case result of
    Left (_ :: SomeException) -> return []
    Right models -> return models

-- Data structures for chat request
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

-- Data structures for parsing SSE chunk JSON
data ChoiceDelta = ChoiceDelta
  { deltaContent   :: Maybe Text
  , deltaReasoning :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON ChoiceDelta where
  parseJSON = withObject "ChoiceDelta" $ \v -> do
    c <- v .:? "content"
    r <- v .:? "reasoning"
    return $ ChoiceDelta c r

data ChoiceItem = ChoiceItem
  { choiceDelta :: ChoiceDelta
  } deriving (Show, Eq)

instance FromJSON ChoiceItem where
  parseJSON = withObject "ChoiceItem" $ \v -> do
    d <- v .: "delta"
    return $ ChoiceItem d

data ChatCompletionChunk = ChatCompletionChunk
  { choices :: [ChoiceItem]
  } deriving (Show, Eq)

instance FromJSON ChatCompletionChunk where
  parseJSON = withObject "ChatCompletionChunk" $ \v -> do
    c <- v .: "choices"
    return $ ChatCompletionChunk c

-- Stream chat completion and print chunks in real-time
-- Start a background thread to display a thinking spinner on the prompt line
startSpinner :: Int -> Int -> IO ThreadId
startSpinner rows promptCol = forkIO spinnerLoop
  where
    frames :: [String]
    frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
    spinnerLoop :: IO ()
    spinnerLoop = do
      mapM_ (\f -> do
        putStr "\ESC[s"                      -- Save cursor position
        setCursorPosition (rows - 1) promptCol -- Move cursor to prompt line after prompt
        putStr "\ESC[K"                      -- Clear from cursor to end of line
        setSGR [SetColor Foreground Vivid Yellow]
        putStr (f ++ " Thinking...")
        setSGR [Reset]
        putStr "\ESC[u"                      -- Restore cursor position
        hFlush stdout
        threadDelay 80000 -- 80ms delay
        ) (cycle frames)

-- Stop the thinking spinner and clear its line
stopSpinner :: Int -> Int -> ThreadId -> IO ()
stopSpinner rows promptCol tid = do
  killThread tid
  putStr "\ESC[s"                      -- Save cursor position
  setCursorPosition (rows - 1) promptCol -- Move cursor to prompt line after prompt
  putStr "\ESC[K"                      -- Clear from cursor to end of line
  putStr "\ESC[u"                      -- Restore cursor position
  hFlush stdout

-- Stream state tracking
data StreamState = StateInit | StateReasoning | StateContent deriving (Show, Eq)

-- Stream chat completion and print chunks in real-time
streamChat :: Config -> String -> [Message] -> Int -> Int -> IO (Either String Text)
streamChat config model msgs rows promptCol = do
  manager <- newManager defaultManagerSettings
  let url = ollamaBaseUrl config ++ "/v1/chat/completions"
  
  spinnerTid <- startSpinner rows promptCol
  
  let action = do
        initialRequest <- parseRequest url
        let reqBody = ChatRequest model msgs True
        let request = initialRequest
              { method = "POST"
              , requestBody = RequestBodyLBS (Aeson.encode reqBody)
              , requestHeaders = [("Content-Type", "application/json")]
              , responseTimeout = responseTimeoutMicro 120000000 -- 120 seconds timeout for model loading
              }
        withResponse request manager $ \response -> do
          let statusCodeVal = statusCode (responseStatus response)
          if statusCodeVal /= 200
            then return (Left $ "HTTP error code: " ++ show statusCodeVal)
            else do
              stopSpinner rows promptCol spinnerTid
              
              setSGR [SetColor Foreground Vivid Cyan]
              putStr "Assistant: "
              setSGR [Reset]
              hFlush stdout
              
              accumulatedText <- processStream (responseBody response) B.empty StateInit T.empty
              return (Right accumulatedText)

  result <- try action `finally` stopSpinner rows promptCol spinnerTid
  case result of
    Left (err :: SomeException) -> return (Left $ show err)
    Right val -> return val

-- Recursive stream reader
processStream :: BodyReader -> B.ByteString -> StreamState -> Text -> IO Text
processStream reader buffer state accumulatedContent = do
  chunk <- reader
  if B.null chunk
    then do
      (_, contentText) <- processFinalBuffer state buffer
      return (accumulatedContent <> contentText)
    else do
      let newBuffer = buffer <> chunk
      (leftover, nextState, _, contentText) <- processLines state newBuffer
      processStream reader leftover nextState (accumulatedContent <> contentText)

-- Process buffer lines
processLines :: StreamState -> B.ByteString -> IO (B.ByteString, StreamState, Text, Text)
processLines state bs = do
  let linesList = splitBytes 10 bs
  case linesList of
    [] -> return (B.empty, state, T.empty, T.empty)
    [lastLine] -> return (lastLine, state, T.empty, T.empty)
    allLines -> do
      let foldLines currState [] = return (currState, T.empty, T.empty)
          foldLines currState (l:ls) = do
            (s1, r1, c1) <- processLine currState l
            (s2, r2, c2) <- foldLines s1 ls
            return (s2, r1 <> r2, c1 <> c2)
      (nextState, rText, cText) <- foldLines state (init allLines)
      return (last allLines, nextState, rText, cText)

-- Process the final buffer when the stream ends (which might not have a trailing newline)
processFinalBuffer :: StreamState -> B.ByteString -> IO (StreamState, Text)
processFinalBuffer state bs = do
  let linesList = splitBytes 10 bs
  case linesList of
    [] -> return (state, T.empty)
    allLines -> do
      let foldLines currState [] = return (currState, T.empty)
          foldLines currState (l:ls) = do
            (s1, _, c1) <- processLine currState l
            (s2, c2) <- foldLines s1 ls
            return (s2, c1 <> c2)
      foldLines state allLines

-- Process a single line and print content if it contains a chat delta
processLine :: StreamState -> B.ByteString -> IO (StreamState, Text, Text)
processLine state lineBytes = do
  let lineText = T.strip (TE.decodeUtf8With lenientDecode lineBytes)
  if T.null lineText
    then return (state, T.empty, T.empty)
    else if T.isPrefixOf "data: " lineText
      then do
        let dataContentText = T.stripPrefix "data: " lineText
        case dataContentText of
          Just "[DONE]" -> return (state, T.empty, T.empty)
          Just jsonText -> do
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
                              StateInit -> do
                                return StateContent
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

-- Helper to split ByteString by a delimiter byte
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

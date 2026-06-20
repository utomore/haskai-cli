{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ollama
  ( fetchModels
  , fetchChatResponseRaw
  , Tool(..)
  , ToolFunction(..)
  , toolsSchema
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
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.Word (Word8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Status (statusCode)
import Control.Exception (try, SomeException)

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
-- Chat request / response types
-- ---------------------------------------------------------------------------

data Tool = Tool
  { toolType     :: Text
  , toolFunction :: ToolFunction
  } deriving (Show, Eq, Generic)

instance ToJSON Tool where
  toJSON t = Aeson.object
    [ "type"     Aeson..= toolType t
    , "function" Aeson..= toolFunction t
    ]

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \v ->
    Tool <$> v .: "type"
         <*> v .: "function"

data ToolFunction = ToolFunction
  { tfName        :: Text
  , tfDescription :: Text
  , tfParameters  :: Aeson.Value
  } deriving (Show, Eq, Generic)

instance ToJSON ToolFunction where
  toJSON tf = Aeson.object
    [ "name"        Aeson..= tfName tf
    , "description" Aeson..= tfDescription tf
    , "parameters"  Aeson..= tfParameters tf
    ]

instance FromJSON ToolFunction where
  parseJSON = withObject "ToolFunction" $ \v ->
    ToolFunction <$> v .: "name"
                 <*> v .: "description"
                 <*> v .: "parameters"

data ChatRequest = ChatRequest
  { reqModel    :: String
  , reqMessages :: [Message]
  , reqStream   :: Bool
  , reqTools    :: Maybe [Tool]
  } deriving (Show, Eq, Generic)

instance ToJSON ChatRequest where
  toJSON cr = Aeson.object $
    [ "model"    Aeson..= reqModel cr
    , "messages" Aeson..= reqMessages cr
    , "stream"   Aeson..= reqStream cr
    ] ++ maybe [] (\ts -> ["tools" Aeson..= ts]) (reqTools cr)

data ChatCompletionChoice = ChatCompletionChoice
  { choiceMessage :: Message
  } deriving (Show, Eq, Generic)

instance FromJSON ChatCompletionChoice where
  parseJSON = withObject "ChatCompletionChoice" $ \v ->
    ChatCompletionChoice <$> v .: "message"

data ChatCompletionResponse = ChatCompletionResponse
  { respChoices :: [ChatCompletionChoice]
  } deriving (Show, Eq, Generic)

instance FromJSON ChatCompletionResponse where
  parseJSON = withObject "ChatCompletionResponse" $ \v ->
    ChatCompletionResponse <$> v .: "choices"

toolsSchema :: [Tool]
toolsSchema =
  [ Tool "function" (ToolFunction "run_command" "Run a shell command on the local system and return the combined stdout/stderr output." runCommandParams)
  , Tool "function" (ToolFunction "read_file" "Read the content of a file from the local filesystem." readFileParams)
  ]
  where
    runCommandParams = Aeson.object
      [ "type"       Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
          [ "command" Aeson..= Aeson.object
              [ "type"        Aeson..= ("string" :: Text)
              , "description" Aeson..= ("The exact shell command to execute." :: Text)
              ]
          ]
      , "required"   Aeson..= (["command"] :: [Text])
      ]
    readFileParams = Aeson.object
      [ "type"       Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
          [ "path" Aeson..= Aeson.object
              [ "type"        Aeson..= ("string" :: Text)
              , "description" Aeson..= ("The absolute or relative path to the file." :: Text)
              ]
          ]
      , "required"   Aeson..= (["path"] :: [Text])
      ]

-- ---------------------------------------------------------------------------
-- HTTP Helpers
-- ---------------------------------------------------------------------------

backendManagerAndHeaders :: ModelBackend -> IO (Manager, [Header])
backendManagerAndHeaders (LocalOllama _) = do
  mgr <- newManager defaultManagerSettings
  return (mgr, [])
backendManagerAndHeaders (RemoteOpenAI _ apiKey) = do
  mgr <- newManager tlsManagerSettings
  let authHeader = ("Authorization", BC.pack ("Bearer " ++ apiKey))
  return (mgr, [authHeader])

backendChatUrl :: ModelBackend -> String
backendChatUrl (LocalOllama baseUrl)      = baseUrl ++ "/v1/chat/completions"
backendChatUrl (RemoteOpenAI baseUrl _)   = baseUrl ++ "/chat/completions"

-- | Call the LLM with structured request, supporting function calling schemas.
fetchChatResponseRaw :: Config -> ModelId -> [Message] -> IO (Either String Message)
fetchChatResponseRaw config (ModelId modelStr) msgs = do
  let backend   = modelBackend config
      url       = backendChatUrl backend
      timeoutUs = httpTimeoutMicros config

  (manager, extraHeaders) <- backendManagerAndHeaders backend

  let action = do
        initialRequest <- parseRequest url
        let reqBody = ChatRequest modelStr msgs False (Just toolsSchema)
            request = initialRequest
              { method          = "POST"
              , requestBody     = RequestBodyLBS (Aeson.encode reqBody)
              , requestHeaders  = [("Content-Type", "application/json")] ++ extraHeaders
              , responseTimeout = responseTimeoutMicro timeoutUs
              }
        resp <- httpLbs request manager
        let sc = statusCode (responseStatus resp)
        if sc /= 200
          then return (Left $ "HTTP error: " ++ show sc)
          else case Aeson.decode (responseBody resp) of
            Just (ChatCompletionResponse (choice:_)) -> return (Right (choiceMessage choice))
            _ -> return (Left "Failed to parse chat response JSON")

  result <- try action
  case result of
    Left (err :: SomeException) -> return (Left $ show err)
    Right val                   -> return val

-- ---------------------------------------------------------------------------
-- Backward compatible types and helpers for test suite
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

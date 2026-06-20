{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), toJSON, parseJSON)
import Data.Text (Text)
import Data.IORef (IORef)
import Control.Monad.Trans.Reader (ReaderT)
import System.Console.Haskeline (InputT)

-- ---------------------------------------------------------------------------
-- Domain identifier newtypes (Phase 1)
-- Serialise as plain JSON strings for backward-compatible sessions.json.
-- ---------------------------------------------------------------------------

newtype SessionId = SessionId { unSessionId :: String }
  deriving (Show, Eq)

instance ToJSON SessionId where
  toJSON (SessionId s) = toJSON s

instance FromJSON SessionId where
  parseJSON v = SessionId <$> parseJSON v

newtype ModelId = ModelId { unModelId :: String }
  deriving (Show, Eq)

instance ToJSON ModelId where
  toJSON (ModelId s) = toJSON s

instance FromJSON ModelId where
  parseJSON v = ModelId <$> parseJSON v

newtype SessionName = SessionName { unSessionName :: String }
  deriving (Show, Eq)

instance ToJSON SessionName where
  toJSON (SessionName s) = toJSON s

instance FromJSON SessionName where
  parseJSON v = SessionName <$> parseJSON v

-- ---------------------------------------------------------------------------
-- Model backend (Phase 4)
-- ---------------------------------------------------------------------------

-- LocalOllama  — hits a local Ollama server (OpenAI-compatible API, no auth)
-- RemoteOpenAI — hits any OpenAI-compatible endpoint with Bearer auth
--                (e.g. Google Gemini via https://…/v1beta/openai)
data ModelBackend
  = LocalOllama String          -- base URL, e.g. "http://localhost:11434"
  | RemoteOpenAI String String  -- base URL, API key
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Core domain types
-- ---------------------------------------------------------------------------

data Message = Message
  { role    :: Text
  , content :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Message
instance FromJSON Message

data Session = Session
  { sessionId   :: SessionId
  , sessionName :: SessionName
  , messages    :: [Message]
  } deriving (Show, Eq, Generic)

instance ToJSON Session
instance FromJSON Session

data AppState = AppState
  { selectedModel    :: ModelId
  , sessionHistory   :: [Message]
  , longTermMemories :: [Text]
  , currentSessionId :: SessionId
  , sessions         :: [Session]
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Configuration (Phase 4) — no more magic numbers in source
-- ---------------------------------------------------------------------------

data Config = Config
  { modelBackend       :: ModelBackend
  , fallbackModel      :: ModelId
  , httpTimeoutMicros  :: Int       -- response timeout for LLM calls
  , spinnerDelayMicros :: Int       -- animation frame delay
  , memoryFilePath     :: FilePath
  , sessionsFilePath   :: FilePath
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Application monad
-- ---------------------------------------------------------------------------

data Env = Env
  { envConfig :: Config
  , envState  :: IORef AppState
  }

type AppM = ReaderT Env (InputT IO)

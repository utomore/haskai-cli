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
-- Command ADT (Phase 3)
-- Represents every slash command the user can type.
-- parseCommand (in Lib) produces one of these; handleCommand dispatches on it.
-- ---------------------------------------------------------------------------

data Command
  = CmdExit                        -- /exit | /quit
  | CmdHelp                        -- /help
  | CmdMemories                    -- /memories
  | CmdRemember Text               -- /remember <fact>
  | CmdForget Int                  -- /forget <1-based index>
  | CmdSessionList                 -- /session list
  | CmdSessionNew (Maybe Text)     -- /session new [name]
  | CmdSessionLoad String          -- /session load <index_or_name>
  | CmdSessionRename String        -- /session rename <new_name>
  | CmdSessionDelete String        -- /session delete <index_or_name>
  | CmdSessionFork (Maybe Text)    -- /session fork [name]
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Structured error type (Type-driven error handling)
-- ---------------------------------------------------------------------------

-- | All failure modes the application can encounter.
-- Use 'displayError' to render a human-readable message at the UI boundary.
data AppError
  = NetworkError String     -- ^ Connection or timeout failure
  | HttpStatusError Int     -- ^ Non-200 HTTP response from the backend
  | ParseError String       -- ^ JSON or SSE decoding failure
  | StorageError String     -- ^ File read / write failure
  | UserInputError String   -- ^ Malformed command arguments supplied by user
  | UnknownCommand String   -- ^ Unrecognised slash-command
  deriving (Show, Eq)

-- | Render an 'AppError' as a one-line human-readable message.
displayError :: AppError -> String
displayError (NetworkError msg)   = "Network error: " ++ msg
displayError (HttpStatusError sc) = "HTTP error: " ++ show sc
displayError (ParseError msg)     = "Parse error: " ++ msg
displayError (StorageError msg)   = "Storage error: " ++ msg
displayError (UserInputError msg) = msg
displayError (UnknownCommand cmd) = "Unknown command: " ++ cmd ++ ". Type /help for assistance."

-- ---------------------------------------------------------------------------
-- Application monad
-- ---------------------------------------------------------------------------

data Env = Env
  { envConfig :: Config
  , envState  :: IORef AppState
  }

type AppM = ReaderT Env (InputT IO)

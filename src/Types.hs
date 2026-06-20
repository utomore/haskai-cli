{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), toJSON, parseJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
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

data FunctionCall = FunctionCall
  { fcName      :: Text
  , fcArguments :: Text  -- JSON-formatted arguments string
  } deriving (Show, Eq, Generic)

instance ToJSON FunctionCall where
  toJSON fc = Aeson.object
    [ "name"      .= fcName fc
    , "arguments" .= fcArguments fc
    ]

instance FromJSON FunctionCall where
  parseJSON = Aeson.withObject "FunctionCall" $ \v ->
    FunctionCall <$> v .: "name"
                 <*> v .: "arguments"

data ToolCall = ToolCall
  { tcId       :: Text
  , tcType     :: Text
  , tcFunction :: FunctionCall
  } deriving (Show, Eq, Generic)

instance ToJSON ToolCall where
  toJSON tc = Aeson.object
    [ "id"       .= tcId tc
    , "type"     .= tcType tc
    , "function" .= tcFunction tc
    ]

instance FromJSON ToolCall where
  parseJSON = Aeson.withObject "ToolCall" $ \v ->
    ToolCall <$> v .: "id"
             <*> v .: "type"
             <*> v .: "function"

data Message = Message
  { role         :: Text
  , content      :: Text
  , tool_calls   :: Maybe [ToolCall]  -- LLM tool calling request
  , tool_call_id :: Maybe Text        -- Tool response reference ID
  } deriving (Show, Eq, Generic)

instance ToJSON Message where
  toJSON msg = Aeson.object $
    [ "role"    .= role msg
    , "content" .= content msg
    ]
    ++ maybe [] (\tcs -> ["tool_calls" .= tcs]) (tool_calls msg)
    ++ maybe [] (\tcid -> ["tool_call_id" .= tcid]) (tool_call_id msg)

instance FromJSON Message where
  parseJSON = Aeson.withObject "Message" $ \v ->
    Message <$> v .: "role"
            <*> v .: "content"
            <*> v .:? "tool_calls"
            <*> v .:? "tool_call_id"

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
  , loadedFile       :: Maybe (FilePath, Text)
  } deriving (Show, Eq)

syncActiveSession :: AppState -> AppState
syncActiveSession state =
  let activeId  = currentSessionId state
      history   = sessionHistory state
      updatedSs = map (\s -> if sessionId s == activeId
                               then s { messages = history }
                               else s) (sessions state)
  in state { sessions = updatedSs }

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
  | CmdRead FilePath (Maybe Text)  -- /read <file> [question]
  | CmdRun String                  -- /run <cmd>
  | CmdClear                       -- /clear
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- CommandResult ADT
-- ---------------------------------------------------------------------------

data CommandResult
  = ResExit
  | ResHelp
  | ResMemories [Text]
  | ResRemember Text
  | ResForget Int [Text]
  | ResSessionList [Session] SessionId
  | ResSessionNew SessionName SessionId
  | ResSessionLoad SessionName SessionId
  | ResSessionRename SessionName
  | ResSessionDelete SessionName SessionId
  | ResSessionFork SessionName SessionId
  | ResRead FilePath
  | ResRun String
  | ResClear
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Application monad
-- ---------------------------------------------------------------------------

data Env = Env
  { envConfig :: Config
  , envState  :: IORef AppState
  }

type AppM = ReaderT Env (InputT IO)

type AgentM a = ReaderT Env IO a


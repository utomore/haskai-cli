{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.IORef (IORef)
import Control.Monad.Trans.Reader (ReaderT)
import System.Console.Haskeline (InputT)

data Message = Message
  { role    :: Text
  , content :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Message
instance FromJSON Message

data Session = Session
  { sessionId   :: String
  , sessionName :: String
  , messages    :: [Message]
  } deriving (Show, Eq, Generic)

instance ToJSON Session
instance FromJSON Session

data AppState = AppState
  { selectedModel    :: String
  , sessionHistory   :: [Message]
  , longTermMemories :: [Text]
  , currentSessionId :: String
  , sessions         :: [Session]
  } deriving (Show, Eq)

data Config = Config
  { ollamaBaseUrl    :: String
  , memoryFilePath   :: FilePath
  , sessionsFilePath :: FilePath
  } deriving (Show, Eq)

-- Environment storing configuration and thread-safe mutable state pointer
data Env = Env
  { envConfig :: Config
  , envState  :: IORef AppState
  }

-- Application Monad Stack: ReaderT Env over Haskeline InputT IO
type AppM = ReaderT Env (InputT IO)

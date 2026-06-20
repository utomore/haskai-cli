{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | JSON stdio server — bridges Tauri desktop frontend to the Haskell core.
--
-- Protocol (newline-delimited JSON):
--   stdin  <- {"input": "user text or /command"}
--   stdout -> {"type": "chat|info|memories|sessions|models|error|exit",
--              "text": "...",
--              "data": null | [...]}

module Main where

import Types
import AppConfig  (loadDevConfig)
import Memory     (loadMemories, saveMemories, loadSessions, saveSessions, ensureConfigDirExists)
import Ollama     (fetchChatResponse, fetchModels)
import Lib        (parseCommand, buildSystemMessage, parseSessionIndexOrName,
                   currentSessionName, syncActiveSession)

import qualified Data.Aeson           as Aeson
import           Data.Aeson           ((.=))
import           Data.Aeson.Types     (parseMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Text.IO         as TIO
import qualified Data.ByteString.Lazy as BL
import           Data.IORef
import           System.IO            (hSetBuffering, stdin, stdout, stderr,
                                       BufferMode(..))
import           System.Directory     (getAppUserDataDirectory)
import           System.FilePath      ((</>))
import           Data.Time            (getCurrentTime, getCurrentTimeZone,
                                       utcToLocalTime, formatTime, defaultTimeLocale)

-- ---------------------------------------------------------------------------
-- Response helpers
-- ---------------------------------------------------------------------------

writeResp :: Text -> Text -> Maybe Aeson.Value -> IO ()
writeResp t txt mData =
  let pairs = ["type" .= t, "text" .= txt]
               ++ maybe [] (\d -> ["data" .= d]) mData
  in TIO.putStrLn (TE.decodeUtf8 (BL.toStrict (Aeson.encode (Aeson.object pairs))))

respChat     :: Text -> IO ()
respChat t    = writeResp "chat"     t Nothing

respInfo     :: Text -> IO ()
respInfo t    = writeResp "info"     t Nothing

respError    :: Text -> IO ()
respError t   = writeResp "error"    t Nothing

respExit     :: IO ()
respExit      = writeResp "exit"     "Goodbye!" Nothing

respMemories :: [Text] -> IO ()
respMemories mems =
  writeResp "memories"
    (T.pack (show (length mems) ++ " memories stored"))
    (Just (Aeson.toJSON mems))

respSessions :: [Session] -> SessionId -> IO ()
respSessions ss activeId =
  let items = Aeson.toJSON $ map (\(i, s) -> Aeson.object
        [ "index"    .= (i :: Int)
        , "id"       .= unSessionId (sessionId s)
        , "name"     .= unSessionName (sessionName s)
        , "active"   .= (sessionId s == activeId)
        , "msgCount" .= length (messages s)
        ]) (zip [1..] ss)
  in writeResp "sessions"
       (T.pack (show (length ss) ++ " sessions"))
       (Just items)

respModels :: [String] -> IO ()
respModels ms =
  writeResp "models"
    (T.pack (show (length ms) ++ " models available"))
    (Just (Aeson.toJSON ms))

-- ---------------------------------------------------------------------------
-- Time helper
-- ---------------------------------------------------------------------------

getTimeStr :: IO String
getTimeStr = do
  now <- getCurrentTime
  tz  <- getCurrentTimeZone
  return $ formatTime defaultTimeLocale "%Y%m%d_%H%M%S" (utcToLocalTime tz now)

-- ---------------------------------------------------------------------------
-- State helpers
-- ---------------------------------------------------------------------------

type StateRef = IORef AppState

getS :: StateRef -> IO AppState
getS = readIORef

modS :: StateRef -> (AppState -> AppState) -> IO ()
modS = modifyIORef'

-- ---------------------------------------------------------------------------
-- Command handlers
-- ---------------------------------------------------------------------------

handleCmd :: Config -> StateRef -> Command -> IO Bool
handleCmd _ _ CmdExit = respExit >> return False

handleCmd _ _ CmdHelp = do
  respInfo $ T.unlines
    [ "Available commands:"
    , "  /help                  — show this help"
    , "  /memories              — list long-term memories"
    , "  /remember <fact>       — save a fact to memory"
    , "  /forget <index>        — delete a memory by index"
    , "  /session list          — list all sessions"
    , "  /session new [name]    — start a new session"
    , "  /session load <n>      — load session by index or name"
    , "  /session rename <name> — rename current session"
    , "  /session delete <n>    — delete a session"
    , "  /session fork [name]   — fork current session"
    , "  /models                — list available Ollama models"
    , "  /exit  /quit           — exit"
    ]
  return True

handleCmd _ ref CmdMemories = do
  st <- getS ref
  respMemories (longTermMemories st)
  return True

handleCmd cfg ref (CmdRemember fact) = do
  st <- getS ref
  let newMems = longTermMemories st ++ [fact]
  modS ref (\s -> s { longTermMemories = newMems })
  saveMemories (memoryFilePath cfg) newMems
  respInfo ("Memory saved: \"" <> fact <> "\"")
  return True

handleCmd cfg ref (CmdForget idx) = do
  st <- getS ref
  let mems = longTermMemories st
  if idx >= 1 && idx <= length mems
    then do
      let (l, r) = splitAt (idx - 1) mems
          newMems = l ++ drop 1 r
      modS ref (\s -> s { longTermMemories = newMems })
      saveMemories (memoryFilePath cfg) newMems
      respInfo (T.pack ("Memory [" ++ show idx ++ "] removed."))
    else respError (T.pack ("Index out of range. Valid: 1–" ++ show (length mems)))
  return True

handleCmd _ ref CmdSessionList = do
  st <- getS ref
  respSessions (sessions st) (currentSessionId st)
  return True

handleCmd cfg ref (CmdSessionNew mName) = do
  st      <- getS ref
  timeStr <- getTimeStr
  let newId  = SessionId  ("session_" ++ timeStr)
      sName  = SessionName (maybe ("Session " ++ timeStr) T.unpack mName)
      newS   = Session newId sName []
      synced = syncActiveSession st
      newSt  = synced { sessions        = sessions synced ++ [newS]
                      , currentSessionId = newId
                      , sessionHistory   = []
                      }
  modS ref (const newSt)
  saveSessions (sessionsFilePath cfg) (sessions newSt)
  respSessions (sessions newSt) newId
  return True

handleCmd cfg ref (CmdSessionLoad arg) = do
  st <- getS ref
  case parseSessionIndexOrName arg (sessions st) of
    Nothing -> respError "Session not found."
    Just targetS -> do
      let synced = syncActiveSession st
          newSt  = synced { currentSessionId = sessionId targetS
                          , sessionHistory   = messages targetS
                          }
      modS ref (const newSt)
      saveSessions (sessionsFilePath cfg) (sessions newSt)
      respSessions (sessions newSt) (sessionId targetS)
  return True

handleCmd cfg ref (CmdSessionRename newName) = do
  st <- getS ref
  let activeId   = currentSessionId st
      updatedSs  = map (\s -> if sessionId s == activeId
                                then s { sessionName = SessionName newName }
                                else s) (sessions st)
      newSt = st { sessions = updatedSs }
  modS ref (const newSt)
  saveSessions (sessionsFilePath cfg) updatedSs
  respInfo (T.pack ("Session renamed to \"" ++ newName ++ "\"."))
  return True

handleCmd cfg ref (CmdSessionDelete arg) = do
  st <- getS ref
  let ss = sessions st
  if length ss <= 1
    then respError "Cannot delete the only session."
    else case parseSessionIndexOrName arg ss of
      Nothing -> respError "Session not found."
      Just targetS -> do
        let targetId             = sessionId targetS
            activeId             = currentSessionId st
            newSs                = filter (\s -> sessionId s /= targetId) ss
            (newSt, newActiveId) =
              if targetId == activeId
                then case newSs of
                  (fb:_) -> ( st { sessions = newSs, currentSessionId = sessionId fb
                                 , sessionHistory = messages fb }
                            , sessionId fb )
                  []     -> (st { sessions = newSs }, activeId)
                else (st { sessions = newSs }, activeId)
        modS ref (const newSt)
        saveSessions (sessionsFilePath cfg) newSs
        respSessions newSs newActiveId
  return True

handleCmd cfg ref (CmdSessionFork mName) = do
  st      <- getS ref
  timeStr <- getTimeStr
  let newId  = SessionId  ("session_fork_" ++ timeStr)
      sName  = SessionName (maybe ("Fork " ++ timeStr) T.unpack mName)
      newS   = Session newId sName (sessionHistory st)
      synced = syncActiveSession st
      newSt  = synced { sessions        = sessions synced ++ [newS]
                      , currentSessionId = newId
                      }
  modS ref (const newSt)
  saveSessions (sessionsFilePath cfg) (sessions newSt)
  respSessions (sessions newSt) newId
  return True

-- ---------------------------------------------------------------------------
-- Chat handler
-- ---------------------------------------------------------------------------

handleChat :: Config -> StateRef -> Text -> IO ()
handleChat cfg ref userText = do
  st <- getS ref
  let userMsg = Message "user" userText
      hist    = sessionHistory st
      sysMsgs = buildSystemMessage (longTermMemories st)
      allMsgs = sysMsgs : hist ++ [userMsg]
  result <- fetchChatResponse cfg (selectedModel st) allMsgs
  case result of
    Left err -> respError (T.pack err)
    Right reply -> do
      let aiMsg  = Message "assistant" reply
          newSt  = syncActiveSession st { sessionHistory = hist ++ [userMsg, aiMsg] }
      modS ref (const newSt)
      saveSessions (sessionsFilePath cfg) (sessions newSt)
      respChat reply

-- ---------------------------------------------------------------------------
-- Main loop
-- ---------------------------------------------------------------------------

loop :: Config -> StateRef -> IO ()
loop cfg ref = do
  line <- TIO.getLine
  case Aeson.decodeStrict (TE.encodeUtf8 line) of
    Nothing -> respError "Invalid JSON request" >> loop cfg ref
    Just v  ->
      case parseMaybe (Aeson..: "input") v :: Maybe Text of
        Nothing    -> respError "Missing 'input' field" >> loop cfg ref
        Just input ->
          let trimmed = T.strip input
          in if T.isPrefixOf "/" trimmed
               then if trimmed == "/models"
                      then do
                        ms <- fetchModels cfg
                        respModels ms
                        loop cfg ref
                      else case parseCommand trimmed of
                        Left err  -> respError (T.pack err) >> loop cfg ref
                        Right cmd -> do
                          cont <- handleCmd cfg ref cmd
                          if cont then loop cfg ref else return ()
               else handleChat cfg ref trimmed >> loop cfg ref

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  appDir <- getAppUserDataDirectory "haskai"
  let memPath  = appDir </> "memory.json"
      sessPath = appDir </> "sessions.json"

  ensureConfigDirExists memPath
  ensureConfigDirExists sessPath

  cfg      <- loadDevConfig "dev.config" memPath sessPath
  mems     <- loadMemories memPath
  loadedSs <- loadSessions sessPath

  let (initSessions, activeId) =
        if null loadedSs
          then let defS = Session (SessionId "default") (SessionName "Default") []
               in ([defS], SessionId "default")
          else (loadedSs, sessionId (head loadedSs))

  let activeHist =
        case filter (\s -> sessionId s == activeId) initSessions of
          (s:_) -> messages s
          []    -> []

  let initState = AppState
        { selectedModel    = fallbackModel cfg
        , sessionHistory   = activeHist
        , longTermMemories = mems
        , currentSessionId = activeId
        , sessions         = initSessions
        }

  ref <- newIORef initState
  loop cfg ref

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controller
  ( executeCommand
  , executeAgentChat
  , buildSystemMessage
  ) where

import Types
import Memory
import Ollama
import Tool

import Control.Monad.Reader
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime, formatTime, defaultTimeLocale)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE

-- ---------------------------------------------------------------------------
-- Time helpers
-- ---------------------------------------------------------------------------

getFormattedTime :: String -> IO String
getFormattedTime fmt = do
  now <- getCurrentTime
  tz  <- getCurrentTimeZone
  return $ formatTime defaultTimeLocale fmt (utcToLocalTime tz now)

-- ---------------------------------------------------------------------------
-- Helper to parse session by name or index
-- ---------------------------------------------------------------------------

parseSessionIndexOrName :: String -> [Session] -> Maybe Session
parseSessionIndexOrName input ss =
  case readMaybe input :: Maybe Int of
    Just idx
      | idx >= 1 && idx <= length ss -> Just (ss !! (idx - 1))
      | otherwise                    -> Nothing
    Nothing ->
      case filter (\s -> unSessionName (sessionName s) == input
                      || unSessionId   (sessionId   s) == input) ss of
        (s:_) -> Just s
        []    -> Nothing

-- ---------------------------------------------------------------------------
-- System message
-- ---------------------------------------------------------------------------

buildSystemMessage :: [Text] -> Message
buildSystemMessage memories =
  let base = "You are a helpful and intelligent AI CLI assistant. You interact with the user via a terminal interface."
      memPart = if null memories
        then ""
        else "\nHere are facts you remember about the user (use this context to personalize your responses when relevant):\n"
             ++ unlines (map (\m -> "- " ++ T.unpack m) memories)
  in Message "system" (T.pack (base ++ memPart)) Nothing Nothing

-- ---------------------------------------------------------------------------
-- Command Handler (Unified Controller)
-- ---------------------------------------------------------------------------

executeCommand :: Command -> AgentM (Either String CommandResult)
executeCommand cmd = do
  env <- ask
  let config = envConfig env
  state <- liftIO $ readIORef (envState env)
  case cmd of
    CmdExit -> return $ Right ResExit

    CmdHelp -> return $ Right ResHelp

    CmdMemories -> return $ Right $ ResMemories (longTermMemories state)

    CmdRemember fact -> do
      let newMems = longTermMemories state ++ [fact]
          infoMsg = Message "system" ("已記住事實: \"" <> fact <> "\"") Nothing Nothing
          newState = state { longTermMemories = newMems, sessionHistory = sessionHistory state ++ [infoMsg] }
          newStateSynced = syncActiveSession newState
      liftIO $ modifyIORef' (envState env) (const newStateSynced)
      liftIO $ saveMemories (memoryFilePath config) newMems
      liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
      return $ Right $ ResRemember fact

    CmdForget idx -> do
      let mems = longTermMemories state
      if idx >= 1 && idx <= length mems
        then do
          let (left, right) = splitAt (idx - 1) mems
              newMems = left ++ drop 1 right
              infoMsg = Message "system" (T.pack ("已刪除記憶 [" ++ show idx ++ "]")) Nothing Nothing
              newState = state { longTermMemories = newMems, sessionHistory = sessionHistory state ++ [infoMsg] }
              newStateSynced = syncActiveSession newState
          liftIO $ modifyIORef' (envState env) (const newStateSynced)
          liftIO $ saveMemories (memoryFilePath config) newMems
          liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
          return $ Right $ ResForget idx newMems
        else return $ Left $ "索引超出範圍。有效範圍: 1 至 " ++ show (length mems)

    CmdSessionList -> return $ Right $ ResSessionList (sessions state) (currentSessionId state)

    CmdSessionNew mName -> do
      timeStr <- liftIO $ getFormattedTime "%Y%m%d_%H%M%S"
      let newId = SessionId ("session_" ++ timeStr)
          sName = SessionName (maybe ("Session " ++ timeStr) T.unpack mName)
          newS  = Session newId sName []
          synced = syncActiveSession state
          infoMsg = Message "system" (T.pack ("已建立新會話: " ++ unSessionName sName)) Nothing Nothing
          newState = synced { sessions = sessions synced ++ [newS]
                            , currentSessionId = newId
                            , sessionHistory = [infoMsg]
                            }
          newStateSynced = syncActiveSession newState
      liftIO $ modifyIORef' (envState env) (const newStateSynced)
      liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
      return $ Right $ ResSessionNew sName newId

    CmdSessionLoad arg -> do
      case parseSessionIndexOrName arg (sessions state) of
        Nothing -> return $ Left "找不到會話。"
        Just targetS -> do
          let synced = syncActiveSession state
              infoMsg = Message "system" (T.pack ("已載入會話: " ++ unSessionName (sessionName targetS))) Nothing Nothing
              newState = synced { currentSessionId = sessionId targetS
                                , sessionHistory = messages targetS ++ [infoMsg]
                                }
              newStateSynced = syncActiveSession newState
          liftIO $ modifyIORef' (envState env) (const newStateSynced)
          liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
          return $ Right $ ResSessionLoad (sessionName targetS) (sessionId targetS)

    CmdSessionRename newName -> do
      let activeId = currentSessionId state
          infoMsg = Message "system" (T.pack ("已將當前會話重新命名為: " ++ newName)) Nothing Nothing
          updatedSs = map (\s -> if sessionId s == activeId
                                   then s { sessionName = SessionName newName }
                                   else s) (sessions state)
          newState = state { sessions = updatedSs, sessionHistory = sessionHistory state ++ [infoMsg] }
          newStateSynced = syncActiveSession newState
      liftIO $ modifyIORef' (envState env) (const newStateSynced)
      liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
      return $ Right $ ResSessionRename (SessionName newName)

    CmdSessionDelete arg -> do
      let ss = sessions state
      if length ss <= 1
        then return $ Left "無法刪除唯一的會話。"
        else case parseSessionIndexOrName arg ss of
          Nothing -> return $ Left "找不到指定會話。"
          Just targetS -> do
            let targetId = sessionId targetS
                activeId = currentSessionId state
                ss' = filter (\s -> sessionId s /= targetId) ss
            if targetId == activeId
              then case ss' of
                (fb:_) -> do
                  let infoMsg = Message "system" (T.pack ("已刪除當前會話，切換至: " ++ unSessionName (sessionName fb))) Nothing Nothing
                      newState = state { sessions = ss'
                                       , currentSessionId = sessionId fb
                                       , sessionHistory = messages fb ++ [infoMsg]
                                       }
                      newStateSynced = syncActiveSession newState
                  liftIO $ modifyIORef' (envState env) (const newStateSynced)
                  liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
                  return $ Right $ ResSessionDelete (sessionName targetS) (sessionId fb)
                [] -> return $ Left "沒有可切換的會話。"
              else do
                let infoMsg = Message "system" (T.pack ("已刪除會話: " ++ unSessionName (sessionName targetS))) Nothing Nothing
                    newState = state { sessions = ss', sessionHistory = sessionHistory state ++ [infoMsg] }
                    newStateSynced = syncActiveSession newState
                liftIO $ modifyIORef' (envState env) (const newStateSynced)
                liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
                return $ Right $ ResSessionDelete (sessionName targetS) activeId

    CmdSessionFork mName -> do
      timeStr <- liftIO $ getFormattedTime "%Y%m%d_%H%M%S"
      let newId = SessionId ("session_fork_" ++ timeStr)
          sName = SessionName (maybe ("Fork " ++ timeStr) T.unpack mName)
          newS  = Session newId sName (sessionHistory state)
          synced = syncActiveSession state
          infoMsg = Message "system" (T.pack ("已複製當前會話至: " ++ unSessionName sName)) Nothing Nothing
          newState = synced { sessions = sessions synced ++ [newS]
                            , currentSessionId = newId
                            , sessionHistory = sessionHistory synced ++ [infoMsg]
                            }
          newStateSynced = syncActiveSession newState
      liftIO $ modifyIORef' (envState env) (const newStateSynced)
      liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
      return $ Right $ ResSessionFork sName newId

    CmdRead path mQuest -> do
      readRes <- liftIO $ readTextFileStrict path
      case readRes of
        Left err -> return $ Left $ "讀取檔案失敗: " ++ err
        Right fileContent -> do
          let infoMsg = Message "system" (T.pack ("已載入檔案: " ++ path)) Nothing Nothing
              newState = state { loadedFile = Just (path, fileContent)
                               , sessionHistory = sessionHistory state ++ [infoMsg]
                               }
              newStateSynced = syncActiveSession newState
          liftIO $ modifyIORef' (envState env) (const newStateSynced)
          liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
          return $ Right $ ResRead path

    CmdRun cmdStr -> do
      let infoMsg = Message "system" (T.pack ("正在執行命令: " ++ cmdStr)) Nothing Nothing
          newState = state { sessionHistory = sessionHistory state ++ [infoMsg] }
          newStateSynced = syncActiveSession newState
      liftIO $ modifyIORef' (envState env) (const newStateSynced)
      liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
      
      runRes <- liftIO $ runShellCommand cmdStr
      case runRes of
        Left err -> return $ Left $ "指令執行錯誤: " ++ err
        Right output -> do
          stateAfter <- liftIO $ readIORef (envState env)
          let resultMsg = Message "system" (T.pack ("指令執行結果:\n" ++ output)) Nothing Nothing
              newStateFinal = stateAfter { sessionHistory = sessionHistory stateAfter ++ [resultMsg] }
              newStateFinalSynced = syncActiveSession newStateFinal
          liftIO $ modifyIORef' (envState env) (const newStateFinalSynced)
          liftIO $ saveSessions (sessionsFilePath config) (sessions newStateFinalSynced)
          return $ Right $ ResRun output

    CmdClear -> do
      let newState = state { sessionHistory = [] }
          newStateSynced = syncActiveSession newState
      liftIO $ modifyIORef' (envState env) (const newStateSynced)
      liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
      return $ Right ResClear

-- ---------------------------------------------------------------------------
-- Agent Chat Handler (Recursive Tool Calling Loop)
-- ---------------------------------------------------------------------------

executeAgentChat :: Text -> (Message -> IO ()) -> AgentM (Either String Message)
executeAgentChat userText stepCallback = do
  env <- ask
  let config = envConfig env
  state <- liftIO $ readIORef (envState env)
  
  -- 1. Merging loadedFile if present
  let (userTextFinal, updatedStatePre) = case loadedFile state of
        Just (path, fileContent) ->
          let prompt = "Below is the content of the file `" ++ path ++ "`:\n\n```\n"
                    ++ T.unpack fileContent ++ "\n```\n\n"
                    ++ if T.null (T.strip userText) then "請分析此檔案內容。" else T.unpack userText
          in (T.pack prompt, state { loadedFile = Nothing })
        Nothing -> (userText, state)
        
  let userMsg = Message "user" userTextFinal Nothing Nothing
      hist' = sessionHistory updatedStatePre ++ [userMsg]
      updatedState = updatedStatePre { sessionHistory = hist' }
      updatedStateSynced = syncActiveSession updatedState
      
  liftIO $ modifyIORef' (envState env) (const updatedStateSynced)
  liftIO $ saveSessions (sessionsFilePath config) (sessions updatedStateSynced)
  
  -- Send userMsg to UI callback
  liftIO $ stepCallback userMsg
  
  -- 2. Agentic Tool Calling loop
  let systemMsg = buildSystemMessage (longTermMemories updatedStateSynced)
      allMessages = systemMsg : hist'
      modelIdVal = selectedModel updatedStateSynced
      
  res <- runAgentLoop config modelIdVal allMessages
  return res

  where
    runAgentLoop :: Config -> ModelId -> [Message] -> AgentM (Either String Message)
    runAgentLoop config modelIdVal initialHistory = runLoop initialHistory 0
      where
        runLoop :: [Message] -> Int -> AgentM (Either String Message)
        runLoop hist depth
          | depth >= 5 = return $ Left "Agentic loop exceeded maximum depth of 5 steps."
          | otherwise = do
              env <- ask
              llmRes <- liftIO $ fetchChatResponseRaw config modelIdVal hist
              case llmRes of
                Left err -> return $ Left err
                Right msg ->
                  case tool_calls msg of
                    Nothing -> finalizeChat msg
                    Just [] -> finalizeChat msg
                    Just tcs -> do
                      -- AI requested tools. Notify callback about this message first.
                      liftIO $ stepCallback msg
                      -- Append assistant tool request message to session history
                      liftIO $ modifyIORef' (envState env) $ \s ->
                        let s' = s { sessionHistory = sessionHistory s ++ [msg] }
                        in syncActiveSession s'
                      
                      let processCall tc = do
                            let callId = tcId tc
                                func   = tcFunction tc
                                name   = fcName func
                                args   = fcArguments func
                            
                            toolOutput <- case name of
                              "run_command" -> do
                                case Aeson.decode (BL.fromStrict (TE.encodeUtf8 args)) :: Maybe Aeson.Value of
                                  Just (Aeson.Object o) | Just (Aeson.String cmd) <- KeyMap.lookup "command" o -> do
                                    let cmdStr = T.unpack cmd
                                    runRes <- liftIO $ runShellCommand cmdStr
                                    case runRes of
                                      Left err -> return $ T.pack ("Error executing command: " ++ err)
                                      Right output -> return $ T.pack output
                                  _ -> return "Error: Missing or invalid 'command' argument."
                                  
                              "read_file" -> do
                                case Aeson.decode (BL.fromStrict (TE.encodeUtf8 args)) :: Maybe Aeson.Value of
                                  Just (Aeson.Object o) | Just (Aeson.String path) <- KeyMap.lookup "path" o -> do
                                    let pathStr = T.unpack path
                                    readRes <- liftIO $ readTextFileStrict pathStr
                                    case readRes of
                                      Left err -> return $ T.pack ("Error reading file: " ++ err)
                                      Right content -> return content
                                  _ -> return "Error: Missing or invalid 'path' argument."
                                  
                              unknown -> return $ T.pack ("Error: Unknown tool '" ++ T.unpack unknown ++ "'")
                            
                            let toolMsg = Message "tool" toolOutput Nothing (Just callId)
                            liftIO $ stepCallback toolMsg
                            
                            -- Append tool response message to session history
                            liftIO $ modifyIORef' (envState env) $ \s ->
                              let s' = s { sessionHistory = sessionHistory s ++ [toolMsg] }
                              in syncActiveSession s'
                              
                            return toolMsg
                            
                      toolMsgs <- mapM processCall tcs
                      runLoop (hist ++ [msg] ++ toolMsgs) (depth + 1)

        finalizeChat :: Message -> AgentM (Either String Message)
        finalizeChat msg = do
          env <- ask
          let config = envConfig env
          -- Notify callback about final message
          liftIO $ stepCallback msg
          -- Append final message to session history
          stateAfter <- liftIO $ readIORef (envState env)
          let finalState = stateAfter { sessionHistory = sessionHistory stateAfter ++ [msg] }
              finalStateSynced = syncActiveSession finalState
          liftIO $ modifyIORef' (envState env) (const finalStateSynced)
          liftIO $ saveSessions (sessionsFilePath config) (sessions finalStateSynced)
          return $ Right msg

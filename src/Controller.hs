{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controller
  ( executeCommand
  , executeAgentChat
  , loadProjectPrompt
  , assembleContext
  , estimateContextCharSize
  , getContextTokenCount
  , getContextSizes
  , partitionHistForContext
  , filterLlmHistory
  , ContextSizes(..)
  ) where

import Types
import Memory
import Ollama
import Tool

import Control.Monad (filterM, when)
import System.Directory (doesFileExist)
import System.FilePath (takeFileName)
import Data.Maybe (fromMaybe)
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
-- Context Assembly System & Summarization Helpers
-- ---------------------------------------------------------------------------

filterLlmHistory :: [Message] -> [Message]
filterLlmHistory = filter isLlmHistoryMessage
  where
    isLlmHistoryMessage msg =
      role msg /= "system" || "System Context Summary of past messages:" `T.isPrefixOf` content msg

data ContextSizes = ContextSizes
  { sysPromptCharSize :: Int
  , historyCharSize   :: Int
  , fileCharSize      :: Int
  , userQuestionSize  :: Int
  } deriving (Show, Eq)

loadProjectPrompt :: IO String
loadProjectPrompt = do
  let paths = [".agent/agent.md", ".agents/agent.md"]
  found <- filterM doesFileExist paths
  case found of
    (p:_) -> readFile p
    []    -> return ""

defaultSystemPrompt :: String
defaultSystemPrompt = "You are a helpful and intelligent AI CLI assistant. You interact with the user via a terminal interface."

assembleContext :: Maybe Text -> String -> Maybe [(FilePath, Text)] -> [Message] -> String -> IO String
assembleContext mbCustomSystem projPrompt mbAttachedFiles history userQuestion = do
  let basePrompt = case mbCustomSystem of
                     Just p  -> T.unpack p
                     Nothing -> defaultSystemPrompt
  let defaultPart = "<SystemPrompt>\n" ++ basePrompt ++ "\n</SystemPrompt>"
      projPart = if null projPrompt
                   then ""
                   else "\n<ProjectPrompt>\n" ++ projPrompt ++ "\n</ProjectPrompt>"
      formatFile (path, fileContent) =
        "\n<AttachedFile filename=\"" ++ path ++ "\">\n" ++ T.unpack fileContent ++ "\n</AttachedFile>"
      filePart = case mbAttachedFiles of
                   Nothing -> ""
                   Just files -> concatMap formatFile files
      cleanHistory = filterLlmHistory history
      historyPart = if null cleanHistory
                      then ""
                      else "\n<ChatHistory>\n" ++ concatMap formatHistoryMsg cleanHistory ++ "</ChatHistory>"
      userPart = if null userQuestion
                   then ""
                   else "\n<UserQuestion>\n" ++ userQuestion ++ "\n</UserQuestion>"
  return $ defaultPart ++ projPart ++ filePart ++ historyPart ++ userPart
  where
    formatHistoryMsg m =
      let r = T.unpack (role m)
          c = T.unpack (content m)
      in "<Message role=\"" ++ r ++ "\">\n" ++ c ++ "\n</Message>\n"

estimateContextCharSize :: Maybe Text -> String -> Maybe [(FilePath, Text)] -> [Message] -> String -> IO Int
estimateContextCharSize mbCustomSystem projPrompt mbFiles history q = do
  ctx <- assembleContext mbCustomSystem projPrompt mbFiles history q
  return (length ctx)

getContextTokenCount :: AppState -> IO Int
getContextTokenCount state = do
  projPrompt <- loadProjectPrompt
  let currentHistory = sessionHistory state
      files = loadedFiles state
      mbFiles = if null files then Nothing else Just files
      mbCustomSystem = customSystemPrompt state
  charSize <- estimateContextCharSize mbCustomSystem projPrompt mbFiles currentHistory ""
  return (charSize `div` 4)

getContextSizes :: Maybe Text -> String -> Maybe [(FilePath, Text)] -> [Message] -> String -> ContextSizes
getContextSizes mbCustomSystem projPrompt mbAttachedFiles history userQuestion =
  let basePrompt = case mbCustomSystem of
                     Just p  -> T.unpack p
                     Nothing -> defaultSystemPrompt
      defaultPrompt = "<SystemPrompt>\n" ++ basePrompt ++ "\n</SystemPrompt>"
      sysPromptLen = length defaultPrompt + (if null projPrompt then 0 else length projPrompt + 32)
      
      fileLen = case mbAttachedFiles of
                  Nothing -> 0
                  Just files -> sum (map (\(path, fileContent) -> length path + length (T.unpack fileContent) + 40) files)
                  
      historyLen = sum (map (\m -> length (T.unpack (role m)) + length (T.unpack (content m)) + 30) (filterLlmHistory history))
      
      userLen = if null userQuestion then 0 else length userQuestion + 32
  in ContextSizes sysPromptLen historyLen fileLen userLen

partitionHistForContext :: [Message] -> ([Message], Maybe [(FilePath, Text)], String)
partitionHistForContext msgs =
  let (beforeUser, afterUser) = spanLastUser msgs
  in case afterUser of
       [] -> (msgs, Nothing, "")
       (userMsg : post) ->
         let attached = attached_files userMsg
             question = T.unpack (content userMsg)
             history = beforeUser ++ post
         in (history, attached, question)
  where
    spanLastUser xs =
      let reversed = reverse xs
          (postReversed, userAndBefore) = span (\m -> role m /= "user") reversed
      in case userAndBefore of
           [] -> (xs, [])
           (u:bef) -> (reverse bef, u : reverse postReversed)

-- Automatic summarizer removed

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

    CmdContext -> do
      projPrompt <- liftIO loadProjectPrompt
      let currentHistory = sessionHistory state
          files = loadedFiles state
          mbFiles = if null files then Nothing else Just files
          mbCustomSystem = customSystemPrompt state
      ctx <- liftIO $ assembleContext mbCustomSystem projPrompt mbFiles currentHistory ""
      let charCount = length ctx
          tokenCount = charCount `div` 4
          msg = "=== Current Assembled Context ===\n"
             ++ ctx ++ "\n"
             ++ "=================================\n"
             ++ "Character Count: " ++ show charCount ++ " (~" ++ show tokenCount ++ " tokens / 8000 max)\n"
      return $ Right $ ResContext msg

    CmdSessionList -> return $ Right $ ResSessionList (sessions state) (currentSessionId state)

    CmdSessionNew mName -> do
      timeStr <- liftIO $ getFormattedTime "%Y%m%d_%H%M%S"
      let newId = SessionId ("session_" ++ timeStr)
          sName = SessionName (maybe ("Session " ++ timeStr) T.unpack mName)
          newS  = Session newId sName [] Nothing (Just 0)
          synced = syncActiveSession state
          infoMsg = Message "system" (T.pack ("已建立新會話: " ++ unSessionName sName)) Nothing Nothing Nothing
          newState = synced { sessions = sessions synced ++ [newS]
                            , currentSessionId = newId
                            , sessionHistory = [infoMsg]
                            , customSystemPrompt = Nothing
                            , historyClearIndex = 0
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
              infoMsg = Message "system" (T.pack ("已載入會話: " ++ unSessionName (sessionName targetS))) Nothing Nothing Nothing
              newState = synced { currentSessionId = sessionId targetS
                                , sessionHistory = messages targetS ++ [infoMsg]
                                , customSystemPrompt = sessionSystemPrompt targetS
                                , historyClearIndex = fromMaybe 0 (sessionClearIndex targetS)
                                }
              newStateSynced = syncActiveSession newState
          liftIO $ modifyIORef' (envState env) (const newStateSynced)
          liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
          return $ Right $ ResSessionLoad (sessionName targetS) (sessionId targetS)

    CmdSessionRename newName -> do
      let activeId = currentSessionId state
          infoMsg = Message "system" (T.pack ("已將當前會話重新命名為: " ++ newName)) Nothing Nothing Nothing
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
                  let infoMsg = Message "system" (T.pack ("已刪除當前會話，切換至: " ++ unSessionName (sessionName fb))) Nothing Nothing Nothing
                      newState = state { sessions = ss'
                                       , currentSessionId = sessionId fb
                                       , sessionHistory = messages fb ++ [infoMsg]
                                       , customSystemPrompt = sessionSystemPrompt fb
                                       , historyClearIndex = fromMaybe 0 (sessionClearIndex fb)
                                       }
                      newStateSynced = syncActiveSession newState
                  liftIO $ modifyIORef' (envState env) (const newStateSynced)
                  liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
                  return $ Right $ ResSessionDelete (sessionName targetS) (sessionId fb)
                [] -> return $ Left "沒有可切換的會話。"
              else do
                let infoMsg = Message "system" (T.pack ("已刪除會話: " ++ unSessionName (sessionName targetS))) Nothing Nothing Nothing
                    newState = state { sessions = ss', sessionHistory = sessionHistory state ++ [infoMsg] }
                    newStateSynced = syncActiveSession newState
                liftIO $ modifyIORef' (envState env) (const newStateSynced)
                liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
                return $ Right $ ResSessionDelete (sessionName targetS) activeId

    CmdSessionFork mName -> do
      timeStr <- liftIO $ getFormattedTime "%Y%m%d_%H%M%S"
      let newId = SessionId ("session_fork_" ++ timeStr)
          sName = SessionName (maybe ("Fork " ++ timeStr) T.unpack mName)
          newS  = Session newId sName (sessionHistory state) (customSystemPrompt state) (Just (historyClearIndex state))
          synced = syncActiveSession state
          infoMsg = Message "system" (T.pack ("已複製當前會話至: " ++ unSessionName sName)) Nothing Nothing Nothing
          newState = synced { sessions = sessions synced ++ [newS]
                            , currentSessionId = newId
                            , sessionHistory = sessionHistory synced ++ [infoMsg]
                            , customSystemPrompt = customSystemPrompt synced
                            , historyClearIndex = historyClearIndex synced
                            }
          newStateSynced = syncActiveSession newState
      liftIO $ modifyIORef' (envState env) (const newStateSynced)
      liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
      return $ Right $ ResSessionFork sName newId

    CmdFile path _mQuest -> do
      readRes <- liftIO $ readTextFileStrict path
      case readRes of
        Left err -> return $ Left $ "讀取檔案失敗: " ++ err
        Right fileContent -> do
          let infoMsg = Message "system" (T.pack ("已載入檔案: " ++ path)) Nothing Nothing Nothing
              currentFiles = loadedFiles state
              newFiles = if path `elem` map fst currentFiles
                           then currentFiles
                           else currentFiles ++ [(path, fileContent)]
              newState = state { loadedFiles = newFiles
                               , sessionHistory = sessionHistory state ++ [infoMsg]
                               }
              newStateSynced = syncActiveSession newState
          liftIO $ modifyIORef' (envState env) (const newStateSynced)
          liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
          return $ Right $ ResFile path

    CmdRun cmdStr -> do
      let infoMsg = Message "system" (T.pack ("正在執行命令: " ++ cmdStr)) Nothing Nothing Nothing
          newState = state { sessionHistory = sessionHistory state ++ [infoMsg] }
          newStateSynced = syncActiveSession newState
      liftIO $ modifyIORef' (envState env) (const newStateSynced)
      liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
      
      runRes <- liftIO $ runShellCommand cmdStr
      case runRes of
        Left err -> return $ Left $ "指令執行錯誤: " ++ err
        Right output -> do
          stateAfter <- liftIO $ readIORef (envState env)
          let resultMsg = Message "system" (T.pack ("指令執行結果:\n" ++ output)) Nothing Nothing Nothing
              newStateFinal = stateAfter { sessionHistory = sessionHistory stateAfter ++ [resultMsg] }
              newStateFinalSynced = syncActiveSession newStateFinal
          liftIO $ modifyIORef' (envState env) (const newStateFinalSynced)
          liftIO $ saveSessions (sessionsFilePath config) (sessions newStateFinalSynced)
          return $ Right $ ResRun output

    CmdClear -> do
      let newState = state { historyClearIndex = length (sessionHistory state) }
          newStateSynced = syncActiveSession newState
      liftIO $ modifyIORef' (envState env) (const newStateSynced)
      liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
      return $ Right ResClear

    CmdSummary mbInstruction -> do
      let hist = filterLlmHistory (sessionHistory state)
      if null hist
        then return $ Left "沒有對話歷史可以總結。"
        else do
          let formatMsg m = T.unpack (role m) ++ ": " ++ T.unpack (content m) ++ "\n"
              formattedHist = concatMap formatMsg hist
              basePrompt = "Please summarize the following conversation history between User and Assistant, preserving all key facts, decisions, and context in a concise summary:\n\n" ++ formattedHist
              finalPrompt = case mbInstruction of
                              Nothing -> basePrompt
                              Just inst -> basePrompt ++ "\n\nCustom Instruction for Summarization: " ++ T.unpack inst
              sumMsg = Message "user" (T.pack finalPrompt) Nothing Nothing Nothing
              modelIdVal = selectedModel state
          
          res <- liftIO $ fetchChatResponseRaw config modelIdVal [sumMsg]
          case res of
            Left err -> return $ Left $ "總結失敗: " ++ err
            Right resp -> do
              let summaryText = content resp
                  summaryMsg = Message "system" ("System Context Summary of past messages:\n" <> summaryText) Nothing Nothing Nothing
                  newState = state { sessionHistory = [summaryMsg]
                                   , historyClearIndex = 0
                                   }
                  newStateSynced = syncActiveSession newState
              liftIO $ modifyIORef' (envState env) (const newStateSynced)
              liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
              return $ Right $ ResSummary (content resp)

    CmdPrompt mbText -> do
      case mbText of
        Nothing -> do
          return $ Right $ ResPrompt (customSystemPrompt state)
        Just "clear" -> do
          let newState = state { customSystemPrompt = Nothing }
              newStateSynced = syncActiveSession newState
          liftIO $ modifyIORef' (envState env) (const newStateSynced)
          liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
          return $ Right $ ResPrompt Nothing
        Just "default" -> do
          let newState = state { customSystemPrompt = Nothing }
              newStateSynced = syncActiveSession newState
          liftIO $ modifyIORef' (envState env) (const newStateSynced)
          liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
          return $ Right $ ResPrompt Nothing
        Just text -> do
          let newState = state { customSystemPrompt = Just text }
              newStateSynced = syncActiveSession newState
          liftIO $ modifyIORef' (envState env) (const newStateSynced)
          liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
          return $ Right $ ResPrompt (Just text)

    CmdUnfile mArg -> do
      let currentFiles = loadedFiles state
      case currentFiles of
        [] -> do
          let msg = Message "system" "目前沒有夾帶任何檔案。" Nothing Nothing Nothing
              newState = state { sessionHistory = sessionHistory state ++ [msg] }
              newStateSynced = syncActiveSession newState
          liftIO $ modifyIORef' (envState env) (const newStateSynced)
          liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
          return $ Right (ResUnfileList [])
        _ -> case mArg of
          Nothing -> do
            return $ Right (ResUnfileList (zip [1..] (map fst currentFiles)))
          Just arg -> do
            let mIdx = Text.Read.readMaybe arg :: Maybe Int
                matchedByIndex = case mIdx of
                  Just idx | idx >= 1 && idx <= length currentFiles -> Just (idx - 1)
                  _ -> Nothing
                
                matchedByPath = case matchedByIndex of
                  Just idx -> Just idx
                  Nothing ->
                    let matches = filter (\(_, (path, _)) -> arg == path || arg == takeFileName path) (zip [0..] currentFiles)
                    in case matches of
                         [(i, _)] -> Just i
                         _        -> Nothing
            
            case matchedByPath of
              Just idx -> do
                let (removedPath, _) = currentFiles !! idx
                    remaining = take idx currentFiles ++ drop (idx + 1) currentFiles
                    msg = Message "system" (T.pack ("已移除夾帶檔案: " ++ removedPath)) Nothing Nothing Nothing
                    newState = state { loadedFiles = remaining
                                     , sessionHistory = sessionHistory state ++ [msg]
                                     }
                    newStateSynced = syncActiveSession newState
                liftIO $ modifyIORef' (envState env) (const newStateSynced)
                liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)
                return $ Right (ResUnfileSuccess removedPath)
              Nothing -> do
                return $ Right (ResUnfileList (zip [1..] (map fst currentFiles)))

-- ---------------------------------------------------------------------------
-- Agent Chat Handler (Recursive Tool Calling Loop)
-- ---------------------------------------------------------------------------

executeAgentChat :: Text -> (Message -> IO ()) -> AgentM (Either String Message)
executeAgentChat userText stepCallback = do
  env <- ask
  let config = envConfig env
  state <- liftIO $ readIORef (envState env)
  
  -- 1. Merging loadedFiles if present
  let (userTextFinal, mbAttachedFiles, updatedStatePre) = case loadedFiles state of
        [] -> (userText, Nothing, state)
        files ->
          let finalTxt = if T.null (T.strip userText) then "請分析夾帶的檔案內容。" else userText
          in (finalTxt, Just files, state { loadedFiles = [] })
        
  let userMsg = Message "user" userTextFinal Nothing Nothing mbAttachedFiles
      hist' = sessionHistory updatedStatePre ++ [userMsg]
      updatedState = updatedStatePre { sessionHistory = hist' }
      updatedStateSynced = syncActiveSession updatedState
      
  liftIO $ modifyIORef' (envState env) (const updatedStateSynced)
  liftIO $ saveSessions (sessionsFilePath config) (sessions updatedStateSynced)
  
  -- Send userMsg to UI callback
  liftIO $ stepCallback userMsg
  
  -- 2. Token Limit Check and Context Occupancy warning
  projPrompt <- liftIO loadProjectPrompt
  let (chatHistory, attachedFilesList, userQuestion) = partitionHistForContext hist'
      cleanChatHistory = filterLlmHistory chatHistory
      mbCustomSystem = customSystemPrompt updatedStateSynced
  
  charSize <- liftIO $ estimateContextCharSize mbCustomSystem projPrompt attachedFilesList cleanChatHistory userQuestion
  let tokenCount = charSize `div` 4
      threshold = 5600 -- 70% of 8000
      modelIdVal = selectedModel updatedStateSynced
  
  when (tokenCount > threshold) $ do
    let warnMsg = Message "system" "⚠️ 偵測到 Context 觸及 70% 上限。建議使用 `/summary` 指令手動壓縮對話歷史以防超出上限。" Nothing Nothing Nothing
    liftIO $ stepCallback warnMsg
 
  -- 3. Agentic Tool Calling loop with Assembled XML Context as separate messages
  let (finalHistoryForCtx, finalAttached, _) = partitionHistForContext hist'
      basePrompt = case mbCustomSystem of
                     Just p  -> T.unpack p
                     Nothing -> defaultSystemPrompt
      sysMsgContent = "<SystemPrompt>\n" ++ basePrompt ++ (if null projPrompt then "" else "\n\nProject Instructions:\n" ++ projPrompt) ++ "\n</SystemPrompt>"
      sysMsg = Message "system" (T.pack sysMsgContent) Nothing Nothing Nothing
      
      -- Format the current user question with <UserQuestion> tags
      userMsgTagged = userMsg { content = T.pack ("<UserQuestion>\n" ++ T.unpack (content userMsg) ++ "\n</UserQuestion>")
                              , attached_files = finalAttached
                              }
      cleanHistory = filterLlmHistory finalHistoryForCtx
      initialMsgs = sysMsg : cleanHistory ++ [userMsgTagged]
      
  res <- runAgentLoop config modelIdVal initialMsgs
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
                                       Right fileText -> return fileText
                                   _ -> return "Error: Missing or invalid 'path' argument."
                                   
                               unknown -> return $ T.pack ("Error: Unknown tool '" ++ T.unpack unknown ++ "'")
                             
                             let toolMsg = Message "tool" toolOutput Nothing (Just callId) Nothing
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
          -- Notify callback about final message
          liftIO $ stepCallback msg
          -- Append final message to session history
          stateAfter <- liftIO $ readIORef (envState env)
          let finalState = stateAfter { sessionHistory = sessionHistory stateAfter ++ [msg] }
              finalStateSynced = syncActiveSession finalState
          liftIO $ modifyIORef' (envState env) (const finalStateSynced)
          liftIO $ saveSessions (sessionsFilePath config) (sessions finalStateSynced)
          return $ Right msg

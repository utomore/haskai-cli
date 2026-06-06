{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( defaultMain
  , buildSystemMessage
  , parseSessionIndexOrName
  , currentSessionName
  , syncActiveSession
  ) where

import Types
import Memory
import Ollama

import System.Console.Haskeline (runInputT, defaultSettings, Settings(..), completeWord, Completion(..), getInputLine)
import System.Console.ANSI
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(NoBuffering))
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ask, lift)
import Data.IORef (readIORef, modifyIORef', newIORef)
import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime, formatTime, defaultTimeLocale)

-- Monadic helpers for Env and AppState access
getConfig :: AppM Config
getConfig = do
  env <- ask
  return (envConfig env)

getState :: AppM AppState
getState = do
  env <- ask
  liftIO $ readIORef (envState env)

modifyState :: (AppState -> AppState) -> AppM ()
modifyState f = do
  env <- ask
  liftIO $ modifyIORef' (envState env) f

-- Build the system message injecting long-term memories
buildSystemMessage :: [T.Text] -> Message
buildSystemMessage memories =
  let basePrompt = "You are a helpful and intelligent AI CLI assistant. You interact with the user via a terminal interface."
      memoryPrompt = if null memories
        then ""
        else "\nHere are facts you remember about the user (use this context to personalize your responses when relevant):\n"
             ++ unlines (map (\m -> "- " ++ T.unpack m) memories)
      fullContent = basePrompt ++ memoryPrompt
  in Message "system" (T.pack fullContent)

-- Print help menu
printHelp :: IO ()
printHelp = do
  putStrLn "=============================================================="
  putStrLn " HaskAI CLI Help"
  putStrLn "=============================================================="
  putStrLn " /help             - Show this help message"
  putStrLn " /memories         - List all stored long-term memories"
  putStrLn " /remember <fact>  - Save a new fact to long-term memory"
  putStrLn " /forget <index>   - Delete a memory by its list index"
  putStrLn " /exit or /quit    - Exit the CLI tool"
  putStrLn " /session list     - List all sessions"
  putStrLn " /session new <n>  - Start a new session"
  putStrLn " /session load <n> - Load/switch to a session by index or name"
  putStrLn " /session rename <r>- Rename the current session"
  putStrLn " /session delete <d>- Delete a session by index or name"
  putStrLn " /session fork <f> - Fork the current session"
  putStrLn "=============================================================="

-- Print memories
printMemories :: [T.Text] -> IO ()
printMemories mems = do
  putStrLn "=============================================================="
  putStrLn " Stored Memories:"
  putStrLn "=============================================================="
  if null mems
    then putStrLn " No memories stored yet. Use /remember <fact> to add some!"
    else mapM_ (\(i, m) -> putStrLn $ " [" ++ show i ++ "] " ++ T.unpack m) (zip [1 :: Int ..] mems)
  putStrLn "=============================================================="

-- Helper to get formatted local time string
getFormattedTime :: String -> IO String
getFormattedTime fmt = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let localTime = utcToLocalTime tz now
  return $ formatTime defaultTimeLocale fmt localTime

-- Select model on startup
selectModelMenu :: [String] -> IO String
selectModelMenu [] = do
  putStrLn "No models found from Ollama server."
  putStrLn "Using default fallback model: gemma4:12b"
  return "gemma4:12b"
selectModelMenu models@(defaultModel:_) = do
  putStrLn "Available Ollama models:"
  mapM_ (\(i, m) -> putStrLn $ " [" ++ show i ++ "] " ++ m) (zip [1 :: Int ..] models)
  putStr "Select a model [1]: "
  hFlush stdout
  input <- getLine
  let trimmed = T.unpack $ T.strip $ T.pack input
  if null trimmed
    then return defaultModel
    else case readMaybe trimmed :: Maybe Int of
      Just idx ->
        if idx >= 1 && idx <= length models
          then return (models !! (idx - 1))
          else do
            putStrLn $ "Invalid index. Using default: " ++ defaultModel
            return defaultModel
      Nothing -> do
        putStrLn $ "Invalid input. Using default: " ++ defaultModel
        return defaultModel

-- Helper to find a session by index or name
parseSessionIndexOrName :: String -> [Session] -> Maybe Session
parseSessionIndexOrName input ss =
  case readMaybe input :: Maybe Int of
    Just idx ->
      if idx >= 1 && idx <= length ss
        then Just (ss !! (idx - 1))
        else Nothing
    Nothing ->
      case filter (\s -> sessionName s == input || sessionId s == input) ss of
        (s:_) -> Just s
        []    -> Nothing

-- Helper to get current session name
currentSessionName :: AppState -> String
currentSessionName state =
  let activeId = currentSessionId state
  in case filter (\s -> sessionId s == activeId) (sessions state) of
       (s:_) -> sessionName s
       []    -> "Unknown Session"

-- Helper to synchronize current active session history into sessions list
syncActiveSession :: AppState -> AppState
syncActiveSession state =
  let activeId = currentSessionId state
      history = sessionHistory state
      updatedSs = map (\s -> if sessionId s == activeId then s { messages = history } else s) (sessions state)
  in state { sessions = updatedSs }

-- Session Management Commands
listSessionsCmd :: AppM ()
listSessionsCmd = do
  state <- getState
  let activeId = currentSessionId state
      ss = sessions state
  liftIO $ do
    putStrLn "=============================================================="
    putStrLn " Sessions list:"
    putStrLn "=============================================================="
    mapM_ (\(i, s) -> do
      let prefix = if sessionId s == activeId then "* " else "  "
      putStrLn $ prefix ++ "[" ++ show i ++ "] " ++ sessionName s ++ " (" ++ sessionId s ++ ") - " ++ show (length (messages s)) ++ " messages"
      ) (zip [1 :: Int ..] ss)
    putStrLn "=============================================================="

newSessionCmd :: String -> AppM ()
newSessionCmd nameInput = do
  config <- getConfig
  state <- getState
  timeStr <- liftIO $ getFormattedTime "%Y%m%d_%H%M%S"
  let newId = "session_" ++ timeStr
      sName = if null nameInput then "Session " ++ timeStr else nameInput
      newS = Session newId sName []
      syncedState = syncActiveSession state
      newSessionsList = syncedState { sessions = sessions syncedState ++ [newS]
                                    , currentSessionId = newId
                                    , sessionHistory = []
                                    }
  modifyState (const newSessionsList)
  liftIO $ saveSessions (sessionsFilePath config) (sessions newSessionsList)
  showSessionHistory

loadSessionCmd :: String -> AppM ()
loadSessionCmd input = do
  config <- getConfig
  state <- getState
  let ss = sessions state
  case parseSessionIndexOrName input ss of
    Just targetS -> do
      let syncedState = syncActiveSession state
          newState = syncedState { currentSessionId = sessionId targetS
                                 , sessionHistory = messages targetS
                                 }
      modifyState (const newState)
      liftIO $ saveSessions (sessionsFilePath config) (sessions newState)
      showSessionHistory
    Nothing -> do
      liftIO $ putStrLn $ "Error: Session not found. Usage: /session load <index_or_name>"

renameSessionCmd :: String -> AppM ()
renameSessionCmd newName = do
  if null newName
    then liftIO $ putStrLn "Error: Name cannot be empty. Usage: /session rename <new_name>"
    else do
      config <- getConfig
      state <- getState
      let activeId = currentSessionId state
          updatedSs = map (\s -> if sessionId s == activeId then s { sessionName = newName } else s) (sessions state)
          newState = state { sessions = updatedSs }
      modifyState (const newState)
      liftIO $ saveSessions (sessionsFilePath config) updatedSs
      liftIO $ redrawLayout newName (selectedModel newState) (length (longTermMemories newState))

deleteSessionCmd :: String -> AppM ()
deleteSessionCmd input = do
  config <- getConfig
  state <- getState
  let ss = sessions state
  if length ss <= 1
    then liftIO $ putStrLn "Error: Cannot delete the only remaining session. Create a new one first."
    else case parseSessionIndexOrName input ss of
      Just targetS -> do
        let targetId = sessionId targetS
            activeId = currentSessionId state
            newSs = filter (\s -> sessionId s /= targetId) ss
        if targetId == activeId
          then case newSs of
            (fallbackS:_) -> do
              let newState = state { sessions = newSs
                                   , currentSessionId = sessionId fallbackS
                                   , sessionHistory = messages fallbackS
                                   }
              modifyState (const newState)
              liftIO $ saveSessions (sessionsFilePath config) newSs
              showSessionHistory
            [] -> return ()
          else do
            let newState = state { sessions = newSs }
            modifyState (const newState)
            liftIO $ saveSessions (sessionsFilePath config) newSs
            liftIO $ redrawLayout (currentSessionName newState) (selectedModel newState) (length (longTermMemories newState))
      Nothing -> do
        liftIO $ putStrLn "Error: Session not found. Usage: /session delete <index_or_name>"

forkSessionCmd :: String -> AppM ()
forkSessionCmd nameInput = do
  config <- getConfig
  state <- getState
  timeStr <- liftIO $ getFormattedTime "%Y%m%d_%H%M%S"
  let newId = "session_fork_" ++ timeStr
      sName = if null nameInput then "Fork " ++ timeStr else nameInput
      newS = Session newId sName (sessionHistory state)
      syncedState = syncActiveSession state
      newSessionsList = syncedState { sessions = sessions syncedState ++ [newS]
                                    , currentSessionId = newId
                                    }
  modifyState (const newSessionsList)
  liftIO $ saveSessions (sessionsFilePath config) (sessions newSessionsList)
  showSessionHistory

-- UI Layout drawing helpers
getRowsOrDefault :: IO Int
getRowsOrDefault = do
  mSize <- getTerminalSize
  case mSize of
    Just (rows, _) -> return rows
    Nothing        -> return 24

-- Redraw the fixed header and separator lines without clearing scroll content
redrawLayout :: String -> String -> Int -> IO ()
redrawLayout sessionNameVal modelName numMemories = do
  rows <- getRowsOrDefault
  -- Header info bar (Rows 0 and 1, 0-indexed)
  setCursorPosition 0 0
  setSGR [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
  putStrLn "======================================================================"
  putStr " HaskAI CLI "
  setSGR [Reset]
  putStr " | "
  setSGR [SetColor Foreground Vivid Cyan]
  putStr $ "Session: " ++ sessionNameVal
  setSGR [Reset]
  putStr " | "
  setSGR [SetColor Foreground Vivid Cyan]
  putStr $ "Model: " ++ modelName
  setSGR [Reset]
  putStr " | "
  setSGR [SetColor Foreground Vivid Green]
  putStr $ "Memories: " ++ show numMemories
  setSGR [Reset]
  putStrLn " | Type /help for help"
  
  -- Top Separator at Row 2 (0-indexed)
  setCursorPosition 2 0
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn "──────────────────────────────────────────────────────────────────────"
  setSGR [Reset]
  
  -- Bottom Separator at Row rows - 2 (0-indexed)
  setCursorPosition (rows - 2) 0
  setSGR [SetColor Foreground Vivid Blue]
  putStr "──────────────────────────────────────────────────────────────────────"
  setSGR [Reset]
  hFlush stdout

initTerminalScreen :: String -> String -> Int -> IO ()
initTerminalScreen sessionNameVal modelName numMemories = do
  rows <- getRowsOrDefault
  clearScreen
  setCursorPosition 0 0
  -- Clear the middle scroll area (Rows 3 to rows - 3)
  mapM_ (\r -> setCursorPosition r 0 >> putStr "\ESC[2K") [3 .. rows - 3]
  redrawLayout sessionNameVal modelName numMemories
  -- Position cursor at prompt line (0-indexed: rows - 1)
  setCursorPosition (rows - 1) 0
  hFlush stdout

-- Clears screen, draws layout, and displays current session history inside the scroll region
showSessionHistory :: AppM ()
showSessionHistory = do
  state <- getState
  rows <- liftIO getRowsOrDefault
  let activeS = currentSessionName state
      model = selectedModel state
      memsCount = length (longTermMemories state)
  liftIO $ do
    clearScreen
    redrawLayout activeS model memsCount
    -- Set scrolling region
    putStr $ "\ESC[4;" ++ show (rows - 2) ++ "r"
    -- Position cursor inside scrolling region at the bottom
    setCursorPosition (rows - 3) 0
    hFlush stdout
  
  -- Print messages in history
  let history = sessionHistory state
  liftIO $ do
    mapM_ (\msg -> do
      if role msg == "user"
        then do
          setSGR [SetColor Foreground Vivid Green]
          putStr "User: "
          setSGR [Reset]
          putStrLn (T.unpack (content msg))
        else if role msg == "assistant"
          then do
            setSGR [SetColor Foreground Vivid Cyan]
            putStr "Assistant: "
            setSGR [Reset]
            putStrLn (T.unpack (content msg))
          else return ()
      ) history
    -- Reset scrolling region
    putStr "\ESC[r"
    -- Clear and position cursor on prompt line
    setCursorPosition (rows - 1) 0
    putStr "\ESC[2K"
    hFlush stdout

-- Command interpreter
handleCommand :: String -> AppM Bool
handleCommand cmd
  | cmd == "/exit" || cmd == "/quit" = return False
  | cmd == "/help" = do
      liftIO printHelp
      return True
  | cmd == "/memories" = do
      state <- getState
      liftIO $ printMemories (longTermMemories state)
      return True
  | cmd == "/remember" = do
      liftIO $ putStrLn "Error: Please specify a fact to remember. Usage: /remember <fact>"
      return True
  | "/remember " `isPrefixOf` cmd = do
      config <- getConfig
      state <- getState
      let fact = T.strip $ T.pack $ drop 10 cmd
      if T.null fact
        then liftIO $ putStrLn "Error: Memory cannot be empty. Usage: /remember <fact>"
        else do
          let newMems = longTermMemories state ++ [fact]
          modifyState (\s -> s { longTermMemories = newMems })
          liftIO $ do
            saveMemories (memoryFilePath config) newMems
            putStrLn "Memory saved!"
      return True
  | cmd == "/forget" = do
      liftIO $ putStrLn "Error: Please specify the memory index to forget. Usage: /forget <index>"
      return True
  | "/forget " `isPrefixOf` cmd = do
      config <- getConfig
      state <- getState
      let indexStr = drop 8 cmd
      case readMaybe indexStr :: Maybe Int of
        Just idx -> do
          let mems = longTermMemories state
          if idx >= 1 && idx <= length mems
            then do
              let (left, right) = splitAt (idx - 1) mems
                  newMems = left ++ drop 1 right
              modifyState (\s -> s { longTermMemories = newMems })
              liftIO $ do
                saveMemories (memoryFilePath config) newMems
                putStrLn $ "Memory [" ++ show idx ++ "] forgotten."
            else liftIO $ putStrLn $ "Error: Index out of range. Valid range: 1 to " ++ show (length mems)
        Nothing -> liftIO $ putStrLn "Error: Invalid index. Usage: /forget <index>"
      return True
  | cmd == "/session list" = do
      listSessionsCmd
      return True
  | "/session new" `isPrefixOf` cmd = do
      let nameInput = T.unpack $ T.strip $ T.pack $ drop 12 cmd
      newSessionCmd nameInput
      return True
  | "/session load " `isPrefixOf` cmd = do
      let nameInput = T.unpack $ T.strip $ T.pack $ drop 14 cmd
      loadSessionCmd nameInput
      return True
  | cmd == "/session load" = do
      liftIO $ putStrLn "Error: Please specify the session index or name to load. Usage: /session load <index_or_name>"
      return True
  | "/session rename " `isPrefixOf` cmd = do
      let nameInput = T.unpack $ T.strip $ T.pack $ drop 16 cmd
      renameSessionCmd nameInput
      return True
  | cmd == "/session rename" = do
      liftIO $ putStrLn "Error: Please specify the new session name. Usage: /session rename <new_name>"
      return True
  | "/session delete " `isPrefixOf` cmd = do
      let nameInput = T.unpack $ T.strip $ T.pack $ drop 16 cmd
      deleteSessionCmd nameInput
      return True
  | cmd == "/session delete" = do
      liftIO $ putStrLn "Error: Please specify the session index or name to delete. Usage: /session delete <index_or_name>"
      return True
  | "/session fork " `isPrefixOf` cmd = do
      let nameInput = T.unpack $ T.strip $ T.pack $ drop 14 cmd
      forkSessionCmd nameInput
      return True
  | cmd == "/session fork" = do
      forkSessionCmd ""
      return True
  | otherwise = do
      liftIO $ putStrLn $ "Unknown command: " ++ cmd ++ ". Type /help for assistance."
      return True

-- Chat logic
handleChat :: T.Text -> AppM ()
handleChat userText = do
  config <- getConfig
  state <- getState
  let userMsg = Message "user" userText
      updatedHistory = sessionHistory state ++ [userMsg]
      systemMsg = buildSystemMessage (longTermMemories state)
      allMessages = systemMsg : updatedHistory

  rows <- liftIO getRowsOrDefault
  let activeS = currentSessionName state
      model = selectedModel state
      memsCount = length (longTermMemories state)
      promptStr = "haskai-cli [" ++ activeS ++ "]> "
      promptCol = length promptStr

  liftIO $ do
    -- 1. Redraw status headers and separators to fix the 1-line scroll caused by the Enter key
    redrawLayout activeS model memsCount
    
    -- 2. Clear and display the prompt line (row rows - 1)
    setCursorPosition (rows - 1) 0
    putStr "\ESC[2K"
    putStr promptStr
    
    -- 3. Set scrolling region to lines 4 to rows - 2
    putStr $ "\ESC[4;" ++ show (rows - 2) ++ "r"
    
    -- 4. Position cursor inside the scrolling region at the bottom (rows - 3)
    setCursorPosition (rows - 3) 0
    
    -- 5. Print User query
    setSGR [SetColor Foreground Vivid Green]
    putStr "User: "
    setSGR [Reset]
    putStrLn (T.unpack userText)
    hFlush stdout

  -- Stream chat from Ollama, passing rows and promptCol for prompt-line spinner rendering
  streamResult <- liftIO $ streamChat config model allMessages rows promptCol
  
  liftIO $ do
    putChar '\n'
    -- Reset scrolling region
    putStr "\ESC[r"
    
    -- Clear and position cursor on prompt line
    setCursorPosition (rows - 1) 0
    putStr "\ESC[2K"
    hFlush stdout

  case streamResult of
    Left errMsg -> do
      liftIO $ do
        setCursorPosition (rows - 1) 0
        putStr "\ESC[2K"
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ "Error calling LLM: " ++ errMsg
        setSGR [Reset]
        hFlush stdout
    Right assistantText -> do
      let assistantMsg = Message "assistant" assistantText
          newState = state { sessionHistory = updatedHistory ++ [assistantMsg] }
          newStateSynced = syncActiveSession newState
      modifyState (const newStateSynced)
      liftIO $ saveSessions (sessionsFilePath config) (sessions newStateSynced)

-- Interactive Haskeline loop
loop :: AppM ()
loop = do
  state <- getState
  rows <- liftIO getRowsOrDefault
  let promptStr = "haskai-cli [" ++ currentSessionName state ++ "]> "
  minput <- lift $ getInputLine promptStr
  case minput of
    Nothing -> liftIO $ putStrLn "\nGoodbye!"
    Just input -> do
      let trimmed = T.unpack $ T.strip $ T.pack input
      if null trimmed
        then loop
        else if "/" `isPrefixOf` trimmed
          then do
            state' <- getState
            liftIO $ do
              -- Redraw layout to fix the Enter scroll
              redrawLayout (currentSessionName state') (selectedModel state') (length (longTermMemories state'))
              -- Clear and draw prompt line
              setCursorPosition (rows - 1) 0
              putStr "\ESC[2K"
              putStr promptStr
              -- Set scrolling region for command output
              putStr $ "\ESC[4;" ++ show (rows - 2) ++ "r"
              setCursorPosition (rows - 3) 0
              hFlush stdout
              
            shouldContinue <- handleCommand trimmed
            
            liftIO $ do
              -- Reset scrolling region
              putStr "\ESC[r"
              -- Clear prompt line and place cursor
              setCursorPosition (rows - 1) 0
              putStr "\ESC[2K"
              hFlush stdout
              
            if shouldContinue
              then loop
              else liftIO $ putStrLn "Goodbye!"
          else do
            handleChat (T.pack trimmed)
            loop

-- Haskeline custom tab completer settings
haskaiSettings :: Settings IO
haskaiSettings = (defaultSettings :: Settings IO)
  { complete = completeWord Nothing " \t" (\s -> return $ map (\c -> Completion c c True) (filter (s `isPrefixOf`) allCmds))
  }
  where
    allCmds = [ "/help", "/memories", "/remember", "/forget", "/exit", "/quit"
              , "/session list", "/session new", "/session load", "/session rename", "/session delete", "/session fork"
              ]

-- Application Entry Point
defaultMain :: IO ()
defaultMain = do
  hSetBuffering stdout NoBuffering
  cwd <- getCurrentDirectory
  let memoryPath = cwd </> ".config" </> "memory.json"
      sessionsPath = cwd </> ".config" </> "sessions.json"
      config = Config "http://localhost:11434" memoryPath sessionsPath

  -- Ensure config directory exists
  ensureConfigDirExists memoryPath
  ensureConfigDirExists sessionsPath

  -- Load long-term memories and sessions
  mems <- loadMemories memoryPath
  loadedSs <- loadSessions sessionsPath

  -- If there are no sessions, initialize with a default one
  (initialSessions, currentId) <- if null loadedSs
    then do
      let defSession = Session "default" "Default Session" []
      saveSessions sessionsPath [defSession]
      return ([defSession], "default")
    else case loadedSs of
      (firstS:_) -> return (loadedSs, sessionId firstS)
      []         -> return ([], "default")

  -- Find active session history
  let activeHistory = case filter (\s -> sessionId s == currentId) initialSessions of
        (s:_) -> messages s
        []    -> []

  putStrLn "Checking available Ollama models..."
  models <- fetchModels (ollamaBaseUrl config)

  selected <- selectModelMenu models
  
  let activeSName = case filter (\s -> sessionId s == currentId) initialSessions of
                      (s:_) -> sessionName s
                      []    -> "Default Session"

  let initialState = AppState selected activeHistory mems currentId initialSessions

  -- Initialize thread-safe state ref
  stateRef <- newIORef initialState
  let env = Env config stateRef

  -- Clear screen, show header, and run Haskeline prompt loop inside AppM
  runInputT haskaiSettings (runReaderT (liftIO (initTerminalScreen activeSName selected (length mems)) >> loop) env)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Types
import Memory
import Ollama (fetchModels)
import Controller
import Lib (parseCommand, currentSessionName)
import AppConfig
import Data.Maybe (fromMaybe)

import System.Console.Haskeline (runInputT, defaultSettings, Settings(..), completeWord, Completion(..), getInputLine, InputT)
import System.Console.ANSI
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(NoBuffering))
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ask, lift)
import Data.IORef (readIORef, modifyIORef', newIORef)
import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Control.Exception (finally, try, SomeException)

-- ---------------------------------------------------------------------------
-- Spinner
-- ---------------------------------------------------------------------------

startSpinner :: Int -> Int -> Int -> IO ThreadId
startSpinner rows promptCol delayMicros = forkIO spinnerLoop
  where
    frames :: [String]
    frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
    spinnerLoop = mapM_ (\f -> do
        putStr "\ESC[s"
        setCursorPosition (rows - 1) promptCol
        putStr "\ESC[K"
        setSGR [SetColor Foreground Vivid Yellow]
        putStr (f ++ " Thinking...")
        setSGR [Reset]
        putStr "\ESC[u"
        hFlush stdout
        threadDelay delayMicros
        ) (cycle frames)

stopSpinner :: Int -> Int -> ThreadId -> IO ()
stopSpinner rows promptCol tid = do
  killThread tid
  putStr "\ESC[s"
  setCursorPosition (rows - 1) promptCol
  putStr "\ESC[K"
  putStr "\ESC[u"
  hFlush stdout

-- ---------------------------------------------------------------------------
-- UI Layout Helpers
-- ---------------------------------------------------------------------------

getRowsOrDefault :: IO Int
getRowsOrDefault = do
  mSize <- getTerminalSize
  case mSize of
    Just (rows, _) -> return rows
    Nothing        -> return 24

redrawLayout :: String -> String -> Int -> IO ()
redrawLayout sessionNameVal modelName numMemories = do
  rows <- getRowsOrDefault
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
  setCursorPosition 2 0
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn "──────────────────────────────────────────────────────────────────────"
  setSGR [Reset]
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
  mapM_ (\r -> setCursorPosition r 0 >> putStr "\ESC[2K") [3 .. rows - 3]
  redrawLayout sessionNameVal modelName numMemories
  setCursorPosition (rows - 1) 0
  hFlush stdout

showSessionHistory :: AppM ()
showSessionHistory = do
  env <- ask
  state <- liftIO $ readIORef (envState env)
  rows  <- liftIO getRowsOrDefault
  let activeS    = currentSessionName state
      model      = unModelId (selectedModel state)
      memsCount  = length (longTermMemories state)
  liftIO $ do
    clearScreen
    redrawLayout activeS model memsCount
    putStr $ "\ESC[4;" ++ show (rows - 2) ++ "r"
    setCursorPosition (rows - 3) 0
    hFlush stdout
  let history = sessionHistory state
  liftIO $ do
    mapM_ (\msg ->
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
          else if role msg == "system"
            then do
              setSGR [SetColor Foreground Vivid Yellow]
              putStr "System: "
              setSGR [Reset]
              putStrLn (T.unpack (content msg))
            else if role msg == "tool"
              then do
                setSGR [SetColor Foreground Vivid Magenta]
                putStr "Tool Output: "
                setSGR [Reset]
                putStrLn (T.unpack (content msg))
              else return ()
      ) history
    putStr "\ESC[r"
    setCursorPosition (rows - 1) 0
    putStr "\ESC[2K"
    hFlush stdout

-- ---------------------------------------------------------------------------
-- Interactive Model Selection
-- ---------------------------------------------------------------------------

selectOllamaModel :: Config -> [String] -> IO ModelId
selectOllamaModel config [] = do
  let fb = unModelId (fallbackModel config)
  putStrLn "No models found from Ollama server."
  putStrLn $ "Using default fallback model: " ++ fb
  return (fallbackModel config)
selectOllamaModel _ models@(def:_) = do
  putStrLn "Available Ollama models:"
  mapM_ (\(i, m) -> putStrLn $ " [" ++ show i ++ "] " ++ m) (zip [1 :: Int ..] models)
  putStr $ "Select a model [1 = " ++ def ++ "]: "
  hFlush stdout
  input <- getLine
  let trimmed = T.unpack $ T.strip $ T.pack input
  if null trimmed
    then return (ModelId def)
    else case readMaybe trimmed :: Maybe Int of
      Just idx
        | idx >= 1 && idx <= length models -> return (ModelId (models !! (idx - 1)))
        | otherwise -> do
            putStrLn $ "Invalid index. Using default: " ++ def
            return (ModelId def)
      Nothing -> do
        putStrLn $ "Invalid input. Using default: " ++ def
        return (ModelId def)

selectRemoteModel :: Config -> IO ModelId
selectRemoteModel config = do
  let fb = unModelId (fallbackModel config)
  putStrLn "Remote API backend (OpenAI-compatible)."
  putStr $ "Enter model name [" ++ fb ++ "]: "
  hFlush stdout
  input <- getLine
  let trimmed = T.unpack $ T.strip $ T.pack input
  return $ if null trimmed then fallbackModel config else ModelId trimmed

selectModel :: Config -> IO ModelId
selectModel config = case modelBackend config of
  LocalOllama _ -> do
    models <- fetchModels config
    selectOllamaModel config models
  RemoteOpenAI _ _ ->
    selectRemoteModel config

-- ---------------------------------------------------------------------------
-- CLI Help / memories display
-- ---------------------------------------------------------------------------

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
  putStrLn " /read <file> [q]  - Read a file and ask an optional question"
  putStrLn " /run <command>    - Run a shell command on the local system"
  putStrLn "=============================================================="

printMemories :: [Text] -> IO ()
printMemories mems = do
  putStrLn "=============================================================="
  putStrLn " Stored Memories:"
  putStrLn "=============================================================="
  if null mems
    then putStrLn " No memories stored yet. Use /remember <fact> to add some!"
    else mapM_ (\(i, m) -> putStrLn $ " [" ++ show i ++ "] " ++ T.unpack m) (zip [1 :: Int ..] mems)
  putStrLn "=============================================================="

-- ---------------------------------------------------------------------------
-- CLI Shell Command Result Presentation
-- ---------------------------------------------------------------------------

presentCommandResult :: CommandResult -> AppM ()
presentCommandResult res = case res of
  ResExit -> liftIO $ putStrLn "Goodbye!"
  ResHelp -> liftIO printHelp
  ResMemories mems -> liftIO $ printMemories mems
  ResRemember fact -> liftIO $ putStrLn $ "Memory saved: " ++ T.unpack fact
  ResForget idx _ -> liftIO $ putStrLn $ "Memory [" ++ show idx ++ "] forgotten."
  ResSessionList sessions activeId -> liftIO $ do
    putStrLn "=============================================================="
    putStrLn " Sessions list:"
    putStrLn "=============================================================="
    mapM_ (\(i, s) -> do
      let prefix = if sessionId s == activeId then "* " else "  "
      putStrLn $ prefix ++ "[" ++ show i ++ "] "
                         ++ unSessionName (sessionName s)
                         ++ " (" ++ unSessionId (sessionId s) ++ ") - "
                         ++ show (length (messages s)) ++ " messages"
      ) (zip [1 :: Int ..] sessions)
    putStrLn "=============================================================="
  ResSessionNew _ _ -> showSessionHistory
  ResSessionLoad _ _ -> showSessionHistory
  ResSessionRename (SessionName newName) -> do
    env <- ask
    state <- liftIO $ readIORef (envState env)
    liftIO $ redrawLayout newName (unModelId (selectedModel state)) (length (longTermMemories state))
  ResSessionDelete _ _ -> showSessionHistory
  ResSessionFork _ _ -> showSessionHistory
  ResRead path -> liftIO $ putStrLn $ "File loaded successfully: " ++ path
  ResRun output -> liftIO $ putStrLn $ "Command execution result:\n" ++ output
  ResClear -> showSessionHistory

-- ---------------------------------------------------------------------------
-- CLI Shell loop
-- ---------------------------------------------------------------------------

loop :: AppM ()
loop = do
  env <- ask
  let config = envConfig env
  state <- liftIO $ readIORef (envState env)
  rows  <- liftIO getRowsOrDefault
  let activeS    = currentSessionName state
      modelStr   = unModelId (selectedModel state)
      memsCount  = length (longTermMemories state)
      promptStr  = "haskai-cli [" ++ activeS ++ "]> "
      promptCol  = length promptStr

  minput <- lift $ getInputLine promptStr
  case minput of
    Nothing -> liftIO $ putStrLn "\nGoodbye!"
    Just input -> do
      let trimmed = T.unpack $ T.strip $ T.pack input
      if null trimmed
        then loop
        else if "/" `isPrefixOf` trimmed
          then do
            -- Reset terminal scrolls before executing command
            liftIO $ do
              redrawLayout activeS modelStr memsCount
              setCursorPosition (rows - 1) 0
              putStr "\ESC[2K"
              putStr promptStr
              putStr $ "\ESC[4;" ++ show (rows - 2) ++ "r"
              setCursorPosition (rows - 3) 0
              hFlush stdout
            case parseCommand (T.pack trimmed) of
              Left errMsg -> do
                liftIO $ do
                  putStrLn errMsg
                  putStr "\ESC[r"
                  setCursorPosition (rows - 1) 0
                  putStr "\ESC[2K"
                  hFlush stdout
                loop
              Right cmd -> do
                -- Execute in AgentM
                execRes <- liftIO $ runReaderT (executeCommand cmd) env
                liftIO $ do
                  putStr "\ESC[r"
                  setCursorPosition (rows - 1) 0
                  putStr "\ESC[2K"
                  hFlush stdout
                case execRes of
                  Left err -> liftIO (putStrLn $ "Error: " ++ err) >> loop
                  Right ResExit -> liftIO $ putStrLn "Goodbye!"
                  Right (ResRead path) -> do
                    -- In CLI, if there's a question suffix or even if not, run executeAgentChat
                    case cmd of
                      CmdRead _ mQuest -> do
                        let qText = fromMaybe "請分析此檔案內容。" mQuest
                        runChat qText rows promptCol
                        loop
                      _ -> loop
                  Right cmdRes -> presentCommandResult cmdRes >> loop
          else do
            runChat (T.pack trimmed) rows promptCol
            loop

runChat :: T.Text -> Int -> Int -> AppM ()
runChat userText rows promptCol = do
  env <- ask
  let config = envConfig env
  state <- liftIO $ readIORef (envState env)
  let activeS    = currentSessionName state
      modelStr   = unModelId (selectedModel state)
      memsCount  = length (longTermMemories state)
      promptStr  = "haskai-cli [" ++ activeS ++ "]> "

  liftIO $ do
    redrawLayout activeS modelStr memsCount
    setCursorPosition (rows - 1) 0
    putStr "\ESC[2K"
    putStr promptStr
    putStr $ "\ESC[4;" ++ show (rows - 2) ++ "r"
    setCursorPosition (rows - 3) 0
    hFlush stdout

  -- Step callback prints intermediate agent steps
  let stepCallback msg = do
        let r = role msg
            c = content msg
        if r == "user"
          then do
            setSGR [SetColor Foreground Vivid Green]
            putStr "User: "
            setSGR [Reset]
            putStrLn (T.unpack c)
            hFlush stdout
          else if r == "assistant"
            then case tool_calls msg of
              Just tcs | not (null tcs) -> do
                setSGR [SetColor Foreground Vivid Yellow]
                putStrLn "🤖 LLM 請求執行工具："
                mapM_ (\tc -> putStrLn $ "  - [工具: " ++ T.unpack (fcName (tcFunction tc))
                                      ++ ", 參數: " ++ T.unpack (fcArguments (tcFunction tc)) ++ "]") tcs
                setSGR [Reset]
                hFlush stdout
              _ -> do
                setSGR [SetColor Foreground Vivid Cyan]
                putStr "Assistant: "
                setSGR [Reset]
                putStrLn (T.unpack c)
                hFlush stdout
            else if r == "tool"
              then do
                setSGR [SetColor Foreground Vivid Magenta]
                putStrLn $ "🔧 工具執行結果: " ++ T.unpack c
                setSGR [Reset]
                hFlush stdout
              else return ()

  -- Run chat inside AgentM under a background spinner
  chatRes <- liftIO $ do
    spinnerTid <- startSpinner rows promptCol (spinnerDelayMicros config)
    res <- try (runReaderT (executeAgentChat userText stepCallback) env) `finally` killThread spinnerTid
    stopSpinner rows promptCol spinnerTid
    putChar '\n'
    putStr "\ESC[r"
    setCursorPosition (rows - 1) 0
    putStr "\ESC[2K"
    hFlush stdout
    return res

  case chatRes of
    Left (err :: SomeException) -> liftIO $ do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "System Error: " ++ show err
      setSGR [Reset]
      hFlush stdout
    Right (Left errMsg) -> liftIO $ do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "LLM Error: " ++ errMsg
      setSGR [Reset]
      hFlush stdout
    Right (Right _) -> return ()

-- ---------------------------------------------------------------------------
-- Autocomplete settings
-- ---------------------------------------------------------------------------

haskaiSettings :: Settings IO
haskaiSettings = (defaultSettings :: Settings IO)
  { complete = completeWord Nothing " \t" $ \s ->
      return $ map (\c -> Completion c c True) (filter (s `isPrefixOf`) allCmds)
  }
  where
    allCmds =
      [ "/help", "/memories", "/remember", "/forget", "/exit", "/quit"
      , "/session list", "/session new", "/session load"
      , "/session rename", "/session delete", "/session fork"
      , "/read", "/run", "/clear"
      ]

-- ---------------------------------------------------------------------------
-- Main Entry Point
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  cwd <- getCurrentDirectory
  let devConfigPath = cwd </> "dev.config"
      memoryPath    = cwd </> ".config" </> "memory.json"
      sessionsPath  = cwd </> ".config" </> "sessions.json"

  ensureConfigDirExists memoryPath
  ensureConfigDirExists sessionsPath

  config <- loadDevConfig devConfigPath memoryPath sessionsPath
  mems      <- loadMemories memoryPath
  loadedSs  <- loadSessions sessionsPath

  (initialSessions, currentId) <- if null loadedSs
    then do
      let defSession = Session (SessionId "default") (SessionName "Default Session") []
      saveSessions sessionsPath [defSession]
      return ([defSession], SessionId "default")
    else case loadedSs of
      (firstS:_) -> return (loadedSs, sessionId firstS)
      []         -> return ([], SessionId "default")

  let activeHistory = case filter (\s -> sessionId s == currentId) initialSessions of
        (s:_) -> messages s
        []    -> []

  putStrLn "Checking available models..."
  selected <- selectModel config

  let activeSName = case filter (\s -> sessionId s == currentId) initialSessions of
                      (s:_) -> unSessionName (sessionName s)
                      []    -> "Default Session"
      modelStr    = unModelId selected

  let initialState = AppState selected activeHistory mems currentId initialSessions Nothing

  stateRef <- newIORef initialState
  let env = Env config stateRef

  runInputT haskaiSettings $
    runReaderT (liftIO (initTerminalScreen activeSName modelStr (length mems)) >> loop) env

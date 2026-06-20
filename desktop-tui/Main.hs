{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import Control.Concurrent (forkIO)
import Control.Monad (void, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.IORef (readIORef, modifyIORef', newIORef)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Data.Maybe (fromMaybe)

-- Core library imports
import Types
import Memory
import Ollama (fetchModels)
import AppConfig
import Lib (parseCommand)
import Controller

-- ---------------------------------------------------------------------------
-- Custom Event & Resource Name Types
-- ---------------------------------------------------------------------------

data CustomEvent
  = AgentStepUpdate Message
  | TuiStateUpdate AppState
  deriving (Show, Eq)

data ResourceName
  = ChatViewport
  | InputField
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- State definition
-- ---------------------------------------------------------------------------

data TuiState = TuiState
  { _tuiInput         :: Text
  , _tuiHistory       :: [Message]
  , _tuiSessions      :: [Session]
  , _tuiCurrentSess   :: SessionId
  , _tuiSelectedModel  :: ModelId
  , _tuiModels        :: [String]
  , _tuiMemories      :: [Text]
  , _tuiStatus        :: Text
  , _tuiIsBusy        :: Bool
  , _tuiAcCandidates  :: [String]
  , _tuiAcIndex       :: Int  -- -1 means no selection
  , _tuiConfig        :: Config
  , _tuiEventChan     :: BChan CustomEvent
  , _tuiLoadedFile    :: Maybe (FilePath, Text)
  , _tuiEnv           :: Env
  }

makeLenses ''TuiState

-- ---------------------------------------------------------------------------
-- Autocomplete Helpers
-- ---------------------------------------------------------------------------

data CommandDef = CommandDef
  { cmdPrefix   :: String
  , cmdHelpDesc :: String
  , cmdNeedsArg :: Bool
  }

commandDefs :: [CommandDef]
commandDefs =
  [ CommandDef "/help"             "顯示本幫助訊息"                  False
  , CommandDef "/memories"         "列出長期記憶"                    False
  , CommandDef "/remember"         "記住一個事實"                    True
  , CommandDef "/forget"           "忘記指定索引的記憶"               True
  , CommandDef "/session list"     "列出所有會話"                    False
  , CommandDef "/session new"      "建立新會話"                      True
  , CommandDef "/session load"     "載入會話"                        True
  , CommandDef "/session rename"   "重命名會話"                      True
  , CommandDef "/session delete"   "刪除會話"                        True
  , CommandDef "/session fork"     "複製會話"                        True
  , CommandDef "/read"             "讀取檔案分析"                    True
  , CommandDef "/run"              "在背景執行系統指令"               True
  , CommandDef "/clear"            "清除當前對話歷史顯示"              False
  , CommandDef "/exit"             "退出程式"                        False
  , CommandDef "/quit"             "退出程式"                        False
  ]

padRightStr :: Int -> String -> String
padRightStr n s = s ++ replicate (max 0 (n - length s)) ' '

allCommands :: [String]
allCommands = map cmdPrefix commandDefs

appendSpaceIfNeedsArg :: String -> String
appendSpaceIfNeedsArg cmd =
  let found = filter (\d -> cmdPrefix d == cmd) commandDefs
  in case found of
       (d:_) -> if cmdNeedsArg d then cmd ++ " " else cmd
       []    -> cmd

updateAutocomplete :: TuiState -> TuiState
updateAutocomplete s =
  let inp = s ^. tuiInput
  in if "/" `T.isPrefixOf` inp
       then
         let cand = filter (T.unpack inp `isPrefixOf`) allCommands
         in s & tuiAcCandidates .~ cand
              & tuiAcIndex .~ (if null cand then -1 else 0)
       else s & tuiAcCandidates .~ []
              & tuiAcIndex .~ -1

-- ---------------------------------------------------------------------------
-- Attributes & Styling
-- ---------------------------------------------------------------------------

selectedAttr :: AttrName
selectedAttr = attrName "selected"

activeSessionAttr :: AttrName
activeSessionAttr = attrName "activeSession"

userMsgAttr :: AttrName
userMsgAttr = attrName "userMsg"

aiMsgAttr :: AttrName
aiMsgAttr = attrName "aiMsg"

systemMsgAttr :: AttrName
systemMsgAttr = attrName "systemMsg"

statusReadyAttr :: AttrName
statusReadyAttr = attrName "statusReady"

statusBusyAttr :: AttrName
statusBusyAttr = attrName "statusBusy"

statusErrorAttr :: AttrName
statusErrorAttr = attrName "statusError"

tuiAttrMap :: TuiState -> AttrMap
tuiAttrMap _ = attrMap V.defAttr
  [ (selectedAttr, V.white `on` V.blue)
  , (activeSessionAttr, fg V.green `V.withStyle` V.bold)
  , (userMsgAttr, fg V.green `V.withStyle` V.bold)
  , (aiMsgAttr, fg V.cyan `V.withStyle` V.bold)
  , (systemMsgAttr, fg V.yellow)
  , (statusReadyAttr, fg V.green)
  , (statusBusyAttr, fg V.yellow)
  , (statusErrorAttr, fg V.red)
  ]

-- ---------------------------------------------------------------------------
-- Async Chat Request
-- ---------------------------------------------------------------------------

sendChatRequest :: EventM ResourceName TuiState ()
sendChatRequest = do
  s <- get
  let userText = s ^. tuiInput
      hasFile = case s ^. tuiLoadedFile of
                  Just _ -> True
                  Nothing -> False
  if T.null (T.strip userText) && not hasFile
    then return ()
    else do
      let env = s ^. tuiEnv
          chan = s ^. tuiEventChan
      tuiInput .= ""
      tuiIsBusy .= True
      tuiStatus .= "AI 思考中..."
      
      liftIO $ void $ forkIO $ do
        -- executeAgentChat merges loadedFile internally and performs tool loop
        _ <- runReaderT (executeAgentChat userText (\msg -> writeBChan chan (AgentStepUpdate msg))) env
        latestState <- readIORef (envState env)
        writeBChan chan (TuiStateUpdate latestState)

-- ---------------------------------------------------------------------------
-- Async Command Handler for TUI State
-- ---------------------------------------------------------------------------

formatCommandResult :: CommandResult -> [Message]
formatCommandResult res = case res of
  ResHelp ->
    let helpLines =
          "=== HaskAI TUI 幫助選單 ===" :
          map (\d -> "  " ++ padRightStr 20 (cmdPrefix d) ++ " - " ++ cmdHelpDesc d)
              (filter (\d -> cmdPrefix d /= "/quit") commandDefs)
    in map (\l -> Message "system" (T.pack l) Nothing Nothing) helpLines
    
  ResMemories mems ->
    let lines' = if null mems
                   then ["無儲存的記憶。輸入 /remember <fact> 來新增。"]
                   else "儲存的記憶:" : map (\(i, m) -> "  [" ++ show i ++ "] " ++ T.unpack m) (zip [1..] mems)
    in map (\l -> Message "system" (T.pack l) Nothing Nothing) lines'
    
  ResSessionList ss activeId ->
    let lines' = "會話列表:" : map (\(i, sess) ->
          let prefix = if sessionId sess == activeId then "  * " else "    "
          in prefix ++ "[" ++ show i ++ "] " ++ unSessionName (sessionName sess) ++ " (" ++ show (length (messages sess)) ++ " msgs)") (zip [1..] ss)
    in map (\l -> Message "system" (T.pack l) Nothing Nothing) lines'
    
  ResClear -> []
  _ -> []

runTuiCommand :: Text -> TuiState -> IO ()
runTuiCommand rawInput s = do
  let env = s ^. tuiEnv
      chan = s ^. tuiEventChan
  case parseCommand rawInput of
    Left err -> do
      modifyIORef' (envState env) $ \st ->
        let msg = Message "system" (T.pack ("語法錯誤: " ++ err)) Nothing Nothing
            st' = st { sessionHistory = sessionHistory st ++ [msg] }
        in syncActiveSession st'
      latestState <- readIORef (envState env)
      writeBChan chan (TuiStateUpdate latestState)
    Right cmd -> void $ forkIO $ do
      res <- runReaderT (executeCommand cmd) env
      case res of
        Left err -> do
          modifyIORef' (envState env) $ \st ->
            let msg = Message "system" (T.pack ("執行錯誤: " ++ err)) Nothing Nothing
                st' = st { sessionHistory = sessionHistory st ++ [msg] }
            in syncActiveSession st'
        Right ResExit -> return () -- Handled by halt in enter key handler
        Right cmdRes -> do
          let extraMsgs = formatCommandResult cmdRes
          unless (null extraMsgs) $
            modifyIORef' (envState env) $ \st ->
              let st' = st { sessionHistory = sessionHistory st ++ extraMsgs }
              in syncActiveSession st'
      
      latestState <- readIORef (envState env)
      writeBChan chan (TuiStateUpdate latestState)

-- ---------------------------------------------------------------------------
-- Brick Event Handler (Brick 2.x style)
-- ---------------------------------------------------------------------------

handleEvent :: BrickEvent ResourceName CustomEvent -> EventM ResourceName TuiState ()
handleEvent (AppEvent (TuiStateUpdate newState)) = do
  tuiHistory .= sessionHistory newState
  tuiSessions .= sessions newState
  tuiCurrentSess .= currentSessionId newState
  tuiMemories .= longTermMemories newState
  tuiLoadedFile .= loadedFile newState
  tuiSelectedModel .= selectedModel newState
  tuiIsBusy .= False
  tuiStatus .= "等待輸入"
  vScrollToEnd (viewportScroll ChatViewport)

handleEvent (AppEvent (AgentStepUpdate msg)) = do
  s <- get
  let hist' = (s ^. tuiHistory) ++ [msg]
  tuiHistory .= hist'
  vScrollToEnd (viewportScroll ChatViewport)

handleEvent (VtyEvent (V.EvKey V.KEnter [V.MShift])) = do
  tuiInput %= (`T.snoc` '\n')
  modify updateAutocomplete

handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  s <- get
  let cands = s ^. tuiAcCandidates
      acIdx = s ^. tuiAcIndex
  if not (null cands) && (acIdx >= 0)
    then do
      let selected = cands !! acIdx
          newVal = T.pack (appendSpaceIfNeedsArg selected)
      tuiInput .= newVal
      tuiAcCandidates .= []
      tuiAcIndex .= -1
    else do
      let inputVal = T.strip (s ^. tuiInput)
      if T.null inputVal
        then return ()
        else if inputVal `elem` ["/exit", "/quit"]
               then halt
               else if "/" `T.isPrefixOf` inputVal
                      then do
                        case parseCommand inputVal of
                          Right (CmdRead _ mQuest) -> tuiInput .= fromMaybe "" mQuest
                          _ -> tuiInput .= ""
                        tuiIsBusy .= True
                        tuiStatus .= "執行指令中..."
                        liftIO $ runTuiCommand inputVal s
                        vScrollToEnd (viewportScroll ChatViewport)
                      else do
                        if s ^. tuiIsBusy
                          then return ()
                          else do
                            sendChatRequest
                            vScrollToEnd (viewportScroll ChatViewport)

handleEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  tuiAcCandidates .= []
  tuiAcIndex .= -1

handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  s <- get
  let cands = s ^. tuiAcCandidates
  if null cands
    then return ()
    else do
      let len = length cands
          nextIdx = (s ^. tuiAcIndex + 1) `mod` len
      tuiAcIndex .= nextIdx

handleEvent (VtyEvent (V.EvKey V.KBackTab [])) = do
  s <- get
  let cands = s ^. tuiAcCandidates
  if null cands
    then return ()
    else do
      let len = length cands
          prevIdx = (s ^. tuiAcIndex - 1 + len) `mod` len
      tuiAcIndex .= prevIdx

handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
  s <- get
  let cands = s ^. tuiAcCandidates
  if null cands
    then return ()
    else do
      let len = length cands
          nextIdx = (s ^. tuiAcIndex + 1) `mod` len
      tuiAcIndex .= nextIdx

handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
  s <- get
  let cands = s ^. tuiAcCandidates
  if null cands
    then return ()
    else do
      let len = length cands
          prevIdx = (s ^. tuiAcIndex - 1 + len) `mod` len
      tuiAcIndex .= prevIdx

handleEvent (VtyEvent (V.EvKey (V.KChar c) [])) = do
  tuiInput %= (`T.snoc` c)
  modify updateAutocomplete

handleEvent (VtyEvent (V.EvKey V.KBS [])) = do
  s <- get
  let inp = s ^. tuiInput
  if T.null inp
    then return ()
    else tuiInput .= T.init inp
  modify updateAutocomplete

handleEvent (VtyEvent (V.EvKey (V.KChar 'u') [V.MCtrl])) = do
  tuiInput .= ""
  tuiAcCandidates .= []
  tuiAcIndex .= -1

handleEvent _ = return ()

-- ---------------------------------------------------------------------------
-- Drawing UI
-- ---------------------------------------------------------------------------

chatList :: TuiState -> Widget ResourceName
chatList s =
  let msgs = s ^. tuiHistory
  in if null msgs
       then padTop (Pad 2) $ hCenter (txt "無歷史對話紀錄。請在下方輸入訊息開始對話，或輸入 /help 查看命令。")
       else vBox $ map renderMsg msgs
  where
    renderMsg msg =
      let r = role msg
          content' = content msg
          roleWidget = if r == "user"
                         then withAttr userMsgAttr (txt "User: ")
                         else if r == "assistant"
                                then withAttr aiMsgAttr (txt "Assistant: ")
                                else withAttr systemMsgAttr (txt "System: ")
          lines' = T.lines content'
          contentWidget = vBox $ map txtWrap lines'
      in hBox [roleWidget, contentWidget]

drawUI :: TuiState -> [Widget ResourceName]
drawUI s = [mainLayout]
  where
    mainLayout =
      hBox
         [ mainPanel
         , vBorder
         , hLimit 35 sidebar
         ]
    
    mainPanel =
      vBox
        [ borderWithLabel (txt " HaskAI TUI 對話區域 ") chatArea
        , autocompleteArea
        , stagedFileArea
        , borderWithLabel (txt " 輸入區域 (支援 / 指令) ") inputLine
        ]
    
    chatArea = viewport ChatViewport Vertical (chatList s)
    
    autocompleteArea =
      let cands = s ^. tuiAcCandidates
      in if null cands
           then emptyWidget
           else borderWithLabel (txt " 自動補全指令 (Tab/方向鍵選擇, Enter確認) ") $
                  vBox (map renderCandidate (zip [0..] cands))
      where
        renderCandidate (idx, cand) =
          let isSelected = idx == s ^. tuiAcIndex
              applyStyle = if isSelected
                             then withAttr selectedAttr
                             else id
          in applyStyle (txt (T.pack ("  " ++ appendSpaceIfNeedsArg cand)))

    stagedFileArea =
      case s ^. tuiLoadedFile of
        Nothing -> emptyWidget
        Just (path, fileContent) ->
          let charCount = T.length fileContent
              sizeStr = if charCount >= 1024
                          then show (charCount `div` 1024) ++ " KB"
                          else show charCount ++ " B"
              desc = "📎 已載入檔案: " ++ path ++ " (" ++ sizeStr ++ ") [送出訊息時會自動附加檔案內容]"
          in padLeft (Pad 2) $ padRight (Pad 2) $ withAttr systemMsgAttr (txt (T.pack desc))
    
    inputLine =
      let inp = s ^. tuiInput
          prompt = txt "haskai-tui> "
          cursor = if s ^. tuiIsBusy
                     then withAttr statusBusyAttr (txt "◌")
                     else txt "█"
          linesList = T.splitOn "\n" inp
          numLines = max 1 (length linesList)
          content = case linesList of
             [] -> hBox [ prompt, cursor, fill ' ' ]
             [singleLine] -> hBox [ prompt, txt singleLine, cursor, fill ' ' ]
             multipleLines ->
               let initLines = init multipleLines
                   lastLine = last multipleLines
                   renderedInit = vBox $ map txt initLines
                   renderedLast = hBox [ txt lastLine, cursor, fill ' ' ]
               in hBox
                    [ prompt
                    , vBox [ renderedInit, renderedLast ]
                    ]
      in vLimit numLines content
    
    sidebar =
      vBox
        [ borderWithLabel (txt " 系統狀態 ") statusBox
        , borderWithLabel (txt " 目前會話 ") sessionBox
        , borderWithLabel (txt " 可用模型 ") modelsBox
        , borderWithLabel (txt " 記憶個數 ") memoryBox
        ]

    statusBox =
      let isBusy = s ^. tuiIsBusy
          statusAttr = if isBusy
                         then statusBusyAttr
                         else if "錯誤" `T.isInfixOf` (s ^. tuiStatus)
                                then statusErrorAttr
                                else statusReadyAttr
      in vBox
           [ hBox [ txt "狀態: ", withAttr statusAttr (txt (s ^. tuiStatus)) ]
           , hBox [ txt "模型: ", txt (T.pack (unModelId (s ^. tuiSelectedModel))) ]
           ]

    sessionBox =
      vBox $ map renderSess (s ^. tuiSessions)
      where
        renderSess sess =
          let isActive = sessionId sess == s ^. tuiCurrentSess
              prefix = if isActive then "* " else "  "
              applyStyle = if isActive then withAttr activeSessionAttr else id
          in applyStyle (txt (T.pack (prefix ++ unSessionName (sessionName sess) ++ " (" ++ show (length (messages sess)) ++ " msgs)")))

    modelsBox =
      vBox $ map (\m -> txt (T.pack ("  " ++ m))) (s ^. tuiModels)

    memoryBox =
      txt (T.pack ("  " ++ show (length (s ^. tuiMemories)) ++ " 個記憶"))

-- ---------------------------------------------------------------------------
-- Brick App definition
-- ---------------------------------------------------------------------------

app :: App TuiState CustomEvent ResourceName
app = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return ()
  , appAttrMap      = tuiAttrMap
  }

-- ---------------------------------------------------------------------------
-- Main / Entry Point
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
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

  models <- fetchModels config
  let selected = case models of
                   (m:_) -> ModelId m
                   []    -> fallbackModel config

  eventChan <- newBChan 10
  stateRef <- newIORef (AppState selected activeHistory mems currentId initialSessions Nothing)
  let env = Env config stateRef

  let initialState = TuiState
        { _tuiInput         = ""
        , _tuiHistory       = activeHistory
        , _tuiSessions      = initialSessions
        , _tuiCurrentSess   = currentId
        , _tuiSelectedModel  = selected
        , _tuiModels        = models
        , _tuiMemories      = mems
        , _tuiStatus        = "等待輸入"
        , _tuiIsBusy        = False
        , _tuiAcCandidates  = []
        , _tuiAcIndex       = -1
        , _tuiConfig        = config
        , _tuiEventChan     = eventChan
        , _tuiLoadedFile    = Nothing
        , _tuiEnv           = env
        }

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just eventChan) app initialState

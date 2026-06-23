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
import Control.Monad (void, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.IORef (readIORef, modifyIORef', newIORef)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, listDirectory, doesDirectoryExist, canonicalizePath)
import System.FilePath ((</>), takeFileName)
import Data.Maybe (fromMaybe)

-- Core library imports
import Types
import Memory
import Ollama (fetchModels)
import AppConfig
import Lib (parseCommand)
import Controller
import Tool (readTextFileStrict)

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
  | AttachedFileBox Int
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- State definition
-- ---------------------------------------------------------------------------

data TuiState = TuiState
  { _tuiInput                     :: Text
  , _tuiHistory                   :: [Message]
  , _tuiSessions                  :: [Session]
  , _tuiCurrentSess               :: SessionId
  , _tuiSelectedModel             :: ModelId
  , _tuiModels                    :: [String]
  , _tuiMemories                  :: [Text]
  , _tuiStatus                    :: Text
  , _tuiIsBusy                    :: Bool
  , _tuiAcCandidates              :: [String]
  , _tuiAcIndex                   :: Int  -- -1 means no selection
  , _tuiConfig                    :: Config
  , _tuiEventChan                 :: BChan CustomEvent
  , _tuiLoadedFiles               :: [(FilePath, Text)]
  , _tuiEnv                       :: Env
  , _tuiFileBrowserActive         :: Bool
  , _tuiFileBrowserPath           :: FilePath
  , _tuiFileBrowserEntries        :: [String]
  , _tuiFileBrowserIndex          :: Int
  , _tuiCustomSystemPrompt        :: Maybe Text
  , _tuiClearIndex                :: Int
  , _tuiAttachedFileSelectedIndex :: Maybe Int
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
  , CommandDef "/context"          "查看目前的 context 組裝狀態"     False
  , CommandDef "/prompt"           "設定 System Prompt 內容"         True
  , CommandDef "/summary"          "手動總結壓縮對話歷史"            True
  , CommandDef "/unfile"           "從 context 中移除夾帶檔案"       False
  , CommandDef "/session list"     "列出所有會話"                    False
  , CommandDef "/session new"      "建立新會話"                      True
  , CommandDef "/session load"     "載入會話"                        True
  , CommandDef "/session rename"   "重命名會話"                      True
  , CommandDef "/session delete"   "刪除會話"                        True
  , CommandDef "/session fork"     "複製會話"                        True
  , CommandDef "/file"             "讀取檔案分析"                    True
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
      hasFile = not (null (s ^. tuiLoadedFiles))
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
    in map (\l -> Message "system" (T.pack l) Nothing Nothing Nothing) helpLines
    
  ResContext details ->
    [Message "system" (T.pack details) Nothing Nothing Nothing]
    
  ResSessionList ss activeId ->
    let lines' = "會話列表:" : map (\(i, sess) ->
          let prefix = if sessionId sess == activeId then "  * " else "    "
          in prefix ++ "[" ++ show i ++ "] " ++ unSessionName (sessionName sess) ++ " (" ++ show (length (messages sess)) ++ " msgs)") (zip [1 :: Int ..] ss)
    in map (\l -> Message "system" (T.pack l) Nothing Nothing Nothing) lines'
    
  ResPrompt mbText ->
    let desc = case mbText of
                 Nothing -> "已還原系統預設 Prompt。"
                 Just t  -> "System Prompt 已設定為:\n" ++ T.unpack t
    in [Message "system" (T.pack desc) Nothing Nothing Nothing]
    
  ResSummary summaryText ->
    [ Message "system" "已成功手動壓縮總結歷史記憶。" Nothing Nothing Nothing
    , Message "system" ("總結內容:\n" <> summaryText) Nothing Nothing Nothing
    ]
    
  ResClear -> []
  ResUnfileSuccess _ -> []
  ResUnfileList files ->
    if null files
      then [Message "system" "目前沒有夾帶任何檔案。" Nothing Nothing Nothing]
      else
        let header = "目前夾帶的檔案:"
            lines' = map (\(i, path) -> "  [" ++ show i ++ "] " ++ path) files
            footer = "請使用 `/unfile <編號或檔案名稱>` 來移除指定檔案。"
        in map (\l -> Message "system" (T.pack l) Nothing Nothing Nothing) (header : lines' ++ [footer])
  _ -> []

runTuiCommand :: Text -> TuiState -> IO ()
runTuiCommand rawInput s = do
  let env = s ^. tuiEnv
      chan = s ^. tuiEventChan
  case parseCommand rawInput of
    Left err -> do
      modifyIORef' (envState env) $ \st ->
        let msg = Message "system" (T.pack ("語法錯誤: " ++ displayError err)) Nothing Nothing Nothing
            st' = st { sessionHistory = sessionHistory st ++ [msg] }
        in syncActiveSession st'
      latestState <- readIORef (envState env)
      writeBChan chan (TuiStateUpdate latestState)
    Right cmd -> void $ forkIO $ do
      res <- runReaderT (executeCommand cmd) env
      case res of
        Left err -> do
          modifyIORef' (envState env) $ \st ->
            let msg = Message "system" (T.pack ("執行錯誤: " ++ err)) Nothing Nothing Nothing
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
-- File Browser & Selection Helpers
-- ---------------------------------------------------------------------------

listDirEntries :: FilePath -> IO [String]
listDirEntries path = do
  rawList <- listDirectory path
  let sortedRaw = sort rawList
  dirsAndFiles <- mapM (\e -> do
    isDir <- doesDirectoryExist (path </> e)
    return (isDir, e)) sortedRaw
  let dirs = [e ++ "/" | (True, e) <- dirsAndFiles, e /= ".git"]
      files = [e | (False, e) <- dirsAndFiles]
  return $ ["../"] ++ dirs ++ files

checkFileBrowserTrigger :: EventM ResourceName TuiState ()
checkFileBrowserTrigger = do
  s <- get
  let inp = T.strip (s ^. tuiInput)
  unless (s ^. tuiFileBrowserActive) $ do
    if inp == "/file"
      then do
        curr <- liftIO getCurrentDirectory
        entries <- liftIO $ listDirEntries curr
        tuiFileBrowserActive .= True
        tuiFileBrowserPath .= curr
        tuiFileBrowserEntries .= entries
        tuiFileBrowserIndex .= if null entries then -1 else 0
        tuiAcCandidates .= []
        tuiAcIndex .= -1
      else return ()

deleteAttachedFileAtIndex :: Int -> EventM ResourceName TuiState ()
deleteAttachedFileAtIndex idx = do
  s <- get
  let env = s ^. tuiEnv
      files = s ^. tuiLoadedFiles
  if idx >= 0 && idx < length files
    then do
      let (removedPath, _) = files !! idx
          remaining = take idx files ++ drop (idx + 1) files
      tuiLoadedFiles .= remaining
      tuiAttachedFileSelectedIndex .= Nothing
      liftIO $ modifyIORef' (envState env) $ \st -> st { loadedFiles = remaining }
      tuiStatus .= T.pack ("已移除夾帶檔案: " ++ removedPath)
    else return ()

-- ---------------------------------------------------------------------------
-- Brick Event Handler (Brick 2.x style)
-- ---------------------------------------------------------------------------

handleEvent :: BrickEvent ResourceName CustomEvent -> EventM ResourceName TuiState ()
handleEvent (AppEvent (TuiStateUpdate newState)) = do
  tuiHistory .= sessionHistory newState
  tuiClearIndex .= historyClearIndex newState
  tuiCustomSystemPrompt .= customSystemPrompt newState
  tuiSessions .= sessions newState
  tuiCurrentSess .= currentSessionId newState
  tuiLoadedFiles .= loadedFiles newState
  tuiSelectedModel .= selectedModel newState
  tuiIsBusy .= False
  tuiStatus .= "等待輸入"
  tuiAttachedFileSelectedIndex .= Nothing
  vScrollToEnd (viewportScroll ChatViewport)

handleEvent (AppEvent (AgentStepUpdate msg)) = do
  s <- get
  let hist' = (s ^. tuiHistory) ++ [msg]
  tuiHistory .= hist'
  vScrollToEnd (viewportScroll ChatViewport)

handleEvent (VtyEvent (V.EvMouseDown _ _ V.BScrollUp _)) = do
  vScrollBy (viewportScroll ChatViewport) (-3)

handleEvent (VtyEvent (V.EvMouseDown _ _ V.BScrollDown _)) = do
  vScrollBy (viewportScroll ChatViewport) 3

handleEvent (MouseDown _ V.BScrollUp _ _) = do
  vScrollBy (viewportScroll ChatViewport) (-3)

handleEvent (MouseDown _ V.BScrollDown _ _) = do
  vScrollBy (viewportScroll ChatViewport) 3

handleEvent (MouseDown name V.BLeft _ _) = do
  case name of
    AttachedFileBox idx -> tuiAttachedFileSelectedIndex .= Just idx
    _                   -> tuiAttachedFileSelectedIndex .= Nothing

handleEvent (VtyEvent (V.EvKey V.KEnter [V.MShift])) = do
  tuiInput %= (`T.snoc` '\n')
  modify updateAutocomplete

handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  s <- get
  if s ^. tuiFileBrowserActive
    then do
      let idx = s ^. tuiFileBrowserIndex
          entries = s ^. tuiFileBrowserEntries
      if idx >= 0 && idx < length entries
        then do
          let selected = entries !! idx
              currentPath = s ^. tuiFileBrowserPath
          if selected == "../"
            then do
              parent <- liftIO $ canonicalizePath (currentPath </> "..")
              newEntries <- liftIO $ listDirEntries parent
              tuiFileBrowserPath .= parent
              tuiFileBrowserEntries .= newEntries
              tuiFileBrowserIndex .= if null newEntries then -1 else 0
            else if "/" `isSuffixOf` selected
              then do
                let childDir = currentPath </> init selected
                canonicalChild <- liftIO $ canonicalizePath childDir
                newEntries <- liftIO $ listDirEntries canonicalChild
                tuiFileBrowserPath .= canonicalChild
                tuiFileBrowserEntries .= newEntries
                tuiFileBrowserIndex .= if null newEntries then -1 else 0
              else do
                let filePath = currentPath </> selected
                readRes <- liftIO $ readTextFileStrict filePath
                case readRes of
                  Left err -> tuiStatus .= T.pack ("讀取檔案失敗: " ++ err)
                  Right body -> do
                    let currentFiles = s ^. tuiLoadedFiles
                        newFiles = if filePath `elem` map fst currentFiles
                                     then currentFiles
                                     else currentFiles ++ [(filePath, body)]
                    tuiLoadedFiles .= newFiles
                    tuiAttachedFileSelectedIndex .= Nothing
                    tuiStatus .= T.pack ("已成功夾帶檔案: " ++ selected)
                    let env = s ^. tuiEnv
                    liftIO $ modifyIORef' (envState env) $ \st ->
                      let bgFiles = loadedFiles st
                          bgNewFiles = if filePath `elem` map fst bgFiles
                                         then bgFiles
                                         else bgFiles ++ [(filePath, body)]
                      in st { loadedFiles = bgNewFiles }
                tuiFileBrowserActive .= False
                tuiFileBrowserEntries .= []
                tuiFileBrowserIndex .= -1
                tuiInput .= ""
        else return ()
    else do
      let cands = s ^. tuiAcCandidates
          acIdx = s ^. tuiAcIndex
      if not (null cands) && (acIdx >= 0)
        then do
          let selected = cands !! acIdx
              newVal = T.pack (appendSpaceIfNeedsArg selected)
          tuiInput .= newVal
          tuiAcCandidates .= []
          tuiAcIndex .= -1
          checkFileBrowserTrigger
        else do
          let inputVal = T.strip (s ^. tuiInput)
          if T.null inputVal
            then return ()
            else if inputVal `elem` ["/exit", "/quit"]
                   then halt
                   else if "/" `T.isPrefixOf` inputVal
                          then do
                            case parseCommand inputVal of
                              Right (CmdFile _ mQuest) -> tuiInput .= fromMaybe "" mQuest
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
  s <- get
  if s ^. tuiFileBrowserActive
    then do
      tuiFileBrowserActive .= False
      tuiFileBrowserEntries .= []
      tuiFileBrowserIndex .= -1
    else do
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
  if s ^. tuiFileBrowserActive
    then do
      let len = length (s ^. tuiFileBrowserEntries)
      if len > 0
        then tuiFileBrowserIndex %= (\i -> (i + 1) `mod` len)
        else return ()
    else do
      let cands = s ^. tuiAcCandidates
      if null cands
        then return ()
        else do
          let len = length cands
              nextIdx = (s ^. tuiAcIndex + 1) `mod` len
          tuiAcIndex .= nextIdx

handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
  s <- get
  if s ^. tuiFileBrowserActive
    then do
      let len = length (s ^. tuiFileBrowserEntries)
      if len > 0
        then tuiFileBrowserIndex %= (\i -> (i - 1 + len) `mod` len)
        else return ()
    else do
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
  checkFileBrowserTrigger

handleEvent (VtyEvent (V.EvKey V.KBS [])) = do
  s <- get
  case s ^. tuiAttachedFileSelectedIndex of
    Just idx -> deleteAttachedFileAtIndex idx
    Nothing -> do
      let inp = s ^. tuiInput
      if T.null inp
        then return ()
        else tuiInput .= T.init inp
      modify updateAutocomplete
      checkFileBrowserTrigger

handleEvent (VtyEvent (V.EvKey V.KDel [])) = do
  s <- get
  case s ^. tuiAttachedFileSelectedIndex of
    Just idx -> deleteAttachedFileAtIndex idx
    Nothing  -> return ()

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
  let msgs = drop (s ^. tuiClearIndex) (s ^. tuiHistory)
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
          attr = if r == "user"
                   then userMsgAttr
                   else if r == "assistant"
                          then aiMsgAttr
                          else systemMsgAttr
          contentWidget = withAttr attr (vBox $ map txtWrap lines')
          attachmentWidget = case attached_files msg of
            Just files | not (null files) ->
              let formatFile (path, fileContent) =
                    let charCount = T.length fileContent
                        sizeStr = if charCount >= 1024
                                    then show (charCount `div` 1024) ++ " KB"
                                    else show charCount ++ " B"
                    in "📎 [已夾帶檔案: " ++ takeFileName path ++ " (" ++ sizeStr ++ ")]"
                  desc = "  " ++ intercalate ", " (map formatFile files)
              in withAttr systemMsgAttr (txt (T.pack desc))
            _ -> emptyWidget
          msgWidget = if r == "user"
                        then vBox [ contentWidget, attachmentWidget ]
                        else contentWidget
      in hBox [roleWidget, msgWidget]

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
        , fileBrowserArea
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

    fileBrowserArea =
      let active = s ^. tuiFileBrowserActive
          path = s ^. tuiFileBrowserPath
          entries = s ^. tuiFileBrowserEntries
          idx = s ^. tuiFileBrowserIndex
      in if not active || null entries
           then emptyWidget
           else
             let len = length entries
                 maxVisible = 10
                 startIndex = max 0 (min (idx - maxVisible `div` 2) (len - maxVisible))
                 visibleEntries = take maxVisible (drop startIndex (zip [0..] entries))
                 renderEntry (i, entry) =
                   let isSelected = i == idx
                       applyStyle = if isSelected
                                      then withAttr selectedAttr
                                      else id
                   in applyStyle (txt (T.pack ("  " ++ entry)))
             in borderWithLabel (txt (T.pack (" 檔案瀏覽器 (方向鍵選擇, Enter進入/確認, Esc關閉) - 目前路徑: " ++ path))) $
                  vBox (map renderEntry visibleEntries)
    
    inputLine =
      let inp = s ^. tuiInput
          prompt = txt "haskai-tui> "
          cursor = if s ^. tuiIsBusy
                     then withAttr statusBusyAttr (txt "◌")
                     else txt "█"
          linesList = T.splitOn "\n" inp
          numLines = max 1 (length linesList)
          inputContent = case linesList of
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
      in vLimit numLines inputContent
    
    sidebar =
      vLimit 40 $ vBox
        [ borderWithLabel (txt " 系統狀態 ") statusBox
        , borderWithLabel (txt " 目前會話 ") sessionBox
        , borderWithLabel (txt " 可用模型 ") modelsBox
        , borderWithLabel (txt " Context 佔有率 ") contextMonitorBox
        , attachedFileBoxBordered
        ]

    attachedFileBoxBordered = borderWithLabel (txt " 本次夾帶檔案 ") attachedFileBox

    attachedFileBox =
      let files = s ^. tuiLoadedFiles
      in if null files
           then txt "  (無)"
           else vBox (map renderFile (zip [0..] files))
      where
        renderFile (idx, (path, fileContent)) =
          let charCount = T.length fileContent
              sizeStr = if charCount >= 1024
                          then show (charCount `div` 1024) ++ " KB"
                          else show charCount ++ " B"
              fileLabel = takeFileName path ++ " (" ++ sizeStr ++ ")"
              widget = txt (T.pack (" 📎 " ++ fileLabel))
              isSelected = s ^. tuiAttachedFileSelectedIndex == Just idx
              applyStyle = if isSelected
                             then withAttr selectedAttr
                             else id
          in clickable (AttachedFileBox idx) (applyStyle widget)

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
      vBox $ map renderSess (zip [1 :: Int ..] (s ^. tuiSessions))
      where
        renderSess (i, sess) =
          let isActive = sessionId sess == s ^. tuiCurrentSess
              prefix = if isActive then "* " else "  "
              applyStyle = if isActive then withAttr activeSessionAttr else id
          in applyStyle (txt (T.pack (prefix ++ "[" ++ show i ++ "] " ++ unSessionName (sessionName sess) ++ " (" ++ show (length (messages sess)) ++ " msgs)")))

    modelsBox =
      vBox $ map (\m -> txt (T.pack ("  " ++ m))) (s ^. tuiModels)

    contextMonitorBox =
      let hist = s ^. tuiHistory
          (chatHistory, _, _) = partitionHistForContext hist
          projPrompt = ""
          files = s ^. tuiLoadedFiles
          mbFiles = if null files then Nothing else Just files
          ContextSizes sys histSize fileSize userSize = getContextSizes (s ^. tuiCustomSystemPrompt) projPrompt mbFiles chatHistory (T.unpack (s ^. tuiInput))
          sysT = sys `div` 4
          histT = histSize `div` 4
          fileT = fileSize `div` 4
          userT = userSize `div` 4
          totalT = sysT + histT + fileT + userT
          limit = 8000
          
          barWidth = 30
          sysW  = (sysT * barWidth) `div` limit
          histW = (histT * barWidth) `div` limit
          fileW = (fileT * barWidth) `div` limit
          userW = (userT * barWidth) `div` limit
          freeW = max 0 (barWidth - (sysW + histW + fileW + userW))
          
          barStr = replicate sysW 'S'
                ++ replicate histW 'H'
                ++ replicate fileW 'T'
                ++ replicate userW 'U'
                ++ replicate freeW '-'
          
          legend = "S:系統 H:歷史 T:夾檔 U:當前"
          numsStr = "總計: " ++ show totalT ++ " / " ++ show limit ++ " Tokens"
      in vBox
           [ hCenter (txt (T.pack ("[" ++ barStr ++ "]")))
           , padLeft (Pad 1) (txt (T.pack legend))
           , padLeft (Pad 1) (txt (T.pack numsStr))
           ]

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
  loadedSs  <- loadSessions sessionsPath

  (initialSessions, currentId) <- if null loadedSs
    then do
      let defSession = Session (SessionId "default") (SessionName "Default Session") [] Nothing (Just 0)
      saveSessions sessionsPath [defSession]
      return ([defSession], SessionId "default")
    else case loadedSs of
      (firstS:_) -> return (loadedSs, sessionId firstS)
      []         -> return ([], SessionId "default")

  let activeSession = filter (\s -> sessionId s == currentId) initialSessions
      activeHistory = case activeSession of
        (s:_) -> messages s
        []    -> []
      activeSysPrompt = case activeSession of
        (s:_) -> sessionSystemPrompt s
        []    -> Nothing
      activeClearIdx = case activeSession of
        (s:_) -> fromMaybe 0 (sessionClearIndex s)
        []    -> 0

  models <- fetchModels config
  let selected = case models of
                   (m:_) -> ModelId m
                   []    -> fallbackModel config

  eventChan <- newBChan 10
  stateRef <- newIORef (AppState selected activeHistory currentId initialSessions [] activeSysPrompt activeClearIdx)
  let env = Env config stateRef

  let initialState = TuiState
        { _tuiInput               = ""
        , _tuiHistory             = activeHistory
        , _tuiSessions            = initialSessions
        , _tuiCurrentSess         = currentId
        , _tuiSelectedModel       = selected
        , _tuiModels              = models
        , _tuiMemories            = []
        , _tuiStatus              = "等待輸入"
        , _tuiIsBusy              = False
        , _tuiAcCandidates        = []
        , _tuiAcIndex             = -1
        , _tuiConfig              = config
        , _tuiEventChan           = eventChan
        , _tuiLoadedFiles         = []
        , _tuiEnv                 = env
        , _tuiFileBrowserActive   = False
        , _tuiFileBrowserPath     = "."
        , _tuiFileBrowserEntries  = []
        , _tuiFileBrowserIndex    = -1
        , _tuiCustomSystemPrompt  = activeSysPrompt
        , _tuiClearIndex          = activeClearIdx
        , _tuiAttachedFileSelectedIndex = Nothing
        }

  let buildVty = do
        vty <- V.mkVty V.defaultConfig
        V.setMode (V.outputIface vty) V.Mouse True
        return vty
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just eventChan) app initialState

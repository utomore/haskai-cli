{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( parseCommand
  , parseSessionIndexOrName
  , currentSessionName
  , buildSystemMessage
  ) where

import Types
import Controller (buildSystemMessage)

import Data.List (isPrefixOf)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Char (isSpace)

-- ---------------------------------------------------------------------------
-- Command parser (Phase 3)
-- ---------------------------------------------------------------------------

-- | Helper to parse arguments for the /read command, supporting double-quoted paths.
parseReadCommandArgs :: String -> Either String (FilePath, Maybe T.Text)
parseReadCommandArgs s =
  let trimmed = dropWhile isSpace s
  in case trimmed of
    [] -> Left "Please specify a file path. Usage: /read <file> [question]"
    '"' : rest ->
      case break (== '"') rest of
        (path, '"' : q) ->
          let question = T.strip (T.pack q)
              mbQ = if T.null question then Nothing else Just question
          in Right (path, mbQ)
        _ -> Left "Mismatched double quotes in file path. Usage: /read \"<file path>\" [question]"
    _ ->
      case break isSpace trimmed of
        ("", _) -> Left "Please specify a file path. Usage: /read <file> [question]"
        (path, q) ->
          let question = T.strip (T.pack q)
              mbQ = if T.null question then Nothing else Just question
          in Right (path, mbQ)

-- | Convert raw user input (a slash command string) into a typed Command.
-- Returns Left with a human-readable error message on bad input.
parseCommand :: T.Text -> Either String Command
parseCommand raw =
  let str = T.unpack (T.strip raw)
  in case str of
    "/exit"           -> Right CmdExit
    "/quit"           -> Right CmdExit
    "/help"           -> Right CmdHelp
    "/memories"       -> Right CmdMemories
    "/remember"       -> Left "Please specify a fact. Usage: /remember <fact>"
    "/forget"         -> Left "Please specify the memory index. Usage: /forget <index>"
    "/session list"   -> Right CmdSessionList
    "/session new"    -> Right (CmdSessionNew Nothing)
    "/session load"   -> Left "Please specify a session. Usage: /session load <index_or_name>"
    "/session rename" -> Left "Please specify a new name. Usage: /session rename <new_name>"
    "/session delete" -> Left "Please specify a session. Usage: /session delete <index_or_name>"
    "/session fork"   -> Right (CmdSessionFork Nothing)
    "/read"           -> Left "Please specify a file path. Usage: /read <file> [question]"
    "/run"            -> Left "Please specify a shell command. Usage: /run <shell_command>"
    "/clear"          -> Right CmdClear
    _ | "/remember " `isPrefixOf` str ->
            let fact = T.strip (T.pack (drop 10 str))
            in if T.null fact
                 then Left "Memory cannot be empty. Usage: /remember <fact>"
                 else Right (CmdRemember fact)
      | "/forget " `isPrefixOf` str ->
            case readMaybe (drop 8 str) :: Maybe Int of
              Just idx -> Right (CmdForget idx)
              Nothing  -> Left "Invalid index. Usage: /forget <index>"
      | "/session new " `isPrefixOf` str ->
            let name = T.strip (T.pack (drop 12 str))
            in Right (CmdSessionNew (if T.null name then Nothing else Just name))
      | "/session load " `isPrefixOf` str ->
            let arg = T.unpack (T.strip (T.pack (drop 14 str)))
            in Right (CmdSessionLoad arg)
      | "/session rename " `isPrefixOf` str ->
            let arg = T.unpack (T.strip (T.pack (drop 16 str)))
            in if null arg
                 then Left "Name cannot be empty. Usage: /session rename <new_name>"
                 else Right (CmdSessionRename arg)
      | "/session delete " `isPrefixOf` str ->
            let arg = T.unpack (T.strip (T.pack (drop 16 str)))
            in Right (CmdSessionDelete arg)
      | "/session fork " `isPrefixOf` str ->
            let name = T.strip (T.pack (drop 14 str))
            in Right (CmdSessionFork (if T.null name then Nothing else Just name))
      | "/read " `isPrefixOf` str ->
            case parseReadCommandArgs (drop 6 str) of
              Right (path, mbQ) -> Right (CmdRead path mbQ)
              Left err          -> Left err
      | "/run " `isPrefixOf` str ->
            let cmd = T.unpack (T.strip (T.pack (drop 5 str)))
            in if null cmd
                 then Left "Command cannot be empty. Usage: /run <shell_command>"
                 else Right (CmdRun cmd)
      | otherwise ->
            Left ("Unknown command: " ++ str ++ ". Type /help for assistance.")

-- ---------------------------------------------------------------------------
-- Session helpers
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

currentSessionName :: AppState -> String
currentSessionName state =
  let activeId = currentSessionId state
  in case filter (\s -> sessionId s == activeId) (sessions state) of
       (s:_) -> unSessionName (sessionName s)
       []    -> "Unknown Session"

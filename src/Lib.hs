{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( parseCommand
  , parseSessionIndexOrName
  , currentSessionName
  ) where

import Types

import Data.List (isPrefixOf)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Char (isSpace)

-- ---------------------------------------------------------------------------
-- Command parser (Phase 3)
-- ---------------------------------------------------------------------------

-- | Helper to parse arguments for the /file command, supporting double-quoted paths.
parseFileCommandArgs :: String -> Either String (FilePath, Maybe T.Text)
parseFileCommandArgs s =
  let trimmed = dropWhile isSpace s
  in case trimmed of
    [] -> Left "Please specify a file path. Usage: /file <file> [question]"
    '"' : rest ->
      case break (== '"') rest of
        (path, '"' : q) ->
          let question = T.strip (T.pack q)
              mbQ = if T.null question then Nothing else Just question
          in Right (path, mbQ)
        _ -> Left "Mismatched double quotes in file path. Usage: /file \"<file path>\" [question]"
    _ ->
      case break isSpace trimmed of
        ("", _) -> Left "Please specify a file path. Usage: /file <file> [question]"
        (path, q) ->
          let question = T.strip (T.pack q)
              mbQ = if T.null question then Nothing else Just question
          in Right (path, mbQ)

-- | Convert raw user input (a slash command string) into a typed Command.
-- Returns 'Left AppError' on bad input; callers render it with 'displayError'.
parseCommand :: T.Text -> Either AppError Command
parseCommand raw =
  let str = T.unpack (T.strip raw)
  in case str of
    "/exit"           -> Right CmdExit
    "/quit"           -> Right CmdExit
    "/help"           -> Right CmdHelp
    "/context"        -> Right CmdContext
    "/session list"   -> Right CmdSessionList
    "/session new"    -> Right (CmdSessionNew Nothing)
    "/session load"   -> Left (UserInputError "Please specify a session. Usage: /session load <index_or_name>")
    "/session rename" -> Left (UserInputError "Please specify a new name. Usage: /session rename <new_name>")
    "/session delete" -> Left (UserInputError "Please specify a session. Usage: /session delete <index_or_name>")
    "/session fork"   -> Right (CmdSessionFork Nothing)
    "/file"           -> Left (UserInputError "Please specify a file path. Usage: /file <file> [question]")
    "/run"            -> Left (UserInputError "Please specify a shell command. Usage: /run <shell_command>")
    "/clear"          -> Right CmdClear
    "/prompt"         -> Right (CmdPrompt Nothing)
    "/summary"        -> Right (CmdSummary Nothing)
    "/unfile"         -> Right (CmdUnfile Nothing)
    _ | "/session new " `isPrefixOf` str ->
            let name = T.strip (T.pack (drop 12 str))
            in Right (CmdSessionNew (if T.null name then Nothing else Just name))
      | "/session load " `isPrefixOf` str ->
            let arg = T.unpack (T.strip (T.pack (drop 14 str)))
            in Right (CmdSessionLoad arg)
      | "/session rename " `isPrefixOf` str ->
            let arg = T.unpack (T.strip (T.pack (drop 16 str)))
            in if null arg
                 then Left (UserInputError "Name cannot be empty. Usage: /session rename <new_name>")
                 else Right (CmdSessionRename arg)
      | "/session delete " `isPrefixOf` str ->
            let arg = T.unpack (T.strip (T.pack (drop 16 str)))
            in Right (CmdSessionDelete arg)
      | "/session fork " `isPrefixOf` str ->
            let name = T.strip (T.pack (drop 14 str))
            in Right (CmdSessionFork (if T.null name then Nothing else Just name))
      | "/file " `isPrefixOf` str ->
            case parseFileCommandArgs (drop 6 str) of
              Right (path, mbQ) -> Right (CmdFile path mbQ)
              Left err          -> Left (UserInputError err)
      | "/run " `isPrefixOf` str ->
            let cmd = T.unpack (T.strip (T.pack (drop 5 str)))
            in if null cmd
                 then Left (UserInputError "Command cannot be empty. Usage: /run <shell_command>")
                 else Right (CmdRun cmd)
      | "/prompt " `isPrefixOf` str ->
            let p = T.unpack (T.strip (T.pack (drop 8 str)))
            in Right (CmdPrompt (if null p then Nothing else Just (T.pack p)))
      | "/summary " `isPrefixOf` str ->
            let p = T.unpack (T.strip (T.pack (drop 9 str)))
            in Right (CmdSummary (if null p then Nothing else Just (T.pack p)))
      | "/unfile " `isPrefixOf` str ->
            let p = T.unpack (T.strip (T.pack (drop 8 str)))
            in Right (CmdUnfile (if null p then Nothing else Just p))
      | otherwise ->
            Left (UnknownCommand str)

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

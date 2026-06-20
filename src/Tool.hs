{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tool
  ( readTextFileStrict
  , runShellCommand
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.ByteString as B
import qualified Control.Exception as E
import qualified System.Process as Process

-- | Read file strictly as ByteString, then decode with lenient decode to prevent crashes.
readTextFileStrict :: FilePath -> IO (Either String Text)
readTextFileStrict path = do
  res <- E.try (B.readFile path) :: IO (Either E.SomeException B.ByteString)
  case res of
    Left err -> return (Left (show err))
    Right bytes -> do
      let content = TE.decodeUtf8With TEE.lenientDecode bytes
      return (Right content)

-- | Run a shell command strictly and return the combined stdout/stderr.
-- Stdin is closed (passed empty string "") to prevent interactive hanging.
runShellCommand :: String -> IO (Either String String)
runShellCommand cmd = do
  -- NOTE: We pass an empty string "" to stdin and close it, so that interactive commands do not hang.
  -- This design can be adjusted in the future to support interactive terminals.
  res <- E.try (Process.readCreateProcessWithExitCode (Process.shell cmd) "")
  case res of
    Left (err :: E.SomeException) -> return (Left (show err))
    Right (_exitCode, stdoutStr, stderrStr) ->
      return (Right (stdoutStr ++ "\n" ++ stderrStr))

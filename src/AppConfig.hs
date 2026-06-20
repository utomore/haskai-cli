-- | Loads configuration from a dev.config KEY=VALUE file.
--
-- Lookup order:
--   1. dev.config in the current working directory
--   2. Built-in defaults (LocalOllama on localhost:11434)
--
-- Example dev.config:
--   OLLAMA_URL=http://localhost:11434
--   DEFAULT_MODEL=gemma4:12b
--   HTTP_TIMEOUT_SECONDS=120
--   SPINNER_DELAY_MS=80
--
--   # Uncomment to switch to the Gemini remote backend:
--   # GEMINI_API_KEY=your-key-here
--   # GEMINI_BASE_URL=https://generativelanguage.googleapis.com/v1beta/openai

module AppConfig
  ( loadDevConfig
  , writeExampleDevConfig
  ) where

import Types

import System.Directory (doesFileExist)
import Data.List (isPrefixOf)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Internal parsing
-- ---------------------------------------------------------------------------

type RawConfig = [(String, String)]

trimSpace :: String -> String
trimSpace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseDevConfig :: String -> RawConfig
parseDevConfig src = foldr parseLine [] (lines src)
  where
    parseLine line acc
      | null stripped               = acc
      | "#" `isPrefixOf` stripped   = acc
      | '=' `notElem` stripped      = acc
      | otherwise =
          let (k, rest) = break (== '=') stripped
          in (trimSpace k, trimSpace (drop 1 rest)) : acc
      where stripped = trimSpace line

lookupKey :: String -> RawConfig -> Maybe String
lookupKey = lookup

-- ---------------------------------------------------------------------------
-- Defaults
-- ---------------------------------------------------------------------------

defaultOllamaUrl :: String
defaultOllamaUrl = "http://localhost:11434"

defaultModelId :: ModelId
defaultModelId = ModelId "gemma4:12b"

geminiDefaultBaseUrl :: String
geminiDefaultBaseUrl = "https://generativelanguage.googleapis.com/v1beta/openai"

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Load Config from dev.config at the given path.  Falls back to defaults
-- when the file does not exist or a key is missing.
loadDevConfig :: FilePath   -- ^ path to dev.config
              -> FilePath   -- ^ memory.json path
              -> FilePath   -- ^ sessions.json path
              -> IO Config
loadDevConfig devConfigPath memPath sessPath = do
  exists <- doesFileExist devConfigPath
  raw <- if exists
           then parseDevConfig <$> readFile devConfigPath
           else return []

  let ollamaUrl  = fromMaybe defaultOllamaUrl (lookupKey "OLLAMA_URL" raw)
      defModel   = fromMaybe (unModelId defaultModelId) (lookupKey "DEFAULT_MODEL" raw)
      timeoutSec = fromMaybe 120 (lookupKey "HTTP_TIMEOUT_SECONDS" raw >>= readMaybe)
      spinnerMs  = fromMaybe 80  (lookupKey "SPINNER_DELAY_MS"     raw >>= readMaybe)
      mApiKey    = lookupKey "GEMINI_API_KEY"  raw
      mGeminiUrl = lookupKey "GEMINI_BASE_URL" raw
      backend    = case mApiKey of
        Just apiKey ->
          let url = fromMaybe geminiDefaultBaseUrl mGeminiUrl
          in RemoteOpenAI url apiKey
        Nothing -> LocalOllama ollamaUrl

  return Config
    { modelBackend       = backend
    , fallbackModel      = ModelId defModel
    , httpTimeoutMicros  = timeoutSec * 1000000
    , spinnerDelayMicros = spinnerMs  * 1000
    , memoryFilePath     = memPath
    , sessionsFilePath   = sessPath
    }

-- | Write a well-commented example dev.config so users have a starting point.
writeExampleDevConfig :: FilePath -> IO ()
writeExampleDevConfig path = writeFile path $ unlines
  [ "# haskai-cli configuration"
  , "# Place this file at dev.config in the project root."
  , ""
  , "# --- Local Ollama backend ---"
  , "OLLAMA_URL=http://localhost:11434"
  , "DEFAULT_MODEL=gemma4:12b"
  , ""
  , "# --- Tuning ---"
  , "HTTP_TIMEOUT_SECONDS=120"
  , "SPINNER_DELAY_MS=80"
  , ""
  , "# --- Remote backend (Gemini via OpenAI-compatible API) ---"
  , "# Uncomment and fill in your key to switch from local Ollama to Gemini."
  , "# When GEMINI_API_KEY is set, OLLAMA_URL is ignored."
  , "#"
  , "# GEMINI_API_KEY=your-api-key-here"
  , "# GEMINI_BASE_URL=https://generativelanguage.googleapis.com/v1beta/openai"
  , "# DEFAULT_MODEL=gemini-2.0-flash"
  ]

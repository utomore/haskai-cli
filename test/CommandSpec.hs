{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CommandSpec (spec) where

import Test.Hspec
import Test.QuickCheck (Positive(..), property, (==>))
import qualified Data.Text as T
import Lib (parseCommand)
import Types (Command(..))

spec :: Spec
spec = do
  describe "parseCommand" $ do

    -- Exit / Quit
    it "parses /exit as CmdExit" $
      parseCommand "/exit" `shouldBe` Right CmdExit

    it "parses /quit as CmdExit" $
      parseCommand "/quit" `shouldBe` Right CmdExit

    -- Help
    it "parses /help" $
      parseCommand "/help" `shouldBe` Right CmdHelp

    -- Context
    it "parses /context" $
      parseCommand "/context" `shouldBe` Right CmdContext

    -- Session commands
    it "parses /session list" $
      parseCommand "/session list" `shouldBe` Right CmdSessionList

    it "parses /session new without a name" $
      parseCommand "/session new" `shouldBe` Right (CmdSessionNew Nothing)

    it "parses /session new with a name" $
      parseCommand "/session new my work session" `shouldBe` Right (CmdSessionNew (Just "my work session"))

    it "parses /session load with arg" $
      parseCommand "/session load 2" `shouldBe` Right (CmdSessionLoad "2")

    it "errors on bare /session load" $
      parseCommand "/session load" `shouldSatisfy` isLeft

    it "parses /session rename with new name" $
      parseCommand "/session rename new name" `shouldBe` Right (CmdSessionRename "new name")

    it "errors on bare /session rename" $
      parseCommand "/session rename" `shouldSatisfy` isLeft

    it "parses /session delete with arg" $
      parseCommand "/session delete 1" `shouldBe` Right (CmdSessionDelete "1")

    it "errors on bare /session delete" $
      parseCommand "/session delete" `shouldSatisfy` isLeft

    it "parses /session fork without a name" $
      parseCommand "/session fork" `shouldBe` Right (CmdSessionFork Nothing)

    it "parses /session fork with a name" $
      parseCommand "/session fork backup" `shouldBe` Right (CmdSessionFork (Just "backup"))

    -- File / Run commands
    it "parses /file without a question" $
      parseCommand "/file src/Lib.hs" `shouldBe` Right (CmdFile "src/Lib.hs" Nothing)
 
    it "parses /file with a question" $
      parseCommand "/file src/Lib.hs explain the functions" `shouldBe` Right (CmdFile "src/Lib.hs" (Just "explain the functions"))
 
    it "parses /file with double-quoted path and no question" $
      parseCommand "/file \"my folder/file name.hs\"" `shouldBe` Right (CmdFile "my folder/file name.hs" Nothing)
 
    it "parses /file with double-quoted path and a question" $
      parseCommand "/file \"my folder/file name.hs\" explain this" `shouldBe` Right (CmdFile "my folder/file name.hs" (Just "explain this"))
 
    it "errors on bare /file" $
      parseCommand "/file" `shouldSatisfy` isLeft

    it "parses /run with command" $
      parseCommand "/run cabal test" `shouldBe` Right (CmdRun "cabal test")

    it "errors on bare /run" $
      parseCommand "/run" `shouldSatisfy` isLeft

    it "parses /clear" $
      parseCommand "/clear" `shouldBe` Right CmdClear

    -- Prompt
    it "parses /prompt without text" $
      parseCommand "/prompt" `shouldBe` Right (CmdPrompt Nothing)

    it "parses /prompt with custom system prompt" $
      parseCommand "/prompt custom system prompt" `shouldBe` Right (CmdPrompt (Just "custom system prompt"))

    -- Summary
    it "parses /summary without text" $
      parseCommand "/summary" `shouldBe` Right (CmdSummary Nothing)

    it "parses /summary with custom instruction" $
      parseCommand "/summary 保留我問過的話題" `shouldBe` Right (CmdSummary (Just "保留我問過的話題"))

    -- Unfile
    it "parses /unfile without arguments" $
      parseCommand "/unfile" `shouldBe` Right (CmdUnfile Nothing)

    it "parses /unfile with index" $
      parseCommand "/unfile 2" `shouldBe` Right (CmdUnfile (Just "2"))

    it "parses /unfile with filename" $
      parseCommand "/unfile src/Lib.hs" `shouldBe` Right (CmdUnfile (Just "src/Lib.hs"))

    -- Unknown command
    it "errors on unknown command" $
      parseCommand "/unknown" `shouldSatisfy` isLeft

    -- Whitespace tolerance
    it "ignores leading/trailing whitespace around the command" $
      parseCommand "  /help  " `shouldBe` Right CmdHelp

    -- Properties
    it "any non-slash input is treated as an unknown command" $
      property $ \(input :: String) ->
        case input of
          (c:_) -> c /= '/' ==> parseCommand (T.pack input) `shouldSatisfy` isLeft
          []    -> property True

-- ---------------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight = not . isLeft

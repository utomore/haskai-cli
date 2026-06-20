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

    -- Memories
    it "parses /memories" $
      parseCommand "/memories" `shouldBe` Right CmdMemories

    it "parses /remember with a fact" $
      parseCommand "/remember I love Haskell" `shouldBe` Right (CmdRemember "I love Haskell")

    it "errors on bare /remember" $
      parseCommand "/remember" `shouldSatisfy` isLeft

    it "strips leading/trailing whitespace from remembered fact" $
      parseCommand "/remember   trimmed fact   " `shouldBe` Right (CmdRemember "trimmed fact")

    it "parses /forget with a valid index" $
      parseCommand "/forget 3" `shouldBe` Right (CmdForget 3)

    it "errors on /forget with non-numeric index" $
      parseCommand "/forget abc" `shouldSatisfy` isLeft

    it "errors on bare /forget" $
      parseCommand "/forget" `shouldSatisfy` isLeft

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

    it "any /remember with a non-empty fact parses successfully" $
      property $ \(fact :: String) ->
        not (null (filter (/= ' ') fact)) ==>
          parseCommand (T.pack ("/remember " ++ fact)) `shouldSatisfy` isRight

    it "any /forget with a positive integer parses successfully" $
      property $ \(n :: Positive Int) ->
        parseCommand (T.pack ("/forget " ++ show (getPositive n)))
          `shouldBe` Right (CmdForget (getPositive n))

-- ---------------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight = not . isLeft

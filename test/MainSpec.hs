{-# LANGUAGE OverloadedStrings #-}

module MainSpec (spec) where

import Test.Hspec
import Data.List (isInfixOf)
import qualified Data.Text as T
import Controller (assembleContext, filterLlmHistory)
import Types (Message(..))

spec :: Spec
spec = do
  describe "filterLlmHistory" $ do
    it "filters out transient system status/command messages but keeps conversational history and summary" $ do
      let history =
            [ Message "system" "已建立新會話: Session 1" Nothing Nothing Nothing
            , Message "user" "hello" Nothing Nothing Nothing
            , Message "assistant" "hi" Nothing Nothing Nothing
            , Message "system" "System Context Summary of past messages:\nOld conversation summary" Nothing Nothing Nothing
            , Message "system" "正在執行命令: run ls" Nothing Nothing Nothing
            , Message "tool" "file1.hs\nfile2.hs" Nothing (Just "tool-1") Nothing
            , Message "system" "=== HaskAI TUI 幫助選單 ===" Nothing Nothing Nothing
            ]
          clean = filterLlmHistory history
      clean `shouldBe`
        [ Message "user" "hello" Nothing Nothing Nothing
        , Message "assistant" "hi" Nothing Nothing Nothing
        , Message "system" "System Context Summary of past messages:\nOld conversation summary" Nothing Nothing Nothing
        , Message "tool" "file1.hs\nfile2.hs" Nothing (Just "tool-1") Nothing
        ]

  describe "assembleContext" $ do
    it "assembles context into XML structure" $ do
      let projPrompt = "Project instructions"
          mbFile = Just [("src/Lib.hs", "module Lib where")]
          history = [Message "user" "hello" Nothing Nothing Nothing]
          userQuestion = "explain the file"
      ctx <- assembleContext Nothing projPrompt mbFile history userQuestion
      ctx `shouldContain` "<SystemPrompt>"
      ctx `shouldContain` "<ProjectPrompt>\nProject instructions"
      ctx `shouldContain` "<AttachedFile filename=\"src/Lib.hs\">\nmodule Lib where"
      ctx `shouldContain` "<ChatHistory>"
      ctx `shouldContain` "<UserQuestion>\nexplain the file"

    it "filters transient system messages from assembled context" $ do
      let history =
            [ Message "system" "已載入會話: Session 2" Nothing Nothing Nothing
            , Message "user" "what is up" Nothing Nothing Nothing
            , Message "system" "=== HaskAI CLI Help ===" Nothing Nothing Nothing
            ]
      ctx <- assembleContext Nothing "" Nothing history ""
      ctx `shouldContain` "what is up"
      ctx `shouldNotContain` "已載入會話"
      ctx `shouldNotContain` "HaskAI CLI Help"


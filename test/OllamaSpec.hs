{-# LANGUAGE OverloadedStrings #-}

module OllamaSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString as B
import qualified Data.Aeson as Aeson
import Data.Word (Word8)
import Ollama

spec :: Spec
spec = do
  describe "splitBytes" $ do
    it "handles splitting empty bytestring" $ do
      splitBytes 10 B.empty `shouldBe` []

    it "handles splitting a string without the delimiter" $ do
      splitBytes 10 "hello" `shouldBe` ["hello"]

    it "splits correctly on single delimiter" $ do
      splitBytes 10 "hello\nworld" `shouldBe` ["hello", "world"]

    it "splits correctly on consecutive delimiters" $ do
      splitBytes 10 "hello\n\nworld" `shouldBe` ["hello", "", "world"]

    it "splits correctly when ending with delimiter" $ do
      splitBytes 10 "hello\n" `shouldBe` ["hello", ""]

    it "satisfies the property that joining the split parts reconstructs the original string" $
      property $ \w (NonNegative len) ->
        -- Generate random bytestrings
        forAll (vectorOf len arbitrary :: Gen [Word8]) $ \bytes ->
          let bs = B.pack bytes
              parts = splitBytes w bs
              reconstructed = B.intercalate (B.singleton w) parts
          in reconstructed == bs

    it "satisfies the property that no split parts contain the delimiter" $
      property $ \w (NonNegative len) ->
        forAll (vectorOf len arbitrary :: Gen [Word8]) $ \bytes ->
          let bs = B.pack bytes
              parts = splitBytes w bs
          in all (not . B.elem w) parts

  describe "JSON Deserialization" $ do
    it "correctly decodes ModelsResponse" $ do
      let jsonInput = "{\"object\":\"list\",\"data\":[{\"id\":\"gemma4:12b\"},{\"id\":\"llama3:latest\"}]}"
          decoded = Aeson.decode jsonInput :: Maybe ModelsResponse
      case decoded of
        Just (ModelsResponse items) -> do
          map modelId items `shouldBe` ["gemma4:12b", "llama3:latest"]
        Nothing -> expectationFailure "Failed to decode ModelsResponse"

    it "correctly decodes ChatCompletionChunk with delta content" $ do
      let jsonInput = "{\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}"
          decoded = Aeson.decode jsonInput :: Maybe ChatCompletionChunk
      case decoded of
        Just chunk -> do
          let deltas = map (deltaContent . choiceDelta) (choices chunk)
          deltas `shouldBe` [Just "hello"]
        Nothing -> expectationFailure "Failed to decode ChatCompletionChunk"

    it "correctly decodes ChatCompletionChunk with empty delta" $ do
      let jsonInput = "{\"choices\":[{\"delta\":{}}]}"
          decoded = Aeson.decode jsonInput :: Maybe ChatCompletionChunk
      case decoded of
        Just chunk -> do
          let deltas = map (deltaContent . choiceDelta) (choices chunk)
          deltas `shouldBe` [Nothing]
        Nothing -> expectationFailure "Failed to decode ChatCompletionChunk"

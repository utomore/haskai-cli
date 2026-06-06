{-# LANGUAGE OverloadedStrings #-}

module MainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (isInfixOf)
import qualified Data.Text as T
import Lib (buildSystemMessage)
import Types (Message(..))

spec :: Spec
spec = do
  describe "buildSystemMessage" $ do
    it "produces a base prompt when memories list is empty" $ do
      let msg = buildSystemMessage []
      role msg `shouldBe` "system"
      T.unpack (content msg) `shouldContain` "helpful and intelligent AI CLI assistant"
      T.unpack (content msg) `shouldNotContain` "facts you remember"

    it "includes memories in the prompt when they are present" $ do
      let mems = ["User likes Haskell", "User wants an AI tool"]
          msg = buildSystemMessage mems
      role msg `shouldBe` "system"
      T.unpack (content msg) `shouldContain` "User likes Haskell"
      T.unpack (content msg) `shouldContain` "User wants an AI tool"

    it "satisfies the property that all memories are present in the prompt" $
      property $ \memsStrings ->
        -- Filter out empty or whitespace-only strings to avoid trivial matches
        let mems = filter (not . T.null . T.strip) $ map T.pack memsStrings
            msg = buildSystemMessage mems
            contentStr = T.unpack (content msg)
        in all (\m -> T.unpack m `isInfixOf` contentStr) mems

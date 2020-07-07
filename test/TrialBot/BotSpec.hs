{-# LANGUAGE OverloadedStrings #-}

module TrialBot.BotSpec where

import Bot (
    getInt, getLatestSupportedUpdate, Chat (..), ChatID, Message (..), Offset, ResponseJSON (..),
    Update (..))
import Data.Text (Text)
import Prelude hiding (id)
import Test.Hspec

spec :: Spec
spec = do
    describe "getInt" .
        it "returns Int" $ getInt ("5" :: Text) `shouldBe` (5 :: Int)

    describe "getLatestSupportedUpdate" $ do
        let responseWithoutUpdates = (RJSON {ok = True, result = []})
        it "returns Nothing" $ getLatestSupportedUpdate responseWithoutUpdates `shouldBe` Nothing

    describe "getLatestSupportedUpdate" $ do
        let responseWithUpdates = RJSON {
            ok = True,
            result = [
                Update {
                    update_id = 858301203,
                    message = Just (Message {chat = Chat {id = 123456789}, text = Just "tost"}),
                    poll = Nothing}
            ]}
            latestSupportedUpdate = Just (Left (123456789,Just "tost"))
        it "returns latest supported update" $
            getLatestSupportedUpdate responseWithUpdates `shouldBe` latestSupportedUpdate

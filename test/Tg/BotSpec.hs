{-# LANGUAGE OverloadedStrings #-}

module Tg.BotSpec where

import Tg (
    getInt, getLatestSupportedUpdate, Chat (..), Message (..), ResponseJSON (..),
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
                    update_id = 858301205,
                    message = Just (Message {chat = Chat {id = 123456789}, text = Just "44"}),
                    poll = Nothing
                },
                Update {
                    update_id = 858301206,
                    message = Just (Message {chat = Chat {id = 123456789}, text = Just "11"}),
                    poll = Nothing
                },
                -- unsupported update
                Update {
                    update_id = 858301207,
                    message = Just (Message {chat = Chat {id = 123456789}, text = Nothing}),
                    poll = Nothing
                }
            ]}
            latestSupportedUpdate = Just (Left (123456789,Just "11"))
        it "returns latest supported update" $
            getLatestSupportedUpdate responseWithUpdates `shouldBe` latestSupportedUpdate

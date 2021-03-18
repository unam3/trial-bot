{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.BotSpec where

import Data.Text (Text)
import Prelude hiding (id)
import Test.Hspec

import Tg (getInt, getLatestSupportedUpdateContent)
import Tg.Requests.JSON

spec :: Spec
spec = do
    describe "getInt" .
        it "returns Int" $ getInt ("5" :: Text) `shouldBe` (5 :: Int)

    describe "getLatestSupportedUpdateContent" $ do
        let responseWithoutUpdates = (RJSON {ok = True, result = []})
        it "returns Nothing" $ getLatestSupportedUpdateContent responseWithoutUpdates `shouldBe` Nothing

    describe "getLatestSupportedUpdateContent" $ do
        let responseWithUpdates = RJSON {
            ok = True,
            result = [
                Update {
                    update_id = 858301205,
                    message = Just (Message {
                        chat = Chat {id = 123456789},
                        from = Just (User {_username = Just "A", _id = 111111111}),
                        text = Just "44"
                    }),
                    callback_query = Nothing
                },
                Update {
                    update_id = 858301206,
                    message = Just (Message {
                        chat = Chat {id = 123456789},
                        from = Just (User {_username = Just "B", _id = 222222222}),
                        text = Just "11"
                    }),
                    callback_query = Nothing
                },
                -- unsupported update
                Update {
                    update_id = 858301207,
                    message = Just (Message {
                        chat = Chat {id = 123456789},
                        from = Just (User {_username = Just "C", _id = 333333333}),
                        text = Nothing
                    }),
                    callback_query = Nothing
                }
            ]}
            latestSupportedUpdate = Just (Left (123456789, "11", "B", 222222222))
        it "returns latest supported update" $
            getLatestSupportedUpdateContent responseWithUpdates `shouldBe` latestSupportedUpdate

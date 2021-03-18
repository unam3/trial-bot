{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.Requests (
    answerCallbackQuery,
    getUpdates,
    isRepeatCommand,
    respondToMessage
) where

import Data.Map.Strict (findWithDefault)
import Data.Text (Text)
import Network.HTTP.Req

import Tg.Requests.JSON
import Tg.Types


withUpdatesOffset :: Offset -> RequestJSON
withUpdatesOffset updatesOffset = WithOffset {timeout = 20, offset = updatesOffset + 1}

getUpdates :: Config -> Maybe Offset -> IO ResponseJSON
getUpdates (tokenSection, _, _, _, _) maybeOffset = let {
    apiMethod = "getUpdates";
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    body = ReqBodyJson $
        maybe (WithoutOffset {timeout = 20}) withUpdatesOffset maybeOffset;
    runReqMonad = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> return (responseBody response :: ResponseJSON));
} in runReq defaultHttpConfig runReqMonad


isRepeatCommand :: Text -> Bool
isRepeatCommand = (== "/repeat");

respondToMessage :: Config -> ChatID -> Text -> Username -> UserID -> IO ResponseStatusJSON
respondToMessage (tokenSection, helpMsg, repeatMsg, echoRepeatNumberText, numberOfRepeatsMap) chatID msg username userID = let {
    apiMethod = "sendMessage";
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    echoRepeatNumber = findWithDefault echoRepeatNumberText userID numberOfRepeatsMap;
    commandOrText :: Text -> Text;
    commandOrText "/help" = helpMsg;
    commandOrText "/repeat" = mconcat [
        "@", username, " Current number of repeats is ", echoRepeatNumber, ". ", repeatMsg
    ];
    commandOrText t = t;
    request = EchoRequest {
        text = commandOrText msg,
        chat_id = chatID,
        reply_markup = if isRepeatCommand msg 
            then let {
                buttons = [[
                    InlineKeyboardButton {text = "1", callback_data = "repeat1"},
                    InlineKeyboardButton {text = "2", callback_data = "repeat2"},
                    InlineKeyboardButton {text = "3", callback_data = "repeat3"},
                    InlineKeyboardButton {text = "4", callback_data = "repeat4"},
                    InlineKeyboardButton {text = "5", callback_data = "repeat5"}
                ]];
            } in Just InlineKeyboardMarkup {inline_keyboard = buttons}
            else Nothing
    };
    body = ReqBodyJson request;
    runReqM = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> return (responseBody response :: ResponseStatusJSON));
} in runReq defaultHttpConfig runReqM


answerCallbackQuery :: Config -> CallbackQuery -> IO ResponseStatusJSON
answerCallbackQuery (tokenSection, _, _, _, _) callbackQuery = let {
    apiMethod = "answerCallbackQuery";
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    body = ReqBodyJson $ AnswerCallbackRequest { callback_query_id = (_id :: CallbackQuery -> Text)  callbackQuery};
    runReqMonad = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> return (responseBody response :: ResponseStatusJSON));
} in runReq defaultHttpConfig runReqMonad


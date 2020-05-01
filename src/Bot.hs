{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bot
    (
    cycleEcho
    ) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), (.:), (.:?), withObject, genericParseJSON, genericToJSON, defaultOptions, omitNothingFields, fieldLabelModifier)
import Data.Int (Int32, Int64)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text, append)
import GHC.Generics (Generic)
import Prelude hiding (id)
import Network.HTTP.Req


type Offset = Int32

data RequestJSON = WithoutOffset {
    timeout :: Int
} | WithOffset {
    timeout :: Int,
    offset :: Offset
} deriving (Show, Generic)

instance ToJSON RequestJSON
instance FromJSON RequestJSON


type ChatID = Int64

newtype Chat = Chat {
    id :: ChatID
} deriving (Show, Generic)

instance ToJSON Chat
instance FromJSON Chat where
    parseJSON = withObject "Chat" $ \v -> Chat
        <$> v .: "id"

data Message = Message {
    chat :: Chat,
    text :: Maybe Text
} deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> Message
        <$> v .: "chat"
        <*> v .:? "text"

mtext :: Message -> Maybe Text
mtext = text

data Update = Update {
    update_id :: Offset,
    message :: Maybe Message
} deriving (Show, Generic)

instance ToJSON Update
instance FromJSON Update where
    parseJSON = withObject "Update" $ \v -> Update
        <$> v .: "update_id"
        <*> v .:? "message"


newtype ResponseStatusJSON = RSJSON {
    ok :: Bool
} deriving (Show, Generic)

instance ToJSON ResponseStatusJSON
instance FromJSON ResponseStatusJSON where
    parseJSON = withObject "ResponseStatusJSON" $ \v -> RSJSON
        <$> v .: "ok"


data ResponseJSON = RJSON {
    ok :: Bool,
    result :: [Update]
} deriving (Show, Generic)

instance ToJSON ResponseJSON
instance FromJSON ResponseJSON where
    parseJSON = withObject "ResponseJSON" $ \v -> RJSON
        <$> v .: "ok"
        <*> v .: "result"


getUpdates :: (Text, Text, Text, Text) -> Maybe Offset -> IO ResponseJSON
getUpdates (token, _, _, _) maybeOffset = let {
    apiMethod = "getUpdates";
    tokenSection = append ("bot" :: Text) token;
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    body = ReqBodyJson $
        maybe (WithoutOffset {timeout = 20}) (\ offset -> WithOffset {timeout = 20, offset = offset + 1}) maybeOffset;
    runReqMonad = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> return (responseBody response :: ResponseJSON));
} in runReq defaultHttpConfig runReqMonad

getUpdateId :: ResponseJSON -> Maybe Offset
getUpdateId rjson = let {
    updates = result rjson;
} in if null updates then Nothing else Just (update_id $ last updates)

getTextMessages :: ResponseJSON -> IO (Maybe [(ChatID, Maybe Text)])
getTextMessages rjson = let {
    updates = result rjson;
} in return $ if null updates
    then Nothing
    else Just .
        fmap ((\ msg -> (id . chat  $ msg, mtext msg)) . fromJust) .
        filter (\ maybeMsg -> isJust maybeMsg && (isJust . mtext $ fromJust maybeMsg)) $
        fmap message updates


data KeyboardButtonPollType = KeyboardButtonPollType {
    _type :: Maybe Text
} deriving (Show, Generic)

instance ToJSON KeyboardButtonPollType where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = drop 1 }

instance FromJSON KeyboardButtonPollType where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = drop 1 }

data KeyboardButton = KeyboardButton {
    text :: Text,
    request_poll :: KeyboardButtonPollType
} deriving (Show, Generic)

instance ToJSON KeyboardButton
instance FromJSON KeyboardButton

data ReplyKeyboardMarkup = ReplyKeyboardMarkup {
    keyboard :: [[KeyboardButton]],
    one_time_keyboard :: Bool
} deriving (Show, Generic)

instance ToJSON ReplyKeyboardMarkup
instance FromJSON ReplyKeyboardMarkup


data EchoRequest = EchoRequest {
    chat_id :: ChatID,
    text :: Text
} deriving (Show, Generic)

instance ToJSON EchoRequest
instance FromJSON EchoRequest


data RepeatRequest = RepeatRequest {
    chat_id :: ChatID,
    question :: Text,
    options :: [Text]
    --reply_markup :: ReplyKeyboardMarkup
} deriving (Show, Generic)

instance ToJSON RepeatRequest
instance FromJSON RepeatRequest



isRepeat :: Maybe Text -> Bool
isRepeat (Just "/repeat") = True;
isRepeat _ = False;

sendMessage :: (Text, Text, Text, Text) -> ChatID -> Maybe Text -> IO ResponseStatusJSON
sendMessage (token, helpMsg, repeatMsg, echoRepeatNumber) chatID maybeText  = let {
    apiMethod = "sendMessage";
    tokenSection = append ("bot" :: Text) token;
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    commandOrText :: Text -> Text;
    commandOrText "/help" = helpMsg;
    commandOrText "/repeat" = mconcat ["Current number of repeats is ", echoRepeatNumber, ". ", repeatMsg];
    commandOrText text = text;
    request = EchoRequest {
        text = maybe "default answer if no \"text\" field" commandOrText maybeText,
        chat_id = chatID
    };
    body = ReqBodyJson request;
    runReqM = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> return (responseBody response :: ResponseStatusJSON));
} in runReq defaultHttpConfig runReqM

sendPoll :: (Text, Text, Text, Text) -> ChatID -> IO ResponseStatusJSON
sendPoll (token, helpMsg, repeatMsg, echoRepeatNumber) chatID = let {
    apiMethod = "sendPoll";
    tokenSection = append ("bot" :: Text) token;
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    request = RepeatRequest {
        question = mconcat ["Current number of repeats is ", echoRepeatNumber, ". ", repeatMsg],
        chat_id = chatID,
        options = ["1", "2", "3", "4", "5"]
        --reply_markup =
        --    let {
        --        requestPoll = KeyboardButtonPollType {_type = Nothing};
        --        buttons = [[KeyboardButton {text = "1", request_poll = requestPoll}, KeyboardButton {text = "2", request_poll = requestPoll}, KeyboardButton {text = "3", request_poll = requestPoll},
        --            KeyboardButton {text = "4", request_poll = requestPoll}, KeyboardButton {text = "5", request_poll = requestPoll}]];
        --    } in ReplyKeyboardMarkup {keyboard = buttons, one_time_keyboard = True}
    };
    body = ReqBodyJson request;
    runReqM = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> return (responseBody response :: ResponseStatusJSON));
} in runReq defaultHttpConfig runReqM


cycleEcho' :: (Text, Text, Text, Text) -> ResponseJSON -> IO ResponseJSON
cycleEcho' args rjson = getUpdates args (getUpdateId rjson) >>=
    \ ioRJSON -> print ioRJSON
    >> getTextMessages ioRJSON
    >>= \ textMessages -> print textMessages
    >> case textMessages of
        Nothing -> return ()
        Just list -> forM_ list (\ (chatID, maybeText) -> if isRepeat maybeText
            then sendPoll args chatID
            else sendMessage args chatID maybeText)
    >> cycleEcho' args ioRJSON

cycleEcho :: (Text, Text, Text, Text) -> IO ResponseJSON
cycleEcho args = getUpdates args Nothing >>=
    \ ioRJSON -> print ioRJSON
    >> getTextMessages ioRJSON
    >>= \ textMessages -> print textMessages
    >> case textMessages of
        Nothing -> return ()
        Just list -> forM_ list (\ (chatID, maybeText) -> if isRepeat maybeText
            then sendPoll args chatID
            else sendMessage args chatID maybeText)
    >> cycleEcho' args ioRJSON

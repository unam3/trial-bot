{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bot
    (
    cycleEcho
    ) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), (.:), (.:?), withObject, genericParseJSON, genericToJSON, defaultOptions, omitNothingFields)
import Data.Int (Int32, Int64)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text, append, pack)
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


type UserID = Int32

newtype User = User {
    id :: UserID
} deriving (Show, Generic)

instance ToJSON User
instance FromJSON User


data Message = Message {
    chat :: Chat,
    text :: Maybe Text,
    from :: Maybe User
} deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> Message
        <$> v .: "chat"
        <*> v .:? "text"
        <*> v .:? "from"

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

getTextMessages :: ResponseJSON -> IO (Maybe [(ChatID, Maybe Text, Maybe UserID)])
getTextMessages rjson = let {
    updates = result rjson;
} in return $ if null updates
    then Nothing
    else Just .
        fmap (
            (\ msg -> (
                (id :: Chat -> ChatID) $ chat msg,
                mtext msg,
                maybe Nothing (Just . (id :: User -> UserID)) $ from msg 
            )) . fromJust
        ) .
        filter (\ maybeMsg ->   
            isJust maybeMsg
            && (isJust . mtext $ fromJust maybeMsg)
            -- && (isJust . from $ fromJust maybeMsg)
        ) $
        fmap message updates


newtype KeyboardButton = KeyboardButton {
    text :: Text
} deriving (Show, Generic)

instance ToJSON KeyboardButton
instance FromJSON KeyboardButton

data ReplyKeyboardMarkup = ReplyKeyboardMarkup {
    keyboard :: [[KeyboardButton]],
    one_time_keyboard :: Bool,
    selective :: Bool
} deriving (Show, Generic)

instance ToJSON ReplyKeyboardMarkup
instance FromJSON ReplyKeyboardMarkup


data EchoRequest = EchoRequest {
    chat_id :: ChatID,
    text :: Text,
    reply_markup :: Maybe ReplyKeyboardMarkup
} deriving (Show, Generic)

instance ToJSON EchoRequest where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance FromJSON EchoRequest where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }


sendMessage :: (Text, Text, Text, Text) -> ChatID -> Maybe Text -> Maybe UserID -> IO ResponseStatusJSON
sendMessage (token, helpMsg, repeatMsg, echoRepeatNumber) chatID maybeText maybeUserID = let {
    apiMethod = "sendMessage";
    tokenSection = append ("bot" :: Text) token;
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    mention = pack . show $ fromJust maybeUserID;
    commandOrText :: Text -> Text;
    commandOrText "/help" = helpMsg;
    commandOrText "/repeat" = mconcat ["@", mention, " Current number of repeats is ", echoRepeatNumber, ". ", repeatMsg];
    commandOrText text = text;
    isRepeat (Just "/repeat") = True;
    isRepeat _ = False;
    echoRequest = EchoRequest {
        text = maybe "default answer if no \"text\" field" commandOrText maybeText,
        chat_id = chatID,
        reply_markup = if isRepeat maybeText 
            then let {
                buttons = [[KeyboardButton {text = "1"}, KeyboardButton {text = "2"}, KeyboardButton {text = "3"},
                    KeyboardButton {text = "4"}, KeyboardButton {text = "5"}]];
            } in Just ReplyKeyboardMarkup {keyboard = buttons, one_time_keyboard = True, selective = True}
            else Nothing
    };
    body = ReqBodyJson echoRequest;
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
        Just list -> forM_ list (\ (chatID, maybeText, maybeUserID) -> sendMessage args chatID maybeText maybeUserID)
    >> cycleEcho' args ioRJSON

cycleEcho :: (Text, Text, Text, Text) -> IO ResponseJSON
cycleEcho args = getUpdates args Nothing >>=
    \ ioRJSON -> print ioRJSON
    >> getTextMessages ioRJSON
    >>= \ textMessages -> print textMessages
    >> case textMessages of
        Nothing -> return ()
        Just list -> forM_ list (\ (chatID, maybeText, maybeUserID) -> sendMessage args chatID maybeText maybeUserID)
    >> cycleEcho' args ioRJSON

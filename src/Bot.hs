{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bot
    (
    cycleEcho
    ) where

import Control.Monad (replicateM_, void)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), (.:), (.:?), withObject, genericParseJSON, genericToJSON, defaultOptions, omitNothingFields, fieldLabelModifier)
import Data.Either (fromRight)
import Data.Int (Int32, Int64)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text, append, pack)
import Data.Text.Read (decimal)
import GHC.Generics (Generic)
import Prelude hiding (id)
--import qualified Prelude (id)
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

data PollOption = PollOption {
    text :: Text,
    voter_count :: Int
} deriving (Show, Generic)

instance ToJSON PollOption
instance FromJSON PollOption where
    parseJSON = withObject "PollOption" $ \v -> PollOption
        <$> v .: "text"
        <*> v .: "voter_count"

newtype Poll = Poll {
    options :: [PollOption]
} deriving (Show, Generic)

instance ToJSON Poll
instance FromJSON Poll where
    parseJSON = withObject "Poll" $ \v -> Poll
        <$> v .: "options"

data Message = Message {
    chat :: Chat,
    text :: Maybe Text
} deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> Message
        <$> v .: "chat"
        <*> v .:? "text"

data Update = Update {
    update_id :: Offset,
    message :: Maybe Message,
    poll :: Maybe Poll
} deriving (Show, Generic)

instance ToJSON Update
instance FromJSON Update where
    parseJSON = withObject "Update" $ \v -> Update
        <$> v .: "update_id"
        <*> v .:? "message"
        <*> v .:? "poll"


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


withUpdatesOffset :: Offset -> RequestJSON
withUpdatesOffset updatesOffset = WithOffset {timeout = 20, offset = updatesOffset + 1}

getUpdates :: (Text, Text, Text, Text) -> Maybe Offset -> IO ResponseJSON
getUpdates (token, _, _, _) maybeOffset = let {
    apiMethod = "getUpdates";
    tokenSection = append ("bot" :: Text) token;
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    body = ReqBodyJson $
        maybe (WithoutOffset {timeout = 20}) withUpdatesOffset maybeOffset;
    runReqMonad = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> return (responseBody response :: ResponseJSON));
} in runReq defaultHttpConfig runReqMonad

getUpdateId :: ResponseJSON -> Maybe Offset
getUpdateId rjson = let {
    updates = result rjson;
} in if null updates then Nothing else Just (update_id $ last updates)

newtype KeyboardButtonPollType = KeyboardButtonPollType {
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
sendMessage (token, helpMsg, _, _) chatID maybeText = let {
    apiMethod = "sendMessage";
    tokenSection = append ("bot" :: Text) token;
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    commandOrText :: Text -> Text;
    commandOrText "/help" = helpMsg;
    commandOrText t = t;
    request = EchoRequest {
        text = maybe "default answer if no \"text\" field" commandOrText maybeText,
        chat_id = chatID
    };
    body = ReqBodyJson request;
    runReqM = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> return (responseBody response :: ResponseStatusJSON));
} in runReq defaultHttpConfig runReqM

sendPoll :: (Text, Text, Text, Text) -> ChatID -> IO ResponseStatusJSON
sendPoll (token, _, repeatMsg, echoRepeatNumber) chatID = let {
    apiMethod = "sendPoll";
    tokenSection = append ("bot" :: Text) token;
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    request = RepeatRequest {
        question = mconcat ["Current number of repeats is ", echoRepeatNumber, ". ", repeatMsg],
        chat_id = chatID,
        options = ["1", "2", "3", "4", "5"]
    };
    body = ReqBodyJson request;
    runReqM = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> return (responseBody response :: ResponseStatusJSON));
} in runReq defaultHttpConfig runReqM

getInt :: Text -> Int
getInt = fst . fromRight (1, pack "1") . decimal

type MaybeUpdateContent = Maybe (Either (ChatID, Maybe Text) PollOption)

getSupportedUpdate :: [Update] -> MaybeUpdateContent
getSupportedUpdate (update : updateList) = let {
    maybeMessage = message update;
    maybeText = maybe Nothing (text :: Message -> Maybe Text) maybeMessage;
    maybeUpdateWithPoll = poll update;
    extractVotedPollOption = head . filter ((> 0) . voter_count) . (options :: Poll -> [PollOption]);
} in if isJust maybeText
    then Just $ Left (id . (chat :: Message -> Chat) $ fromJust maybeMessage, maybeText)
    else maybe (getSupportedUpdate updateList) (Just . Right . extractVotedPollOption) maybeUpdateWithPoll
getSupportedUpdate [] = Nothing

getLatestSupportedUpdate :: ResponseJSON -> MaybeUpdateContent
getLatestSupportedUpdate rjson = let {
    updates = result rjson;
} in getSupportedUpdate $ reverse updates

cycleEcho' :: (Text, Text, Text, Text) -> Maybe ResponseJSON -> IO ResponseJSON
cycleEcho' args@(_, _, _, echoRepeatNumberText) maybeRJSON = let {
        maybeOffset = maybe Nothing getUpdateId maybeRJSON;
    } in getUpdates args maybeOffset >>=

    \ ioRJSON -> print ioRJSON

    >> return (getLatestSupportedUpdate ioRJSON)
    >>= \ latestSupportedUpdate -> print latestSupportedUpdate
    >> case latestSupportedUpdate of
        Just (Left (chatID, maybeText)) -> if isRepeat maybeText
            then void $ sendPoll args chatID
            else replicateM_ (getInt echoRepeatNumberText) (sendMessage args chatID maybeText)
        _ -> return ()

    >> let {
        (token, helpMsg, repeatMsg, _) = args;
        args' = case latestSupportedUpdate of
            Just (Right pollOption) -> (token, helpMsg, repeatMsg, (text :: PollOption -> Text) pollOption)
            _ -> args;
    } in cycleEcho' args' $ Just ioRJSON

cycleEcho :: (Text, Text, Text, Text) -> IO ResponseJSON
cycleEcho args = let {
    noRJSON = Nothing;
} in cycleEcho' args noRJSON

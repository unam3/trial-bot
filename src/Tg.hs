{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tg
    (
    getInt,
    getLatestSupportedUpdate,
    startBotWithLogger,
    Chat (..),
    ChatID,
    Message (..),
    Offset,
    ResponseJSON (..),
    Update (..)
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
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, setLevel, traplogging, updateGlobalLogger)


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
} deriving (Show, Generic, Eq)

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
instance FromJSON Update


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
getUpdates (tokenSection, _, _, _) maybeOffset = let {
    apiMethod = "getUpdates";
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


type TokenSection = Text
type HelpMessage = Text
type RepeatMessage = Text
type NumberOfRepeats = Text
type Config = (TokenSection, HelpMessage, RepeatMessage, NumberOfRepeats)

isRepeat :: Maybe Text -> Bool
isRepeat (Just "/repeat") = True;
isRepeat _ = False;

sendMessage :: Config -> ChatID -> Maybe Text -> IO ResponseStatusJSON
sendMessage (tokenSection, helpMsg, _, _) chatID maybeText = let {
    apiMethod = "sendMessage";
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

sendPoll :: Config -> ChatID -> IO ResponseStatusJSON
sendPoll (tokenSection, _, repeatMsg, echoRepeatNumber) chatID = let {
    apiMethod = "sendPoll";
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
getInt = fst . fromRight (1, "1") . decimal

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

cycleEcho' :: Config -> Maybe ResponseJSON -> IO ResponseJSON
cycleEcho' config@(_, _, _, echoRepeatNumberText) maybeRJSON = let {
        maybeOffset = maybe Nothing getUpdateId maybeRJSON;
    } in getUpdates config maybeOffset >>=

    \ ioRJSON -> debugM "trial-bot.bot" (show ioRJSON)

    >> return (getLatestSupportedUpdate ioRJSON)
    >>= \ latestSupportedUpdate -> debugM "trial-bot.bot" (show latestSupportedUpdate)
    >> case latestSupportedUpdate of
        Just (Left (chatID, maybeText)) -> if isRepeat maybeText
            then void $ sendPoll config chatID
            else replicateM_ (getInt echoRepeatNumberText) (sendMessage config chatID maybeText)
        _ -> return ()

    >> let {
        (tokenSection, helpMsg, repeatMsg, _) = config;
        config' = case latestSupportedUpdate of
            Just (Right pollOption) -> (tokenSection, helpMsg, repeatMsg, (text :: PollOption -> Text) pollOption)
            _ -> config;
    } in cycleEcho' config' $ Just ioRJSON

cycleEcho :: Config -> IO ResponseJSON
cycleEcho config = let {
    noRJSON = Nothing;
} in updateGlobalLogger "trial-bot.bot" (setLevel DEBUG)
    >> cycleEcho' config noRJSON


processArgs :: [String] -> Maybe Config
processArgs [token, helpMsg, repeatMsg, echoRepeatNumberStr] = let {
    echoRepeatNumber = (read echoRepeatNumberStr :: Int);
    isInRange n = n > 0 && n < 6;
} in if or [null token, null helpMsg, null repeatMsg, not $ isInRange echoRepeatNumber]
    then Nothing
    else Just (append "bot" $ pack token, pack helpMsg, pack repeatMsg, pack echoRepeatNumberStr)
processArgs _ = Nothing

startBot :: [String] -> IO ()
startBot args =
    case args of
        [_, _, _, _] -> case processArgs args of
            Just args' -> void $ cycleEcho args'
            Nothing -> error "error: some argument passed from command line is wrong"
        _ -> error "error: exactly four arguments needed: token, helpMsg, repeatMsg, echoRepeatNumber"


startBotWithLogger :: [String] -> IO ()
startBotWithLogger args = traplogging "trial-bot.main" ERROR "Bot shutdown due to" $ startBot args

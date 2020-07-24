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


type Username = Text

newtype User = User {
    username :: Username
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

data Update = Update {
    update_id :: Offset,
    message :: Maybe Message,
    --
    poll :: Maybe Poll
    --
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


data KeyboardButton = KeyboardButton {
    text :: Text
} deriving (Show, Generic)

instance ToJSON KeyboardButton


data ReplyKeyboardMarkup = ReplyKeyboardMarkup {
    keyboard :: [[KeyboardButton]],
    one_time_keyboard :: Bool,
    selective :: Bool
} deriving (Show, Generic)

instance ToJSON ReplyKeyboardMarkup


data EchoRequest = EchoRequest {
    chat_id :: ChatID,
    text :: Text,
    reply_markup :: Maybe ReplyKeyboardMarkup
} deriving (Show, Generic)

instance ToJSON EchoRequest where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }
 

type TokenSection = Text
type HelpMessage = Text
type RepeatMessage = Text
type NumberOfRepeats = Text
type Config = (TokenSection, HelpMessage, RepeatMessage, NumberOfRepeats)
-- data State = State {
--     config :: Config,
-- 
-- }

respondToMessage :: Config -> ChatID -> Maybe Text -> Maybe Username -> IO ResponseStatusJSON
respondToMessage (tokenSection, helpMsg, echoRepeatNumber, repeatMsg) chatID maybeText maybeUsername = let {
    apiMethod = "sendMessage";
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    mention = fromJust maybeUsername;
    commandOrText :: Text -> Text;
    commandOrText "/help" = helpMsg;
    commandOrText "/repeat" = mconcat [
        "@", mention, " Current number of repeats is ", echoRepeatNumber, ". ", repeatMsg
    ];
    commandOrText t = t;
    request = EchoRequest {
        text = maybe "default answer if no \"text\" field" commandOrText maybeText,
        chat_id = chatID,
        reply_markup = if isRepeat maybeText 
            then let {
                buttons = [[
                    KeyboardButton {text = "1"},
                    KeyboardButton {text = "2"},
                    KeyboardButton {text = "3"},
                    KeyboardButton {text = "4"},
                    KeyboardButton {text = "5"}
                ]];
            } in Just ReplyKeyboardMarkup {keyboard = buttons, one_time_keyboard = True, selective = True}
            else Nothing
    };
    body = ReqBodyJson request;
    runReqM = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> return (responseBody response :: ResponseStatusJSON));
} in runReq defaultHttpConfig runReqM

getInt :: Text -> Int
getInt = fst . fromRight (1, "1") . decimal

data PollOption = PollOption {
    text :: Text,
    voter_count :: Int
} deriving (Show, Generic, Eq)

instance ToJSON PollOption
instance FromJSON PollOption where
    parseJSON = withObject "PollOption" $ \v -> PollOption
        <$> v .: "text"
        <*> v .: "voter_count"

type MaybeUpdateContent = Maybe (Either (ChatID, Maybe Text, Maybe Username) PollOption)


--
newtype Poll = Poll {
    options :: [PollOption]
} deriving (Show, Generic)
 
instance ToJSON Poll
instance FromJSON Poll where
    parseJSON = withObject "Poll" $ \v -> Poll
        <$> v .: "options"
--

getSupportedUpdate :: [Update] -> MaybeUpdateContent
getSupportedUpdate (update : updateList) = let {
    maybeMessage = message update;
    maybeText = maybe Nothing (text :: Message -> Maybe Text) maybeMessage;
    maybeUsername = maybe Nothing (maybe Nothing (Just . username) . from) maybeMessage;

    maybeUpdateWithPoll = poll update;
    extractVotedPollOption = head . filter ((> 0) . voter_count) . (options :: Poll -> [PollOption]);

} in if isJust maybeText && isJust maybeUsername
    then Just $ Left (id . (chat :: Message -> Chat) $ fromJust maybeMessage, maybeText, maybeUsername)
    else maybe (getSupportedUpdate updateList) (Just . Right . extractVotedPollOption) maybeUpdateWithPoll
getSupportedUpdate [] = Nothing

getLatestSupportedUpdate :: ResponseJSON -> MaybeUpdateContent
getLatestSupportedUpdate rjson = let {
    updates = result rjson;
} in getSupportedUpdate $ reverse updates


isRepeat :: Maybe Text -> Bool
isRepeat = maybe False (== "/repeat");

isHelp :: Maybe Text -> Bool
isHelp = maybe False (== "/help");

cycleEcho' :: Config -> Maybe ResponseJSON -> IO ResponseJSON
cycleEcho' config@(_, _, _, echoRepeatNumberText) maybeRJSON = let {
        maybeOffset = maybe Nothing getUpdateId maybeRJSON;
    } in getUpdates config maybeOffset >>=

    \ ioRJSON -> debugM "trial-bot.bot" (show ioRJSON)

    >> return (getLatestSupportedUpdate ioRJSON)
    >>= \ latestSupportedUpdate -> debugM "trial-bot.bot" (show latestSupportedUpdate)
    >> case latestSupportedUpdate of
        Just (Left (chatID, maybeText, maybeUsername)) ->
            let {
                numberOfRepeats = if isRepeat maybeText || isHelp maybeText
                    then 1
                    else getInt echoRepeatNumberText;
            } in replicateM_ numberOfRepeats $ respondToMessage config chatID maybeText maybeUsername
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

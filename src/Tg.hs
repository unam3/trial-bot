{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tg
    (
    getInt,
    getLatestSupportedUpdateContent,
    startBotWithLogger,
    Chat (..),
    ChatID,
    Message (..),
    Offset,
    ResponseJSON (..),
    Update (..),
    User (..)
    ) where

import Control.Monad (replicateM_, void)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), (.:), (.:?), withObject, genericParseJSON, genericToJSON, defaultOptions, omitNothingFields, fieldLabelModifier)
import Data.Either (fromRight)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, fromJust)
import Data.Text (Text, append, pack)
import Data.Text.Read (decimal)
import GHC.Generics (Generic)
import Prelude hiding (id)
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

data User = User {
    _username :: Maybe Username,
    _id :: UserID
} deriving (Show, Generic, Eq)

--instance ToJSON User
instance FromJSON User where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}


data Message = Message {
    chat :: Chat,
    text :: Maybe Text,
    from :: Maybe User
} deriving (Show, Generic)

instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> Message
        <$> v .: "chat"
        <*> v .:? "text"
        <*> v .:? "from"


data CallbackQuery = CallbackQuery {
    _id :: Text,
    _data :: Text,
    _from :: User
} deriving (Show, Generic, Eq)

--instance Eq CallbackQuery
instance FromJSON CallbackQuery where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}


data Update = Update {
    update_id :: Offset,
    message :: Maybe Message,
    callback_query :: Maybe CallbackQuery
} deriving (Show, Generic)

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

instance FromJSON ResponseJSON where
    parseJSON = withObject "ResponseJSON" $ \v -> RJSON
        <$> v .: "ok"
        <*> v .: "result"


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

getUpdateId :: ResponseJSON -> Maybe Offset
getUpdateId rjson = let {
    updates = result rjson;
} in if null updates then Nothing else Just (update_id $ last updates)


data InlineKeyboardButton = InlineKeyboardButton {
    text :: Text,
    -- 1-64 bytes ( https://core.telegram.org/bots/api#inlinekeyboardbutton  )
    callback_data :: Text
} deriving (Show, Generic)

instance ToJSON InlineKeyboardButton


newtype InlineKeyboardMarkup = InlineKeyboardMarkup {
    inline_keyboard :: [[InlineKeyboardButton]]
} deriving (Show, Generic)

instance ToJSON InlineKeyboardMarkup


data EchoRequest = EchoRequest {
    chat_id :: ChatID,
    text :: Text,
    reply_markup :: Maybe InlineKeyboardMarkup
} deriving (Show, Generic)

instance ToJSON EchoRequest where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }
 

type TokenSection = Text
type HelpMessage = Text
type RepeatMessage = Text
type UserID = Int64
type NumberOfRepeats = Text
type NumberOfRepeatsMap = M.Map UserID NumberOfRepeats
type Config = (TokenSection, HelpMessage, RepeatMessage, NumberOfRepeats, NumberOfRepeatsMap)

respondToMessage :: Config -> ChatID -> Text -> Username -> UserID -> IO ResponseStatusJSON
respondToMessage (tokenSection, helpMsg, repeatMsg, echoRepeatNumberText, numberOfRepeatsMap) chatID msg username userID = let {
    apiMethod = "sendMessage";
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    echoRepeatNumber = M.findWithDefault echoRepeatNumberText userID numberOfRepeatsMap;
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

getInt :: Text -> Int
getInt = fst . fromRight (1, "1") . decimal

type MaybeUpdateContent = Maybe (Either (ChatID, Text, Username, UserID) CallbackQuery)

getSupportedUpdateContent :: [Update] -> MaybeUpdateContent
getSupportedUpdateContent (update : updateList) = let {
    maybeMessage = message update;
    chatID = id . (chat :: Message -> Chat) $ fromJust maybeMessage;
    maybeText = maybe Nothing (text :: Message -> Maybe Text) maybeMessage;
    maybeUser = maybe Nothing from maybeMessage;
    maybeUsername = maybe Nothing _username maybeUser;
    --maybeUserID = maybe Nothing (_id :: User -> UserID) maybeUser;
    userID = (_id :: User -> UserID) $ fromJust maybeUser;
    maybeCallbackQuery = callback_query update;
} in if isJust maybeText && isJust maybeUsername
    then Just $ Left (
        chatID,
        fromJust maybeText,
        fromJust maybeUsername,
        userID
    )
    else maybe (getSupportedUpdateContent updateList) (Just . Right) maybeCallbackQuery
getSupportedUpdateContent [] = Nothing

getLatestSupportedUpdateContent :: ResponseJSON -> MaybeUpdateContent
getLatestSupportedUpdateContent rjson = let {
    updates = result rjson;
} in getSupportedUpdateContent $ reverse updates


isRepeatCommand :: Text -> Bool
isRepeatCommand = (== "/repeat");

isHelpCommand :: Text -> Bool
isHelpCommand = (== "/help");

getNumberOfRepeats :: Text -> Text
getNumberOfRepeats "repeat5" = "5"
getNumberOfRepeats "repeat4" = "4"
getNumberOfRepeats "repeat3" = "3"
getNumberOfRepeats "repeat2" = "2"
getNumberOfRepeats _ = "1"

newtype AnswerCallbackRequest = AnswerCallbackRequest {
    callback_query_id :: Text
} deriving (Show, Generic)

instance ToJSON AnswerCallbackRequest

answerCallbackQuery :: Config -> CallbackQuery -> IO ResponseStatusJSON
answerCallbackQuery (tokenSection, _, _, _, _) callbackQuery = let {
    apiMethod = "answerCallbackQuery";
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    body = ReqBodyJson $ AnswerCallbackRequest { callback_query_id = (_id :: CallbackQuery -> Text)  callbackQuery};
    runReqMonad = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> return (responseBody response :: ResponseStatusJSON));
} in runReq defaultHttpConfig runReqMonad


processUpdates :: Config -> ResponseJSON -> IO Config
processUpdates config@(token, helpMsg, repeatMsg, echoRepeatNumberText, numberOfRepeatsMap) ioRJSON =
    let {
        latestSupportedUpdateContent = getLatestSupportedUpdateContent ioRJSON;
    } in case latestSupportedUpdateContent of
        Just (Left (chatID, msg, username, userID)) ->
            let {
                numberOfRepeats = if isRepeatCommand msg || isHelpCommand msg
                    then 1
                    else getInt $ M.findWithDefault echoRepeatNumberText userID numberOfRepeatsMap;
            } in replicateM_ numberOfRepeats (respondToMessage config chatID msg username userID)
            >> return config
        -- https://core.telegram.org/bots/api#answercallbackquery
        Just (Right callbackQuery) ->
            void (answerCallbackQuery config callbackQuery)
            >> let {
                userID = (_id :: User -> UserID) $ _from callbackQuery;
                newNumberOfRepeats = getNumberOfRepeats $ _data callbackQuery;
                newNumberOfRepeatsMap = M.insert userID newNumberOfRepeats numberOfRepeatsMap;
            } in return (token, helpMsg, repeatMsg, echoRepeatNumberText, newNumberOfRepeatsMap)
        _ -> return config
  

cycleEcho' :: Config -> Maybe ResponseJSON -> IO ResponseJSON
cycleEcho' config maybeRJSON =
    let {
        maybeOffset = maybe Nothing getUpdateId maybeRJSON;
    } in getUpdates config maybeOffset >>=

    \ ioRJSON -> debugM "trial-bot.bot" (show ioRJSON)

    >> processUpdates config ioRJSON

    >>= \ config' -> cycleEcho' config' $ Just ioRJSON

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
    else Just (append "bot" $ pack token, pack helpMsg, pack repeatMsg, pack echoRepeatNumberStr, M.empty)
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

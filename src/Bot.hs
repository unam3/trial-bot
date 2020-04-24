{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Bot
    (
    cycleUpdate
    ) where

import Data.Aeson (FromJSON (parseJSON), ToJSON, (.:), (.:?), withObject)
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

data Chat = Chat {
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

data Update = Update {
    update_id :: Offset,
    message :: Maybe Message
} deriving (Show, Generic)

instance ToJSON Update
instance FromJSON Update where
    parseJSON = withObject "Update" $ \v -> Update
        <$> v .: "update_id"
        <*> v .:? "message"

data ResponseJSON = RJSON {
    ok :: Bool,
    result :: [Update]
} deriving (Show, Generic)

instance ToJSON ResponseJSON
instance FromJSON ResponseJSON where
    parseJSON = withObject "ResponseJSON" $ \v -> RJSON
        <$> v .: "ok"
        <*> v .: "result"


getUpdates :: (String, String, String, Int) -> Maybe Offset -> IO ResponseJSON
getUpdates (token, helpMsg, repeatMsg, echoRepeatNumber) maybeOffset = let {
    apiMethod = "getUpdates";
    tokenSection = append ("bot" :: Text) $ pack token;
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

getTextMessages :: ResponseJSON -> IO (Maybe [Maybe Text])
getTextMessages rjson = let {
    updates = result rjson;
} in return $ if null updates
    then Nothing
    else Just .
        fmap (text . fromJust) .
        filter (\ maybeMsg -> isJust maybeMsg && ((maybe False (const True)) . text $ fromJust maybeMsg)) $
        fmap message updates

cycleUpdate' :: (String, String, String, Int) -> ResponseJSON -> IO ResponseJSON
cycleUpdate' args rjson = getUpdates args (getUpdateId rjson) >>=
    \ ioRJSON -> print ioRJSON
    >> getTextMessages ioRJSON
    >>= \ textMessages -> print textMessages
    >> cycleUpdate' args ioRJSON

cycleUpdate :: (String, String, String, Int) -> IO ResponseJSON
cycleUpdate args = getUpdates args Nothing >>= cycleUpdate' args

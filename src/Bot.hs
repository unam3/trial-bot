{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Bot
    (
    cycleUpdate
    ) where

import Data.Aeson (FromJSON (parseJSON), ToJSON, (.:), withObject)
import Data.Int (Int32)
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


newtype Update = Update {
    update_id :: Offset
}  deriving (Show, Generic)

instance ToJSON Update
instance FromJSON Update where
    parseJSON = withObject "Update" $ \v -> Update
        <$> v .: "update_id"


data ResponseJSON = RJSON {
    ok :: Bool,
    result :: [Update]
    --result :: ByteString
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
    body = ReqBodyJson (WithoutOffset 20);
    runReqMonad = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> return (responseBody response :: ResponseJSON));
} in runReq defaultHttpConfig runReqMonad

getUpdateId :: ResponseJSON -> Maybe Offset
getUpdateId rjson = let {
    updates = result rjson;
} in if null updates then Nothing else Just (update_id $ last updates)

cycleUpdate' :: (String, String, String, Int) -> ResponseJSON -> IO ResponseJSON
cycleUpdate' args rjson = getUpdates args (getUpdateId rjson) >>=
    \ ioRJSON -> print ioRJSON
    >> cycleUpdate' args ioRJSON

cycleUpdate :: (String, String, String, Int) -> IO ResponseJSON
cycleUpdate args = getUpdates args Nothing >>= cycleUpdate' args

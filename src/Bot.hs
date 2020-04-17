{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Bot
    (
    cycleUpdate
    ) where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON (parseJSON), ToJSON, Value, Object, Array, (.:), withObject)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int32)
import Data.Text (Text, append, pack)
import GHC.Generics (Generic)
import Prelude hiding (id)
import Network.HTTP.Req


newtype RequestJSON = WithOffset {
    -- offset :: Int32,
    timeout :: Int
} deriving (Show, Generic)

instance ToJSON RequestJSON
instance FromJSON RequestJSON


newtype Update = Update {
    update_id :: Int32
}  deriving (Show, Generic)

instance ToJSON Update
instance FromJSON Update where
    parseJSON = withObject "Update" $ \v -> Update
        <$> v .: "update_id"


newtype ResponseJSON = RJSON {
    --ok :: Bool,
    result :: [Update]
} deriving (Show, Generic)

instance ToJSON ResponseJSON
instance FromJSON ResponseJSON where
    parseJSON = withObject "ResponseJSON" $ \v -> RJSON
        <$> v .: "result"

printUpdates :: (String, String, String, Int) -> IO ()
printUpdates (token, helpMsg, repeatMsg, echoRepeatNumber) = let {
    apiMethod = "getUpdates";
    tokenSection = append ("bot" :: Text) $ pack token;
    urlScheme = https "api.telegram.org" /: tokenSection /: apiMethod;
    body = (ReqBodyJson (WithOffset 20));
    runReqMonad = req POST urlScheme body jsonResponse mempty >>=
        (\ response -> liftIO $ print (responseBody response :: ResponseJSON));
} in runReq defaultHttpConfig runReqMonad
    

cycleUpdate :: (String, String, String, Int) -> IO ()
cycleUpdate args = printUpdates args >> cycleUpdate args

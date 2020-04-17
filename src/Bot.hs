{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Bot
    ( printUpdates,
    cycleUpdate
    ) where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON (parseJSON), ToJSON, Value, Object, Array, (.:), withObject)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, append, pack)
import GHC.Generics (Generic)
import Prelude hiding (id)
import Network.HTTP.Req


newtype RequestJSON = WithOffset {
    -- offset :: Double,
    timeout :: Int
} deriving (Show, Generic)

instance ToJSON RequestJSON
instance FromJSON RequestJSON


newtype Update = Update {
    update_id :: Double
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
    -- https://core.telegram.org/bots/api#available-methods
    apiMethod = "getUpdates";
    tokenSection = append ("bot" :: Text) $ pack token;
    --getResult = (HM.! "result");
-- examples section of http://hackage.haskell.org/package/req-3.1.0/docs/Network-HTTP-Req.html#v:req
} in runReq defaultHttpConfig $ do
    response <- req POST (https "api.telegram.org" /: tokenSection /: apiMethod) (ReqBodyJson (WithOffset 200)) jsonResponse mempty
    --liftIO . print $ getResult (responseBody response :: Object)
    liftIO $ print (responseBody response :: ResponseJSON)

cycleUpdate :: (String, String, String, Int) -> IO ()
cycleUpdate args = printUpdates args >> cycleUpdate args

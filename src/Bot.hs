{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Bot
    ( printUpdates,
    cycleUpdate
    ) where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON, Value, Object, Array, (.:))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, append, pack)
import GHC.Generics (Generic)
import Prelude hiding (id)
import Network.HTTP.Req


newtype RequestJSON = WithOffset {
    -- offset :: Double,
    tiemout :: Int
} deriving (Show, Generic)

instance ToJSON RequestJSON
instance FromJSON RequestJSON


printUpdates :: (String, String, String, Int) -> IO ()
printUpdates (token, helpMsg, repeatMsg, echoRepeatNumber) = let {
    -- https://core.telegram.org/bots/api#available-methods
    apiMethod = "getUpdates";
    tokenSection = append ("bot" :: Text) $ pack token;
    --getResult :: Object -> Array;
    getResult = (HM.! "result");
-- examples section of http://hackage.haskell.org/package/req-3.1.0/docs/Network-HTTP-Req.html#v:req
} in runReq defaultHttpConfig $ do
    response <- req POST (https "api.telegram.org" /: tokenSection /: apiMethod) (ReqBodyJson (WithOffset 200)) jsonResponse mempty
    liftIO . print $ getResult (responseBody response :: Object)

cycleUpdate :: (String, String, String, Int) -> IO ()
cycleUpdate args = printUpdates args >> cycleUpdate args

{-# LANGUAGE OverloadedStrings #-}

module Bot
    ( printUpdates
    ) where

import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Text (Text, append, pack)
import Network.HTTP.Req

-- https://core.telegram.org/bots/api#available-methods
apiMethod = "getUpdates"

printUpdates :: (String, String, String, Int) -> IO ()
printUpdates (token, helpMsg, repeatMsg, echoRepeatNumber) = let {
    tokenSection = append ("bot" :: Text) $ pack token;
-- examples section of http://hackage.haskell.org/package/req-3.1.0/docs/Network-HTTP-Req.html#v:req
} in runReq defaultHttpConfig $ do
    v <- req POST (https "api.telegram.org" /: tokenSection /: apiMethod) NoReqBody jsonResponse mempty
    liftIO $ print (responseBody v :: Value)

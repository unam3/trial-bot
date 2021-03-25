{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg
    (
    getInt,
    getLatestSupportedUpdateContent,
    processArgs,
    startBotWithLogger
    ) where

import Control.Monad (replicateM_, void)
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, fromJust)
import Data.Text (Text, append, pack)
import Data.Text.Read (decimal)
import Prelude hiding (id)
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, errorM, setLevel, traplogging, updateGlobalLogger)
import System.Exit (exitFailure, exitSuccess)

import Tg.Requests
import Tg.Requests.JSON
import Tg.Types


getUpdateId :: ResponseJSON -> Maybe Offset
getUpdateId rjson = let {
    updates = result rjson;
} in if null updates then Nothing else Just (update_id $ last updates)

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


isHelpCommand :: Text -> Bool
isHelpCommand = (== "/help");

getNumberOfRepeats :: Text -> Text
getNumberOfRepeats "repeat5" = "5"
getNumberOfRepeats "repeat4" = "4"
getNumberOfRepeats "repeat3" = "3"
getNumberOfRepeats "repeat2" = "2"
getNumberOfRepeats _ = "1"


processUpdates :: Config -> ResponseJSON -> IO Config
processUpdates config ioRJSON =
    let {
        latestSupportedUpdateContent = getLatestSupportedUpdateContent ioRJSON;
    } in case latestSupportedUpdateContent of
        Just (Left (chatID, msg, username, userID)) ->
            let {
                numberOfRepeats' = if isRepeatCommand msg || isHelpCommand msg
                    then 1
                    else getInt $ M.findWithDefault (repeatMessage config) userID (numberOfRepeatsMap config);
            } in replicateM_ numberOfRepeats' (respondToMessage config chatID msg username userID)
                >> return config
        -- https://core.telegram.org/bots/api#answercallbackquery
        Just (Right callbackQuery) ->
            void (answerCallbackQuery (tokenSection config) callbackQuery)
                >> let {
                    userID = (_id :: User -> UserID) $ _from callbackQuery;
                    newNumberOfRepeats = getNumberOfRepeats $ _data callbackQuery;
                    newNumberOfRepeatsMap = M.insert userID newNumberOfRepeats (numberOfRepeatsMap config);
                } in return config {numberOfRepeatsMap = newNumberOfRepeatsMap}
        _ -> return config
  

cycleEcho' :: Config -> Maybe ResponseJSON -> IO ResponseJSON
cycleEcho' config maybeRJSON =
    let {
        maybeOffset = maybe Nothing getUpdateId maybeRJSON;
    } in getUpdates (tokenSection config) maybeOffset
        >>= \ ioRJSON -> debugM "trial-bot.bot" (show ioRJSON)
            >> processUpdates config ioRJSON
                >>= \ config' -> cycleEcho' config' $ Just ioRJSON

cycleEcho :: Config -> IO ResponseJSON
cycleEcho config = let {
    noRJSON = Nothing;
} in updateGlobalLogger "trial-bot.bot" (setLevel DEBUG)
    >> cycleEcho' config noRJSON


processArgs :: [String] -> Either String Config
processArgs [token, helpMsg, repeatMsg, echoRepeatNumberStr] = let {
    echoRepeatNumber = (read echoRepeatNumberStr :: Int);
    isInRange n = n > 0 && n < 6;
} in if or [null token, null helpMsg, null repeatMsg, not $ isInRange echoRepeatNumber]
    then Left "Some argument passed from command line is wrong."
    else Right Config {
        tokenSection = append "bot" $ pack token,
        helpMessage = pack helpMsg,
        repeatMessage = pack repeatMsg,
        numberOfRepeats = pack echoRepeatNumberStr,
        numberOfRepeatsMap = M.empty
    }
processArgs _ = Left "Exactly four arguments needed: token, helpMsg, repeatMsg, echoRepeatNumber."

startBot :: [String] -> IO ()
startBot args =
    case processArgs args of
            Right args' -> void $ cycleEcho args' >> exitSuccess
            Left errorMessage -> errorM "trial-bot.bot" errorMessage
                >> exitFailure

startBotWithLogger :: [String] -> IO ()
startBotWithLogger args = traplogging
    "trial-bot.main"
    ERROR
    "Bot shutdown due to"
    $ startBot args

module Main where

import Bot (cycleEcho, Config)
import Data.Text (append, pack)
import System.Environment (getArgs)
import System.Log.Logger (traplogging, Priority (ERROR))


processArgs :: [String] -> Maybe Config
processArgs [token, helpMsg, repeatMsg, echoRepeatNumberStr] = let {
    echoRepeatNumber = (read echoRepeatNumberStr :: Int);
    isInRange n = n > 0 && n < 6;
} in if or [null token, null helpMsg, null repeatMsg, not $ isInRange echoRepeatNumber]
    then Nothing
    else Just (append (pack "bot") $ pack token, pack helpMsg, pack repeatMsg, pack echoRepeatNumberStr)
processArgs _ = Nothing

trialBot :: IO ()
trialBot = do
    args <- getArgs
    case args of
        [_, _, _, _] -> case processArgs args of
            Just args' -> cycleEcho args' >> return ()
            Nothing -> error "error: some argument passed from command line is wrong"
        _ -> error "error: exactly four arguments needed: token, helpMsg, repeatMsg, echoRepeatNumber"

main :: IO ()
main = traplogging "trial-bot.main" ERROR "Bot shutdown due to" trialBot

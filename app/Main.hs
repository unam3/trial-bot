module Main where

import System.Environment (getArgs)
import Bot (cycleEcho)


processArgs :: [String] -> Maybe (String, String, String, Int)
processArgs [token, helpMsg, repeatMsg, echoRepeatNumberStr] = let {
    echoRepeatNumber = (read echoRepeatNumberStr :: Int);
    isInRange n = n > 0 && n < 6;
} in if or [null token, null helpMsg, null repeatMsg, not $ isInRange echoRepeatNumber]
    then Nothing
    else Just (token, helpMsg, repeatMsg, echoRepeatNumber)
processArgs _ = Nothing

main :: IO ()
main = do
    args <- getArgs
    case args of
        [_, _, _, _] -> case processArgs args of
            Just args' -> cycleEcho args' >> return ()
            Nothing -> putStrLn "some argument passed from command line is wrong"
        _ -> putStrLn "error: exactly four arguments needed: token, helpMsg, repeatMsg, echoRepeatNumber"

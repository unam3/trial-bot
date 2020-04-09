module Main where

import System.Environment (getArgs)

processArgs :: [String] -> Maybe (String, String, String, Int)
processArgs [token, helpMsg, repeatMsg, echoRepeatNumber] = let {
    echoRepeatNumberInt = (read echoRepeatNumber :: Int);
    isInRange n = n > 0 && n < 6;
} in if or [null token, null helpMsg, null repeatMsg, not $ isInRange echoRepeatNumberInt]
    then Nothing
    else Just (token, helpMsg, repeatMsg, echoRepeatNumberInt)
processArgs _ = Nothing

main :: IO ()
main = do
    args <- getArgs
    case args of
        [_, _, _, _] -> putStrLn $ case processArgs args of
            Just args' -> show args'
            Nothing -> "some argument passed from command line is wrong"
        _ -> putStrLn "error: exactly four arguments needed: token, helpMsg, repeatMsg, echoRepeatNumber"

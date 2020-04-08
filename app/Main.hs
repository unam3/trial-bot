module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        args@[token, help_msg, repeat_msg, echo_repeat_number] -> putStrLn $ show args
        _ -> putStrLn "error: exactly four arguments needed: token, help_msg, repeat_msg, echo_repeat_number"


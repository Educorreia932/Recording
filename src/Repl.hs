module Repl where

import Control.Monad (forever)
import Implementation.Compilation
import Implementation.Evaluator
import System.Exit
import System.IO (hFlush, stdout)

executeCommand :: String -> String -> IO ()
executeCommand command expression = case command of
    x | x `elem` [":quit", ":q"] -> exitSuccess
    x | x `elem` [":eval", ":e"] -> print $ evaluate expression
    x | x `elem` [":help", ":h"] -> do
        putStrLn ":q[uit] - Exit the REPL"
        putStrLn ":e[val] - Evaluate an expression"
        putStrLn ":h[elp] - Display this help message"
    _ -> putStrLn "Invalid command."

repl :: IO ()
repl = forever $ do
    putStr "λ> "
    hFlush stdout
    input <- getLine
    let (command, expression) = span (/=' ') input
    executeCommand command expression

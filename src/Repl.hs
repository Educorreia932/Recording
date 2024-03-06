module Repl where

import Control.Monad (forever)
import Implementation.Evaluator (evaluate)
import System.IO (hFlush, stdout)
import System.Exit

repl :: IO ()
repl = forever $ do
    putStr "λ> "
    hFlush stdout
    input <- getLine
    case input of
        ":exit" -> exitSuccess
        _ -> print (evaluate input)

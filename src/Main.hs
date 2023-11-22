module Main where

import Control.Monad (forever)
import Evaluator (evaluate)
import Parser (parseExpression)
import System.IO (hFlush, stdout)

main :: IO ()
main = forever $ do
    putStr "λ> "
    hFlush stdout
    input <- getLine
    let result = parseExpression input
    case result of
        Left parseError -> print parseError
        Right expression -> print (evaluate expression)

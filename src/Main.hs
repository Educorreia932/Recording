module Main where

import Evaluator (evaluate)
import Control.Monad (forever)
import Parser (parse)
import Lexer (tokenize)
import System.IO ( hFlush, stdout )

main :: IO ()
main = forever $ do
    putStr "λ> "
    hFlush stdout
    input <- getLine
    let expression = parse $ tokenize input
        evaluation = case expression of
            Left parseError -> show parseError
            Right value -> show $ evaluate value
    putStrLn evaluation
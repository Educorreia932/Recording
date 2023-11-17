module Lexer where

import Common (Token (..))
import Data.Char

tokenize :: String -> [Token]
tokenize "" = []
tokenize (x : xs)
    | x == '(' = LeftParentheses : tokenize xs
    | x == ')' = RightParenteses : tokenize xs
    | x == 'λ' || x == '\\' = Lambda : tokenize xs
    | x == '.' = Dot : tokenize xs
    | x == ' ' = tokenize xs
    | x == '-' =
        let token = tokenizeNumber xs
         in case token of
                Number n -> Number (-n) : tokenize (dropWhile isDigit (tail xs))
                _ -> error "Invalid token"
    | x `elem` ['1' .. '9'] = tokenizeNumber (x : xs) : tokenize (dropWhile isDigit xs)
    | x `elem` ['a' .. 'z'] = tokenizeWord (x : xs) : tokenize (dropWhile isAlpha xs)
    | otherwise = error "Invalid token"

tokenizeNumber :: String -> Token
tokenizeNumber x = Number (read (takeWhile isDigit x) :: Int)

tokenizeWord :: String -> Token
tokenizeWord x = Word (takeWhile isAlpha x)

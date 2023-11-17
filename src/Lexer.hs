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
    | x `elem` ['1' .. '9'] || x == '-' = tokenizeNumber (x : xs) : tokenize (dropWhile isDigit xs)
    | x `elem` ['a' .. 'z'] = tokenizeWord (x : xs) : tokenize (dropWhile isAlpha xs)
    | otherwise = error "Invalid token"

tokenizeNumber :: String -> Token
tokenizeNumber x = Number (read (takeWhile isDigit x) :: Int)

tokenizeWord :: String -> Token
tokenizeWord x = Word (takeWhile isAlpha x)

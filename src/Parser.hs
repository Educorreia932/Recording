module Parser where

import Common (Expression (..), Token (..))

newtype ParseError = ParseError String deriving (Eq, Show)

parse :: [Token] -> Either ParseError Expression
parse [] = Left (ParseError "Empty input")
parse (x : xs) = case x of
    Number n -> Right (Literal n)
    Word v -> Right (Variable v)
    Lambda -> case xs of
        (Word v : Dot : rest) -> case parse rest of
            Right expression -> Right (Abstraction v expression)
            Left parseError -> Left parseError
        _ -> Left (ParseError "Invalid abstraction")
    _ -> Left (ParseError "Invalid token")
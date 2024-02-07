module Explicit.Parser (parseExpression) where

import Explicit.Terms
import Explicit.Types

import Data.Functor ((<&>))
import Text.Parsec (ParseError, parse, (<|>))
import Text.Parsec.Char (letter, spaces)
import Text.Parsec.Combinator (eof, many1)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser, makeTokenParser, reservedNames, reservedOpNames)
import Text.Parsec.Token qualified as Token

lexer :: TokenParser ()
lexer =
    makeTokenParser
        emptyDef
            { reservedOpNames = ["λ", "\\", ".", "->", ":"]
            , reservedNames = []
            }

lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

identifier :: Parser String
identifier = lexeme $ many1 letter

parentheses :: Parser a -> Parser a
parentheses = Token.parens lexer

integer :: Parser Integer
integer = lexeme $ Token.integer lexer

number :: Parser Expression
number = integer <&> (Literal . fromIntegral)

variable :: Parser Expression
variable = identifier <&> Variable <*> pure []

lambda :: Parser Expression
lambda = do
    _ <- Token.reservedOp lexer "λ" <|> Token.reservedOp lexer "\\"
    x <- identifier
    _ <- Token.reservedOp lexer ":"
    t <- typeAnnotation
    _ <- Token.reservedOp lexer "="
    Abstraction x t <$> term

dotExpression :: Parser Expression
dotExpression = do
    e <- identifier
    _ <- Token.reservedOp lexer "."
    field <- identifier
    _ <- Token.reservedOp lexer ":"
    t <- typeAnnotation
    return (Dot (Variable e []) t field)

typeParameter :: Parser Type
typeParameter = identifier <&> Parameter

arrowType :: Parser Type
arrowType = do
    t1 <- typeAnnotation
    _ <- Token.reservedOp lexer "->"
    Arrow t1 <$> typeAnnotation

typeAnnotation :: Parser Type
typeAnnotation =
    typeParameter

-- <|> arrowType

term :: Parser Expression
term =
    dotExpression
        <|> parentheses expression
        <|> number
        <|> variable
        <|> lambda

expression :: Parser Expression
expression = do
    terms <- many1 term
    return (foldl1 Application terms)

contents :: Parser a -> Parser a
contents parser = do
    Token.whiteSpace lexer
    r <- parser
    eof
    return r

parseExpression :: String -> Either ParseError Expression
parseExpression = parse (contents expression) "<stdin>"

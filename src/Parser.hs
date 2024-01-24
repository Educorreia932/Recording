module Parser (parseExpression) where

import Common (Expression (..))

import Data.Functor ((<&>))
import Text.Parsec (ParseError, parse, (<|>))
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (eof, many1)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser, makeTokenParser, reservedNames, reservedOpNames)
import Text.Parsec.Token qualified as Token

lexer :: TokenParser ()
lexer =
    makeTokenParser
        emptyDef
            { reservedOpNames = ["λ", "\\"]
            , reservedNames = []
            }

identifier :: Parser String
identifier = many1 letter

parentheses :: Parser a -> Parser a
parentheses = Token.parens lexer

integer :: Parser Integer
integer = Token.integer lexer

number :: Parser Expression
number = integer <&> (Literal . fromIntegral)

variable :: Parser Expression
variable = identifier <&> Variable

lambda :: Parser Expression
lambda = do
    _ <- Token.reservedOp lexer "λ" <|> Token.reservedOp lexer "\\"
    v <- identifier
    _ <- Token.reservedOp lexer "."
    Abstraction v <$> expression

term :: Parser Expression
term =
    parentheses expression
        <|> lambda
        <|> variable
        <|> number

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

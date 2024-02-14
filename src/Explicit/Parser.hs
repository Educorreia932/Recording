module Explicit.Parser (parseExpression) where

import Explicit.Terms
import qualified Explicit.Types as T

import Data.Functor ((<&>))
import Data.Map qualified as Map
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

dotExpression :: Parser Expression
dotExpression = do
    e <- identifier
    _ <- Token.reservedOp lexer "."
    field <- identifier
    _ <- Token.reservedOp lexer ":"
    t <- typeAnnotation
    return (Dot (Variable e []) t field)

recordKind :: Parser T.Kind
recordKind = do
    _ <- Token.reserved lexer "{{"
    k <- identifier
    _ <- Token.reserved lexer ":"
    t <- typeAnnotation
    _ <- Token.reserved lexer "}}"
    pure (T.RecordKind (Map.singleton k t))

universalKind :: Parser T.Kind
universalKind = Token.reserved lexer "U" >> pure T.Universal

kind :: Parser T.Kind
kind = recordKind <|> universalKind

stringType :: Parser T.Type
stringType = Token.reserved lexer "String" >> pure T.String

typeParameter :: Parser T.Type
typeParameter = T.Parameter <$> identifier

arrowType :: Parser T.Type
arrowType = do
    t1 <- typeAnnotation
    _ <- Token.reservedOp lexer "->"
    T.Arrow t1 <$> typeAnnotation

forAll :: Parser T.Type
forAll = do
    _ <- Token.reservedOp lexer "∀"
    v <- identifier
    _ <- Token.reservedOp lexer "::"
    k <- kind
    _ <- Token.reservedOp lexer "."
    T.ForAll v k <$> typeAnnotation

typeAnnotation :: Parser T.Type
typeAnnotation =
    forAll
        <|> parentheses arrowType
        <|> stringType
        <|> typeParameter

lambda :: Parser Expression
lambda = do
    _ <- Token.reservedOp lexer "λ" <|> Token.reservedOp lexer "\\"
    x <- identifier
    _ <- Token.reservedOp lexer ":"
    t <- typeAnnotation
    _ <- Token.reservedOp lexer "->"
    Abstraction x t <$> term

term :: Parser Expression
term =
    lambda
        <|> variable
        <|> number

application :: Parser Expression
application = do
    terms <- many1 term
    return (foldl1 Application terms)

contents :: Parser a -> Parser a
contents parser = do
    Token.whiteSpace lexer
    r <- parser
    eof
    return r

parseExpression :: String -> Either ParseError Expression
parseExpression = parse (contents term) "<stdin>"

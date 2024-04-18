module Implicit.Parser (parseExpression) where

import Implicit.Terms

import Data.Functor ((<&>))
import Data.Map qualified as Map
import Text.Parsec (alphaNum, many, parse, (<|>))
import Text.Parsec.Char (letter, spaces)
import Text.Parsec.Combinator (eof, many1, sepBy)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Token
import Text.ParserCombinators.Parsec (try)

lexer :: Token.TokenParser ()
lexer =
    Token.makeTokenParser
        emptyDef
            { Token.reservedOpNames = ["λ", "\\", ".", "->", "\\\\"]
            , Token.reservedNames = []
            }

lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

identifier :: Parser String
identifier = lexeme $ do
    first <- letter
    rest <- many $ alphaNum
    return (first : rest)

parentheses :: Parser a -> Parser a
parentheses = Token.parens lexer

integer :: Parser Integer
integer = lexeme $ Token.integer lexer

number :: Parser Expression
number = integer <&> (Literal . fromIntegral)

string :: Parser Expression
string = lexeme $ Token.stringLiteral lexer <&> String

variable :: Parser Expression
variable = identifier <&> Variable

record :: Parser Expression
record = do
    _ <- Token.reservedOp lexer "{"
    fields <- Map.fromList <$> (field `sepBy` Token.comma lexer)
    _ <- Token.reservedOp lexer "}"
    return (Record fields)
  where
    field = do
        k <- identifier
        _ <- Token.reservedOp lexer ":"
        v <- term
        return (k, v)

dotExpression :: Parser Expression
dotExpression = do
    _ <- Token.reservedOp lexer "("
    e <- term
    _ <- Token.reserved lexer ")"
    _ <- Token.reservedOp lexer "."
    Dot e <$> identifier

modify :: Parser Expression
modify = do
    _ <- Token.reservedOp lexer "modify("
    e1 <- term
    _ <- Token.reserved lexer ","
    l <- identifier
    _ <- Token.reserved lexer ","
    e2 <- term
    _ <- Token.reserved lexer ")"
    return $ Modify e1 l e2

letExpression :: Parser Expression
letExpression = do
    _ <- Token.reserved lexer "let"
    x <- identifier
    _ <- Token.reservedOp lexer "="
    e1 <- term
    _ <- Token.reserved lexer "in"
    Let x e1 <$> term

lambda :: Parser Expression
lambda = do
    _ <- Token.reservedOp lexer "λ" <|> Token.reservedOp lexer "\\"
    x <- identifier
    _ <- Token.reservedOp lexer "->"
    Abstraction x <$> term

term :: Parser Expression
term =
    lambda
        <|> letExpression
        <|> record
        <|> modify
        <|> try dotExpression
        <|> application
        <|> string
        <|> variable
        <|> number

application :: Parser Expression
application = do
    e1 <- parentheses term
    Application e1 <$> term

contents :: Parser a -> Parser a
contents parser = do
    Token.whiteSpace lexer
    r <- parser
    eof
    return r

parseExpression :: String -> Expression
parseExpression e = case parse (contents term) "<stdin>" e of
    Left err -> error (show err)
    Right expr -> expr

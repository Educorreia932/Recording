module Explicit.Parser (parseExpression) where

import Explicit.Terms
import Explicit.Types qualified as T

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
            { Token.reservedOpNames = ["λ", "\\", ".", "->", ":", "\\\\"]
            , Token.reservedNames = []
            }

lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

identifier :: Parser String
identifier = lexeme $ do
    first <- letter
    rest <- many alphaNum
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
variable = identifier <&> Variable <*> pure []

variableInstantiation :: Parser Expression
variableInstantiation = do
    _ <- Token.reservedOp lexer "("
    x <- identifier
    types <- many1 typeAnnotation
    _ <- Token.reservedOp lexer ")"
    pure (Variable x types)

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
    _ <- Token.reservedOp lexer ":"
    t <- typeAnnotation
    _ <- Token.reserved lexer ")"
    _ <- Token.reservedOp lexer "."
    Dot e t <$> identifier

modify :: Parser Expression
modify = do
    _ <- Token.reserved lexer "modify("
    e1 <- term
    _ <- Token.reservedOp lexer ":"
    t <- typeAnnotation
    _ <- Token.reservedOp lexer ","
    l <- identifier
    _ <- Token.reservedOp lexer ","
    e2 <- term
    _ <- Token.reservedOp lexer ")"
    return $ Modify e1 t l e2

contraction :: Parser Expression
contraction = do
    _ <- Token.reservedOp lexer "("
    e <- term
    _ <- Token.reservedOp lexer "\\\\"
    l <- identifier
    _ <- Token.reservedOp lexer ")"
    _ <- Token.reservedOp lexer ":"
    Contraction e l <$> typeAnnotation

extend :: Parser Expression
extend = do
    _ <- Token.reserved lexer "extend("
    e1 <- term
    _ <- Token.reservedOp lexer ":"
    t <- typeAnnotation
    _ <- Token.reservedOp lexer ","
    l <- identifier
    _ <- Token.reservedOp lexer ","
    e2 <- term
    _ <- Token.reservedOp lexer ")"
    return $ Extend e1 t l e2

recordKind :: Parser T.Kind
recordKind = do
    _ <- Token.reserved lexer "{{"
    fields <- Map.fromList <$> (field `sepBy` Token.comma lexer)
    _ <- Token.reserved lexer "}}"
    return (T.RecordKind fields)
  where
    field = do
        k <- identifier
        _ <- Token.reservedOp lexer ":"
        t <- typeAnnotation
        return (k, t)

universalKind :: Parser T.Kind
universalKind = Token.reserved lexer "U" >> pure T.Universal

kind :: Parser T.Kind
kind = recordKind <|> universalKind

stringType :: Parser T.Type
stringType = Token.reserved lexer "String" >> pure T.String

intType :: Parser T.Type
intType = Token.reserved lexer "Int" >> pure T.Int

typeParameter :: Parser T.Type
typeParameter = T.Parameter <$> identifier

recordType :: Parser T.Type
recordType = do
    _ <- Token.reservedOp lexer "{"
    fields <- Map.fromList <$> (field `sepBy` Token.comma lexer)
    _ <- Token.reservedOp lexer "}"
    return (T.Record fields)
  where
    field = do
        k <- identifier
        _ <- Token.reservedOp lexer ":"
        t <- typeAnnotation
        return (k, t)

arrowType :: Parser T.Type
arrowType = do
    t1 <- typeAnnotation
    _ <- Token.reservedOp lexer "->"
    T.Arrow t1 <$> typeAnnotation

forAll :: Parser T.Type
forAll = do
    _ <- Token.reservedOp lexer "∀"
    t <- identifier
    _ <- Token.reservedOp lexer "::"
    k <- kind
    _ <- Token.reservedOp lexer "."
    T.ForAll (t, k) <$> typeAnnotation

typeAnnotation :: Parser T.Type
typeAnnotation =
    forAll
        <|> parentheses arrowType
        <|> recordType
        <|> stringType
        <|> intType
        <|> typeParameter

poly :: Parser Expression
poly = do
    _ <- Token.reserved lexer "Poly"
    e <- parentheses term
    _ <- Token.reservedOp lexer ":"
    Poly e <$> typeAnnotation

letExpression :: Parser Expression
letExpression = do
    _ <- Token.reserved lexer "let"
    x <- identifier
    _ <- Token.reservedOp lexer ":"
    t <- typeAnnotation
    _ <- Token.reservedOp lexer "="
    e1 <- term
    _ <- Token.reserved lexer "in"
    Let x t e1 <$> term

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
        <|> try contraction
        <|> letExpression
        <|> poly
        <|> record
        <|> extend
        <|> modify
        <|> try dotExpression
        <|> try variableInstantiation
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

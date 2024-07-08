module Implicit.Parser (parseExpression) where

import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Void (Void)
import Implicit.Terms
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer =
    L.space
        space1
        (L.skipLineComment "//")
        (L.skipBlockComment "/*" "*/")

reservedWords :: [String]
reservedWords =
    [ "let"
    , "in"
    , "modify"
    , "extend"
    , "difference"
    , "union"
    , "λ"
    , "\\"
    , "\\\\"
    ]

reservedWord :: String -> Parser ()
reservedWord w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = some letterChar
    check x
        | x `elem` reservedWords = fail $ "Keyword " ++ show x ++ " cannot be an identifier"
        | otherwise = return x

parentheses :: Parser a -> Parser a
parentheses = between (symbol "(") (symbol ")")

curlyBraces :: Parser a -> Parser a
curlyBraces = between (symbol "{") (symbol "}")

integer :: Parser Integer
integer = do
    sign <- optional $ char '-'
    n <- lexeme L.decimal
    return $ case sign of
        Just _ -> -n
        Nothing -> n

number :: Parser Expression
number = integer <&> (Literal . fromIntegral)

text :: Parser Expression
text = lexeme $ char '"' >> manyTill L.charLiteral (char '"') <&> String

variable :: Parser Expression
variable = identifier <&> Variable

record :: Parser Expression
record = do
    fields <- curlyBraces $ Map.fromList <$> (field `sepBy` lexeme (char ','))
    return (Record fields)
  where
    field = do
        k <- identifier
        _ <- lexeme $ char '='
        v <- term
        return (k, v)

dotExpression :: Parser Expression
dotExpression = do
    e1 <-
        parentheses expression
            <|> variable
            <|> record
            <|> modify
            <|> contract
            <|> extend
            <|> difference
            <|> union
    _ <- lexeme $ char '.'
    Dot e1 <$> identifier

letExpression :: Parser Expression
letExpression = do
    _ <- reservedWord "let"
    x <- identifier
    _ <- lexeme $ char '='
    e1 <- expression
    _ <- reservedWord "in"
    Let x e1 <$> expression

modify :: Parser Expression
modify = do
    _ <- reservedWord "modify"
    parentheses $ do
        e1 <- term
        _ <- lexeme $ char ','
        l <- identifier
        _ <- lexeme $ char ','
        Modify e1 l <$> term

contract :: Parser Expression
contract = do
    e <-
        parentheses term
            <|> variable
            <|> record
            <|> modify
            <|> extend
            <|> difference
            <|> union
    _ <- reservedWord "\\\\"
    Contract e <$> identifier

extend :: Parser Expression
extend = do
    _ <- reservedWord "extend"
    parentheses $ do
        e1 <- term
        _ <- lexeme $ char ','
        l <- identifier
        _ <- lexeme $ char ','
        Extend e1 l <$> term

difference :: Parser Expression
difference = do
    _ <- reservedWord "difference"
    parentheses $ do
        e1 <- term
        _ <- lexeme $ char ','
        e2 <- record
        let Record e2' = e2
        return $ foldl (\acc (l, _) -> Contract acc l) e1 (Map.toList e2')

union :: Parser Expression
union = do
    _ <- reservedWord "union"
    parentheses $ do
        e1 <- term
        _ <- lexeme $ char ','
        e2 <- record
        let Record e2' = e2
        return $ foldl (\acc (l, v) -> Extend acc l v) e1 (Map.toList e2')

abstraction :: Parser Expression
abstraction = do
    _ <- char 'λ' <|> char '\\'
    vars <- lexeme $ some identifier
    _ <- reservedWord "->"
    e <- expression
    return $ foldr Abstraction e vars

application :: Parser Expression
application = do
    es <- some $ term <|> parentheses application
    return $ foldl1 Application es

term :: Parser Expression
term =
    spaceConsumer
        >> text
            <|> number
            <|> letExpression
            <|> try contract
            <|> abstraction
            <|> modify
            <|> extend
            <|> difference
            <|> union
            <|> try dotExpression
            <|> variable
            <|> record

expression :: Parser Expression
expression = application

parser :: Parser Expression
parser = expression <* eof

parseExpression :: String -> Expression
parseExpression e = case parse parser "" e of
    Left bundle -> error $ errorBundlePretty bundle
    Right xs -> xs

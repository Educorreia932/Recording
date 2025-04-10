module ParserTest where

import Data.Map qualified as Map
import Implicit.Parser
import Implicit.Terms
import System.Exit qualified as Exit
import Test.HUnit

parseExpression' :: String -> Expression
parseExpression' input = case parseExpression input of
    Left err -> error $ show err
    Right expr -> expr

testParser :: Test
testParser =
    TestList
        [ TestCase $
            assertEqual
                "Integer"
                (Literal 42)
                (parseExpression' "42")
        , TestCase $
            assertEqual
                "Negative Integer"
                (Literal (-42))
                (parseExpression' "-42")
        , TestCase $
            assertEqual
                "Record"
                (Record (Map.fromList [("Name", String "Joe"), ("Office", Literal 433)]))
                (parseExpression' "{ Name = \"Joe\", Office = 433 }")
        , TestCase $
            assertEqual
                "Field access"
                ( Dot
                    (Record (Map.fromList [("Name", String "Joe"), ("Office", Literal 433)]))
                    "Name"
                )
                (parseExpression' "{ Name = \"Joe\", Office = 433 }.Name")
        , TestCase $
            assertEqual
                "Application"
                ( Modify
                    (Record (Map.singleton "Name" (String "Joe")))
                    "Name"
                    (String "Hanako")
                )
                (parseExpression' "modify({ Name = \"Joe\"}, Name, \"Hanako\")")
        , TestCase $
            assertEqual
                "Application"
                ( Application (Variable "x") (Variable "y")
                )
                (parseExpression' "(x) y")
        , TestCase $
            assertEqual
                "Identity"
                ( Abstraction
                    "x"
                    (Variable "x")
                )
                (parseExpression' "\\x -> x")
        , TestCase $
            assertEqual
                "Let expression"
                ( Let
                    "x"
                    (Literal 42)
                    (Variable "x")
                )
                (parseExpression' "let x = 42 in x")
        ]

tests :: Test
tests =
    TestList
        [testParser]

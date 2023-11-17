module ParserTest where

import System.Exit qualified as Exit
import Test.HUnit

import Common (Expression (..))
import Evaluator (evaluate)
import Lexer (tokenize)
import Parser ( parse )

testParser :: Test
testParser =
  TestList
    [ TestCase
        $ assertEqual
          "Parse Literal"
          (Right (Literal 42))
          (parse $ tokenize "42"),
      TestCase
        $ assertEqual
          "Parse Abstraction"
          (Right (Abstraction "x" (Variable "x")))
          (parse $ tokenize "λx.x")
    ]

tests :: Test
tests =
  TestList
    [testParser]

module ParserTest where

import System.Exit qualified as Exit
import Test.HUnit

import Common (Expression (..))
import Evaluator (evaluate)
import Parser (parseExpression)

testParser :: Test
testParser =
  TestList
    [ TestCase
        $ assertEqual
          "Literal"
          (Right (Literal 42))
          (parseExpression "42")
    , TestCase
        $ assertEqual
          "Abstraction"
          (Right (Abstraction "x" (Variable "x")))
          (parseExpression "λx.x")
    , TestCase
        $ assertEqual
          "Application with literal"
          (Right (Application (Abstraction "x" (Variable "x")) (Literal 42)))
          (parseExpression "(λx.x) 42")
    , TestCase
        $ assertEqual
          "Application with variable"
          (Right (Application (Abstraction "x" (Variable "x")) (Variable "y")))
          (parseExpression "(λx.x) y")
    , TestCase
        $ assertEqual
          "Grouping"
          (Right (Application (Abstraction "x" (Variable "x")) (Application (Abstraction "y" (Variable "y")) (Literal 42))))
          (parseExpression "(λx.x) ((λy.y) 42)")
          -- TODO: Currying
    ]

tests :: Test
tests =
  TestList
    [testParser]

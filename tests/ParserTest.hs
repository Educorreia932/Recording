module ParserTest where

import System.Exit qualified as Exit
import Test.HUnit

import Terms (Expression (..))
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
          "Negative literal"
          (Right (Literal (-42)))
          (parseExpression "-42")
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
    , TestCase
        $ assertEqual
          "Function with multiple arguments"
          (Right (Application (Application (Abstraction "x" (Abstraction "y" (Variable "x"))) (Literal 1)) (Literal 2)))
          (parseExpression "(λx.λy.x) 1 2")
    , TestCase
        $ assertEqual
          "Currying"
          (Right (Application (Application (Abstraction "x" (Abstraction "y" (Variable "x"))) (Literal 1)) (Literal 2)))
          (parseExpression "(λx y.x) 1 2")
    ]

tests :: Test
tests =
  TestList
    [testParser]

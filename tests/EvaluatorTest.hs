module EvaluatorTest where

import System.Exit qualified as Exit
import Test.HUnit (Test (TestCase, TestList), assertEqual)

import Common (Expression (..))
import Evaluator (evaluate)
import Parser (parseExpression)

eval :: String -> Expression
eval string = case parseExpression string of
    Left parseError -> error "Error on parsing"
    Right expression -> evaluate expression

testEvaluate :: Test
testEvaluate =
    TestList
        [ TestCase
            $ assertEqual
                "Abstraction"
                (Abstraction "x" (Variable "x"))
                (eval "λx.x")
        , TestCase
            $ assertEqual
                "Application"
                (Literal 42)
                (eval "(λx.x) 42")
        , TestCase
            $ assertEqual
                "Nested application"
                (Literal 42)
                (eval "((λx.x) λy.y) 42")
        , TestCase
            $ assertEqual
                "Application with abstraction argument"
                (Abstraction "y" (Literal 42))
                (eval "(λx.x) λy.42")
        , TestCase
            $ assertEqual
                "Application with variable argument"
                (Variable "y")
                (eval "(λx.x) y")
        , TestCase
            $ assertEqual
                "Application with nested abstraction argument"
                (Abstraction "y" (Literal 42))
                (eval "(λx.x) λy.42")
        , TestCase
            $ assertEqual
                "Currying"
                (Literal 1)
                (eval "(λx.λy.x) 1 2")
        , TestCase
            $ assertEqual
                "Application with free variable"
                (Variable "y")
                (eval "(λx. (λx.x) y) 42")
        , TestCase
            $ assertEqual
                "α-conversion"
                (Abstraction "z" (Variable "y"))
                (eval "(λx.λy.x) (λz.y) 42")
        ]

tests :: Test
tests =
    TestList
        [testEvaluate]

module EvaluatorTest where

import System.Exit qualified as Exit
import Test.HUnit ( Test(TestList, TestCase), assertEqual )

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
                "Application"
                (Literal 42)
                (eval "(λx.x) 42")
        , TestCase
            $ assertEqual
                "Nested Application"
                (Literal 42)
                (eval "((λx.x) λy.y) 42")
        , TestCase
            $ assertEqual
                "Application with Abstraction Argument"
                (Abstraction "y" (Literal 42))
                (eval "(λx.x) λy.42")
        , TestCase
            $ assertEqual
                "Application with Variable Argument"
                (Variable "y")
                (eval "(λx.x) y")
        , TestCase
            $ assertEqual
                "Application with Nested Abstraction Argument"
                 (Abstraction "y" (Literal 42))
                (eval "(λx.x) λy.42")
        , TestCase
            $ assertEqual
                "Currying"
                (Literal 1)
                (eval "(λx.(λy.x)) (1 2)")
        , TestCase
            $ assertEqual
                "Application with Nested Abstraction Function"
                (Variable "y")
                (eval "(λx. (λx.x) y) 42")
        ]

tests :: Test
tests =
    TestList
        [testEvaluate]

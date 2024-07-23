module EvaluatorTest where

import Data.Map qualified as Map
import Explicit.TypeInference (typeInference)
import Implementation.Compilation
import Implementation.Evaluator
import Implementation.Terms
import Implicit.Parser
import System.Exit qualified as Exit
import Test.HUnit (Test (TestCase, TestList), assertEqual)

evaluate' :: String -> Expression
evaluate' input = case parseExpression input >>= typeInference >>= \(expr, _) -> compile expr >>= evaluate of
    Left err -> error $ show err
    Right expr -> expr

testEvaluate :: Test
testEvaluate =
    TestList
        [ TestCase $
            assertEqual
                "Application"
                (Literal 42)
                (evaluate' "(λx -> x) 42")
        , TestCase $
            assertEqual
                "Nested application"
                (Literal 42)
                (evaluate' "((λx -> x) λy -> y) 42")
        , TestCase $
            assertEqual
                "Application with abstraction argument"
                (Abstraction "y" (Literal 42))
                (evaluate' "(λx -> x) λy -> 42")
        , TestCase $
            assertEqual
                "Application with nested abstraction argument"
                (Abstraction "y" (Literal 42))
                (evaluate' "(λx -> x) λy -> 42")
        , TestCase $
            assertEqual
                "Currying"
                (Literal 1)
                (evaluate' "((λx λy -> x) 1) 2")
        , TestCase $
            assertEqual
                "Field access"
                (Literal 433)
                (evaluate' "{ Name = \"Joe\", Office = 433 }.Office")
        , TestCase $
            assertEqual
                "Modification"
                (Record [String "Hanako"])
                (evaluate' "modify({ Name = \"Joe\"}, Name, \"Hanako\")")
        , TestCase $
            assertEqual
                "Let expression"
                (Literal 443)
                (evaluate' "let office = 443 in office")
        , TestCase $
            assertEqual
                "Polymorphic extension"
                (Record [Literal 1])
                (evaluate' "let f = λx -> extend(x, B, 2) \\\\ B in f { A = 1 }")
        , TestCase $
            assertEqual
                "Polymorphic contraction"
                (Record [Literal 2])
                (evaluate' "let f = λx -> x \\\\ A in f { A = 1, B = 2 }")
        , TestCase $
            assertEqual
                "Polymorphic extension and contraction"
                (Record [Literal 3, Literal 2])
                (evaluate' "let f = λx -> extend(x, C, 2) \\\\ A in f { A = 1, B = 3 }")
        ]

tests :: Test
tests =
    TestList
        [testEvaluate]

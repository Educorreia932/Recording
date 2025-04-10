module EvaluationTest where

import Data.Map qualified as Map
import Explicit.TypeInference (typeInference)
import Implementation.Compilation
import Implementation.Evaluation
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
                (evaluate' "(\\x -> x) 42")
        , TestCase $
            assertEqual
                "Nested application"
                (Literal 42)
                (evaluate' "((\\x -> x) \\y -> y) 42")
        , TestCase $
            assertEqual
                "Application with abstraction argument"
                (Abstraction "y" (Literal 42))
                (evaluate' "(\\x -> x) \\y -> 42")
        , TestCase $
            assertEqual
                "Application with nested abstraction argument"
                (Abstraction "y" (Literal 42))
                (evaluate' "(\\x -> x) \\y -> 42")
        , TestCase $
            assertEqual
                "Currying"
                (Literal 1)
                (evaluate' "((\\x y -> x) 1) 2")
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
                (evaluate' "let f = \\x -> extend(x, B, 2) \\\\ B in f { A = 1 }")
        , TestCase $
            assertEqual
                "Polymorphic contraction"
                (Record [Literal 2])
                (evaluate' "let f = \\x -> x \\\\ A in f { A = 1, B = 2 }")
        , TestCase $
            assertEqual
                "Polymorphic extension and contraction"
                (Record [Literal 3, Literal 2])
                (evaluate' "let f = \\x -> extend(x, C, 2) \\\\ A in f { A = 1, B = 3 }")
        , TestCase $
            assertEqual
                "Function composition (1)"
                (Record [Literal 2])
                (evaluate' "let a = \\x -> x \\\\ A in let b = \\x -> extend(x, B, 2) in let f = \\x -> a (b x) in f { A = 1 }")
        , TestCase $
            assertEqual
                "Function composition (2)"
                (Literal 2)
                (evaluate' "let a = \\x -> x \\\\ A in let b = \\x -> extend(x, B, 2) in let c = \\x -> x.B in let f = \\x -> c (a (b x)) in f { A = 1 }")
        ]

tests :: Test
tests =
    TestList
        [testEvaluate]

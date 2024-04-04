module EvaluatorTest where

import System.Exit qualified as Exit
import Test.HUnit (Test (TestCase, TestList), assertEqual)

import Data.Map.Ordered qualified as OMap
import Explicit.Parser
import Implementation.Evaluator
import Implementation.Terms

testEvaluate :: Test
testEvaluate =
    TestList
        [ TestCase
            $ assertEqual
                "Application"
                (Literal 42)
                (evaluate "(λx : Int -> x) 42")
        , TestCase
            $ assertEqual
                "Nested application"
                (Literal 42)
                (evaluate "((λx : Int -> x) λy : Int -> y) 42")
        , TestCase
            $ assertEqual
                "Application with abstraction argument"
                (Abstraction "y" (Literal 42))
                (evaluate "(λx : Int -> x) λy : Int -> 42")
        , TestCase
            $ assertEqual
                "Application with variable argument"
                (Variable "y")
                (evaluate "(λx : Int -> x) y")
        , TestCase
            $ assertEqual
                "Application with nested abstraction argument"
                (Abstraction "y" (Literal 42))
                (evaluate "(λx : (Int -> Int) -> x) λy : Int -> 42")
        , TestCase
            $ assertEqual
                "Currying"
                (Literal 1)
                (evaluate "((λx : Int -> λy : Int -> x) 1) 2")
        , TestCase
            $ assertEqual
                "Application with free variable"
                (Variable "y")
                (evaluate "(λx : Int -> (λx : Int -> x) y) 42")
        , TestCase
            $ assertEqual
                "α-conversion"
                (Abstraction "z" (Variable "y"))
                (evaluate "((λx : (Int -> Int) -> λy : Int -> x) λz : Int -> y) 42")
        , TestCase
            $ assertEqual
                "Field access"
                (Literal 433)
                (evaluate "({ Name: \"Joe\", Office: 433 } : { Name: String, Office: Int }).Office")
        , TestCase
            $ assertEqual
                "Modification"
                (Record (OMap.singleton ("Name", String "Hanako")))
                (evaluate "modify({ Name: \"Joe\"} : { Name: String }, Name, \"Hanako\")")
        , TestCase
            $ assertEqual
                "Let expression"
                (Literal 443)
                (evaluate "let office: Int = 443 in office")
        , TestCase
            $ assertEqual
                "Polymorphic field access"
                (String "Joe")
                ( evaluate
                    ( let t = "∀t1::U.∀t2::{{ Name: String }}.(t1 -> t2)"
                       in "let name: "
                            ++ t
                            ++ "= Poly(λx: t2 -> (x : t2).Name): "
                            ++ t
                            ++ "in ((name String { Name: String, Office: Int })) { Name: \"Joe\", Office: 443 }"
                    )
                )
        , TestCase
            $ assertEqual
                "Nested record"
                (Literal 2)
                ( evaluate
                    ( let t = "∀t1::{{ C: Int }}.∀t2::{{ B: t1 }}.(t2 -> Int)"
                       in "let f: "
                            ++ t
                            ++ "= Poly(λx: t2 -> ((x : t2).B : t1).C  ): "
                            ++ t
                            ++ "in ((f { C: Int } { A: Int, B: { C: Int } })) { A: 1, B: { C: 2 } }"
                    )
                )
        ]

tests :: Test
tests =
    TestList
        [testEvaluate]

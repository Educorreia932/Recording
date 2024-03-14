module EvaluatorTest where

import System.Exit qualified as Exit
import Test.HUnit (Test (TestCase, TestList), assertEqual)

import Data.Map qualified as Map
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
                (Record [String "Hanako"])
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
                    ( let t = "∀t1::U.∀t2::{{ Name: String || }}.(t1 -> t2)"
                       in "let name: "
                            ++ t
                            ++ "= Poly(λx: t2 -> (x: t2).Name): "
                            ++ t
                            ++ "in ((name String { Name: String, Office: Int })) { Name: \"Joe\", Office: 443 }"
                    )
                )
        , TestCase
            $ assertEqual
                "Nested record"
                (Literal 2)
                ( evaluate
                    ( let t = "∀t1::{{ C: Int || }}.∀t2::{{ B: t1 || }}.(t2 -> Int)"
                       in "let f: "
                            ++ t
                            ++ "= Poly(λx: t2 -> ((x : t2).B : t1).C  ): "
                            ++ t
                            ++ "in ((f { C: Int } { A: Int, B: { C: Int } })) { A: 1, B: { C: 2 } }"
                    )
                )
        , TestCase
            $ assertEqual
                "Contraction"
                (Record [Literal 443])
                (evaluate "({ Name: \"Joe\", Office: 443 } : { Name: String, Office: Int } \\\\ Name)")
        , TestCase
            $ assertEqual
                "Extension"
                (Record [String "Joe", Literal 443])
                (evaluate "extend({ Name: \"Joe\"} : { Name: String }, Office, 443)")
        , TestCase
            $ assertEqual
                "Polymorphic extension"
                (Record [String "Joe", Literal 443])
                ( evaluate
                    ( let t = "∀t1::{{ || Office: Int }}.(t1 -> (t1 + { Office: Int }) )"
                       in "let addOffice: "
                            ++ t
                            ++ "= λx: t2 -> extend( x: t1, Office, 443 ) "
                            ++ "in (addOffice) { Name: \"Joe\" }"
                    )
                )
        , TestCase
            $ assertEqual
                "Polymorphic extension"
                (Record [Literal 2])
                ( evaluate
                    ( let t = "∀t1::{{ A: Int || }}.(t1 -> (t1 - { A: Int }) )"
                       in "let f: "
                            ++ t
                            ++ "= Poly(λx: t1 -> ( x: t1 \\\\ A )): "
                            ++ t
                            ++ "in ((f { A: Int, B: Int })) { A: 1, B: 2 }"
                    )
                )
        , TestCase
            $ assertEqual
                "Polymorphic extension and contraction"
                (Record [Literal 2])
                ( evaluate
                    ( let t = "∀t1::{{ A: Int || B: Int }}.(t1 -> ((t1 + { B: Int }) - { A: Int }) )"
                       in "let f: "
                            ++ t
                            ++ "= Poly(λx: t1 -> ( extend(x: t1, B, 2) : t1 \\\\ A )): "
                            ++ t
                            ++ "in ((f { A: Int })) { A: 1 }"
                    )
                )
        ]

tests :: Test
tests =
    TestList
        [testEvaluate]

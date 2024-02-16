module EvaluatorTest where

import System.Exit qualified as Exit
import Test.HUnit (Test (TestCase, TestList), assertEqual)

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
                (String "Joe")
                (evaluate "({ Name: \"Joe\" } : { Name: String }).Name ")
        ]

tests :: Test
tests =
    TestList
        [testEvaluate]

module EvaluatorTest where

import System.Exit qualified as Exit
import Test.HUnit

import Common (Expression (..))
import Evaluator (evaluate)

testEvaluate :: Test
testEvaluate =
  TestList
    [ TestCase
        $ assertEqual
          "Evaluate Application"
          (Literal 42)
          (evaluate (Application (Abstraction "x" (Variable "x")) (Literal 42)))
    , TestCase
        $ assertEqual
          "Evaluate Nested Application"
          (Literal 42)
          (evaluate (Application (Application (Abstraction "x" (Abstraction "y" (Variable "x"))) (Literal 42)) (Literal 1)))
    , TestCase
        $ assertEqual
          "Evaluate Application with Abstraction Argument"
          (Abstraction "y" (Literal 42))
          (evaluate (Application (Abstraction "x" (Variable "x")) (Abstraction "y" (Literal 42))))
    , TestCase
        $ assertEqual
          "Evaluate Application with Variable Argument"
          (Variable "y")
          (evaluate (Application (Abstraction "x" (Variable "x")) (Variable "y")))
    , TestCase
        $ assertEqual
          "Evaluate Application with Nested Abstraction Argument"
          (Abstraction "y" (Literal 42))
          (evaluate (Application (Abstraction "x" (Abstraction "y" (Variable "x"))) (Literal 42)))
    , TestCase
        $ assertEqual
          "Evaluate Application with Nested Abstraction Function"
          (Abstraction "z" (Literal 1))
          (evaluate (Application (Application (Abstraction "x" (Abstraction "y" (Variable "x"))) (Abstraction "z" (Literal 1))) (Literal 42)))
    , TestCase
        $ assertEqual
          "Evaluate Application with Nested Abstraction Function"
          (Application (Variable "y") (Abstraction "x" (Variable "x")))
          (evaluate (Application (Abstraction "x" (Application (Variable "y") (Abstraction "x" (Variable "x")))) (Literal 42)))
    ]

tests :: Test
tests =
  TestList
    [testEvaluate]

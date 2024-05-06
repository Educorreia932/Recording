module Main where

import Test.HUnit (Test (TestList), runTestTT)

import CompilationTest qualified
import EvaluatorTest qualified
import ParserTest qualified
import TypeInferenceTest qualified

main =
  runTestTT
    ( TestList
        [ ParserTest.tests
        , TypeInferenceTest.tests
        , CompilationTest.tests
        , EvaluatorTest.tests
        ]
    )
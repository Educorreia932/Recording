module Main where

import Test.HUnit (Test (TestList), runTestTT)

import CompilationTest qualified
import EvaluatorTest qualified
import ImplicitParserTest qualified
import ParserTest qualified
import TypeInferenceTest qualified

main =
  runTestTT
    ( TestList
        [ EvaluatorTest.tests
        , CompilationTest.tests
        , ParserTest.tests
        , ImplicitParserTest.tests
        , TypeInferenceTest.tests
        ]
    )
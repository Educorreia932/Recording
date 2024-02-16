module Main where

import Test.HUnit (Test (TestList), runTestTT)

import CompilationTest qualified
import EvaluatorTest qualified
import ParserTest qualified

main =
  runTestTT
    ( TestList
        [ EvaluatorTest.tests
        , CompilationTest.tests
        , ParserTest.tests
        ]
    )
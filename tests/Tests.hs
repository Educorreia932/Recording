module Main where

import CompilationTest qualified
import EvaluationTest qualified
import ParserTest qualified
import Test.HUnit (Test (TestList), runTestTT)
import TypeInferenceTest qualified
import Test.HUnit.Base
import System.Exit (exitFailure, exitSuccess)

tests =
    TestList
        [ ParserTest.tests
        , TypeInferenceTest.tests
        , CompilationTest.tests
        , EvaluationTest.tests
        ]

main = do
    counts <- runTestTT tests
    if errors counts + failures counts > 0
        then exitFailure
        else exitSuccess

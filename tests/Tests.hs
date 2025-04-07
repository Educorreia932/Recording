module Main where

import CompilationTest qualified
import EvaluationTest qualified
import ParserTest qualified
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Test (TestList), runTestTT)
import Test.HUnit.Base
import TypeInferenceTest qualified
import UnificationTest qualified

tests =
    TestList
        [ ParserTest.tests
        , UnificationTest.tests
        , TypeInferenceTest.tests
        , CompilationTest.tests
        , EvaluationTest.tests
        ]

main = do
    counts <- runTestTT tests
    if errors counts + failures counts > 0
        then exitFailure
        else exitSuccess

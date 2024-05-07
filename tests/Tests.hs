module Main where

import CompilationTest qualified
import EvaluatorTest qualified
import ParserTest qualified
import Test.HUnit (Test (TestList), runTestTT)
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

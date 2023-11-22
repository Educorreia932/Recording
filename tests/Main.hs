module Main where

import Test.HUnit (Test (TestList), runTestTT)

import EvaluatorTest qualified
import ParserTest qualified

main =
    runTestTT
        ( TestList
            [ EvaluatorTest.tests
            , ParserTest.tests
            ]
        )
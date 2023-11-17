module Main where

import Test.HUnit (Test (TestList), runTestTT)

import EvaluatorTest qualified
import LexerTest qualified

main =
    runTestTT
        ( TestList
            [ EvaluatorTest.tests
            , LexerTest.tests
            ]
        )
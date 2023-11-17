module Main where

import Test.HUnit (Test (TestList), runTestTT)

import EvaluatorTest qualified
import LexerTest qualified
import ParserTest qualified

main =
    runTestTT
        ( TestList
            [ EvaluatorTest.tests
            , LexerTest.tests
            , ParserTest.tests
            ]
        )
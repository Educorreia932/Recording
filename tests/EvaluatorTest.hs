module Main where

import System.Exit qualified as Exit
import Test.HUnit

import Common (Expression (..))
import Evaluator (evaluate)

test1 :: Test
test1 = TestCase (assertEqual "Application of Literal to Abstraction" expected (evaluate expression))
  where
    expression = Application (Abstraction "x" (Variable "x")) (Literal 1)
    expected = Literal 1

test2 :: Test
test2 = TestCase (assertEqual "Application of Abstraction to Literal" expected (evaluate expression))
  where
    expression = Application (Literal 1) (Abstraction "x" (Variable "x"))
    expected = Application (Literal 1) (Abstraction "x" (Variable "x"))

test3 :: Test
test3 = TestCase (assertEqual "Application of Abstraction to Abstraction" expected (evaluate expression))
  where
    expression = Application (Abstraction "x" (Variable "x")) (Abstraction "y" (Variable "y"))
    expected = Abstraction "y" (Variable "y")

test4 :: Test
test4 = TestCase (assertEqual "Application of Variable to Abstraction" expected (evaluate expression) )
  where
    expression = Application (Abstraction "x" (Variable "x")) (Variable "x")
    expected = Variable "x"

test5 :: Test
test5 = TestCase (assertEqual "Free variables" expected (evaluate expression))
  where
    expression = Application (Abstraction "x" (Application (Variable "y") (Variable "x"))) (Variable "z")
    expected = Application (Variable "y") (Variable "z")

tests :: Test
tests =
    TestList
        [ TestLabel "test1" test1
        , TestLabel "test2" test2
        , TestLabel "test3" test3
        , TestLabel "test4" test4
        , TestLabel "test5" test5
        ]

main :: IO Counts
main = runTestTT tests
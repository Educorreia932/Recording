module LexerTest where

import System.Exit qualified as Exit
import Test.HUnit

import Common (Expression (..), Token (..))
import Evaluator (evaluate)
import Lexer (tokenize)

testLexer :: Test
testLexer =
  TestList  
    [ TestCase
        $ assertEqual
          "Tokenize Literal"
          [Number 42]
          (tokenize "42")
    , TestCase
        $ assertEqual
          "Tokenize Abstraction"
          [Lambda, Word "x", Dot, Word "x"]
          (tokenize "λx.x")
    , TestCase
        $ assertEqual
          "Tokenize Application"
          [LeftParentheses, Lambda, Word "x", Dot, Word "x", Word "y", RightParenteses]
          (tokenize "(λx.x y)")
    ]

tests :: Test
tests =
  TestList
    [testLexer]
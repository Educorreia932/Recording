module ParserTest where

import System.Exit qualified as Exit
import Test.HUnit

import Data.Map qualified as Map
import Explicit.Parser
import Explicit.Terms
import Explicit.Types qualified as T

testParser :: Test
testParser =
  TestList
    [ TestCase
        $ assertEqual
          "Integer"
          (Right (Literal 42))
          (parseExpression "42")
    , TestCase
        $ assertEqual
          "Negative Integer"
          (Right (Literal (-42)))
          (parseExpression "-42")
    , TestCase
        $ assertEqual
          "Identity (Universal kind)"
          ( Right
              ( Abstraction
                  "x"
                  (T.ForAll "t" T.Universal (T.Parameter "t" `T.Arrow` T.Parameter "t"))
                  (Variable "x" [])
              )
          )
          (parseExpression "λx : ∀t::U.(t -> t) -> x")
    , TestCase
        $ assertEqual
          "Identity (Record kind)"
          ( Right
              ( Abstraction
                  "x"
                  ( T.ForAll
                      "t"
                      (T.RecordKind (Map.singleton "Name" T.String))
                      (T.Parameter "t" `T.Arrow` T.Parameter "t")
                  )
                  (Variable "x" [])
              )
          )
          (parseExpression "λx : ∀t::{{ Name: String }}.(t -> t) -> x")
    ]

tests :: Test
tests =
  TestList
    [testParser]

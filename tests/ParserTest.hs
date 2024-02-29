module ParserTest where

import System.Exit qualified as Exit
import Test.HUnit

import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Explicit.Parser
import Explicit.Terms
import Explicit.Types qualified as T

testParser :: Test
testParser =
    TestList
        [ TestCase
            $ assertEqual
                "Integer"
                (Literal 42)
                (parseExpression "42")
        , TestCase
            $ assertEqual
                "Negative Integer"
                (Literal (-42))
                (parseExpression "-42")
        , TestCase
            $ assertEqual
                "Record"
                (ERecord (OMap.fromList [("Name", String "Joe"), ("Office", Literal 433)]))
                (parseExpression "{ Name: \"Joe\", Office: 433 }")
        , TestCase
            $ assertEqual
                "Field access"
                ( Dot
                    (ERecord (OMap.fromList [("Name", String "Joe"), ("Office", Literal 433)]))
                    (T.Record (Map.fromList [("Name", T.String), ("Office", T.Int)]))
                    "Name"
                )
                (parseExpression "({ Name: \"Joe\", Office: 433 } : { Name: String, Office: Int }).Name")
        , TestCase
            $ assertEqual
                "Application"
                ( Modify
                    (ERecord (OMap.singleton ("Name", String "Joe")))
                    (T.Record (Map.singleton "Name" T.String))
                    "Name"
                    (String "Hanako")
                )
                (parseExpression "modify({ Name: \"Joe\"} : { Name: String }, Name, \"Hanako\")")
        , TestCase
            $ assertEqual
                "Contraction"
                ( Contraction
                    (ERecord (OMap.singleton ("Name", String "Joe")))
                    "Name"
                    T.String
                )
                (parseExpression "({ Name: \"Joe\" } \\\\ Name) : String")
        , TestCase 
            $ assertEqual
                "Extension"
                ( Extend
                    (ERecord (OMap.singleton ("Name", String "Joe")))
                    (T.Record (Map.singleton "Name" T.String))
                    "Office"
                    (Literal 433)
                )
                (parseExpression "extend({ Name: \"Joe\"} : { Name: String }, Office, 433)")
        , TestCase
            $ assertEqual
                "Application"
                ( Application (Variable "x" []) (Variable "y" [])
                )
                (parseExpression "(x) y")
        , TestCase
            $ assertEqual
                "Identity (Universal kind)"
                ( Abstraction
                    "x"
                    (T.ForAll ("t", T.Universal) (T.Parameter "t" `T.Arrow` T.Parameter "t"))
                    (Variable "x" [])
                )
                (parseExpression "λx : ∀t::U.(t -> t) -> x")
        , TestCase
            $ assertEqual
                "Identity (Record kind)"
                ( Abstraction
                    "x"
                    ( T.ForAll
                        ("t", (T.RecordKind (Map.singleton "Name" T.String)))
                        (T.Parameter "t" `T.Arrow` T.Parameter "t")
                    )
                    (Variable "x" [])
                )
                (parseExpression "λx : ∀t::{{ Name: String }}.(t -> t) -> x")
        , TestCase
            $ assertEqual
                "Polymorphic field access"
                ( Poly
                    ( Abstraction
                        "x"
                        (T.Parameter "t2")
                        ( Dot
                            (Variable "x" [])
                            (T.Parameter "t2")
                            "Name"
                        )
                    )
                    ( T.ForAll
                        ("t1", T.Universal)
                        ( T.ForAll
                            ("t2", T.RecordKind (Map.singleton "Name" T.String))
                            (T.Parameter "t1" `T.Arrow` T.Parameter "t2")
                        )
                    )
                )
                (parseExpression "Poly(λx : t2 -> (x : t2).Name): ∀t1::U.∀t2::{{ Name: String }}.(t1 -> t2)")
        , TestCase
            $ assertEqual
                "Let expression"
                ( Let
                    "x"
                    T.String
                    (Literal 42)
                    (Variable "x" [])
                )
                (parseExpression "let x : String = 42 in x")
        ]

tests :: Test
tests =
    TestList
        [testParser]

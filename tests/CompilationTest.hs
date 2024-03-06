module CompilationTest where

import Data.Map qualified as Map
import Explicit.Parser
import Explicit.Terms qualified as E
import Explicit.Types qualified as T
import Implementation.Compilation
import Implementation.Terms qualified as I
import Test.HUnit

testCompilation :: Test
testCompilation =
    TestList
        [ TestCase
            $ assertEqual
                "Field access"
                ( I.IndexExpression
                    (I.Record [I.String "Joe", I.Literal 433])
                    (Left 2)
                )
                (compile "({ Office: 433, Name: \"Joe\" } : { Office: Int, Name: String }).Office")
        , TestCase
            $ assertEqual
                "Polymorphic field access"
                ( I.IndexAbstraction
                    "I1"
                    (I.Abstraction "x" (I.IndexExpression (I.Variable "x") (Right "I1")))
                )
                (compile "Poly(λx: t2 -> (x : t2).Name): ∀t1::U.∀t2::{{ Name: String || }}.(t2 -> t1)")
        , TestCase
            $ assertEqual
                "Let expression"
                ( I.Let
                    "name"
                    ( I.IndexAbstraction
                        "I1"
                        ( I.Abstraction "x" (I.IndexExpression (I.Variable "x") (Right "I1"))
                        )
                    )
                    ( I.Application
                        (I.IndexApplication (I.Variable "name") (Left 1))
                        (I.Record [I.String "Joe", I.Literal 443])
                    )
                )
                ( compile
                    ( let t = "∀t1::U.∀t2::{{ Name: String || }}.(t1 -> t2)"
                       in "let name: "
                            ++ t
                            ++ "= Poly(λx: t2 -> (x : t2).Name): "
                            ++ t
                            ++ "in ((name String { Name: String, Office: Int })) { Name: \"Joe\", Office: 443 }"
                    )
                )
        , TestCase
            $ assertEqual
                "Poly application"
                ( I.Application
                    (I.IndexAbstraction "I1" (I.Abstraction "x" (I.IndexExpression (I.Variable "x") (Right "I1"))))
                    (I.IndexAbstraction "I2" (I.Abstraction "y" (I.IndexExpression (I.Variable "y") (Right "I2"))))
                )
                ( compile
                    ( let e1 = "Poly(λx: Int -> (x : t1).a): ∀t1::{{ a: Int || }}.(t1 -> Int)"
                          e2 = "Poly(λy: Int -> (y : t2).b): ∀t2::{{ b: Int || }}.(t2 -> Int)"
                       in "(" ++ e1 ++ ")" ++ e2
                    )
                )
        , TestCase
            $ assertEqual
                "Contraction"
                ( I.Contraction
                    (I.Record [I.String "Joe", I.Literal 443])
                    (Left 1)
                )
                (compile "({ Name: \"Joe\", Office: 443 } : { Name: String, Office: Int } \\\\ Name)")
        , TestCase
            $ assertEqual
                "Extend"
                ( I.Extend
                    (I.Record [I.String "Joe"])
                    (I.Literal 443)
                )
                (compile "extend({ Name: \"Joe\"} : { Name: String }, Office, 443)")
        ]

tests :: Test
tests =
    TestList
        [testCompilation]

module CompilationTest where

import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
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
                    (I.Record (OMap.fromList [("Name", I.String "Joe"), ("Office", I.Literal 433)]))
                    (Left 2)
                )
                (compile "({ Name: \"Joe\", Office: 433 } : { Name: String, Office: Int }).Office")
        , TestCase
            $ assertEqual
                "Polymorphic field access"
                ( I.IndexAbstraction
                    "I1"
                    (I.Abstraction "x" (I.IndexExpression (I.Variable "x") (Right "I1")))
                )
                (compile "Poly(λx: t2 -> (x : t2).Name): ∀t1::U.∀t2::{{ Name: String }}.(t2 -> t1)")
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
                        (I.Record (OMap.fromList [("Name", I.String "Joe"), ("Office", I.Literal 443)]))
                    )
                )
                ( compile
                    ( let t = "∀t1::U.∀t2::{{ Name: String }}.(t1 -> t2)"
                       in "let name: "
                            ++ t
                            ++ "= Poly(λx: t2 -> (x : t2).Name): "
                            ++ t
                            ++ "in ((name String { Name: String, Office: Int })) { Name: \"Joe\", Office: 443 }"
                    )
                )
        ]

tests :: Test
tests =
    TestList
        [testCompilation]

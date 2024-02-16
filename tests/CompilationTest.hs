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
                    (I.Record (OMap.singleton ("Name", I.String "Joe")))
                    (Left 1)
                )
                (compile "({ Name: \"Joe\" } : { Name: String }).Name ")
        , TestCase
            $ assertEqual
                "Polymorphic field access"
                ( I.IndexAbstraction
                    (Right "I")
                    (I.Abstraction "x" (I.IndexExpression (I.Variable "x") (Right "I")))
                )
                (compile "Poly(λx: t2 -> (x : t2).Name): ∀t1::U.∀t2::{{ Name: String }}.(t1 -> t2)")
        , TestCase
            $ assertEqual
                "Let expression"
                ( I.Let
                    "name"
                    ( I.IndexAbstraction
                        (Right "I")
                        ( I.Abstraction "x" (I.IndexExpression (I.Variable "x") (Right "I"))
                        )
                    )
                    ( I.Application
                        (I.IndexApplication (I.Variable "name") 1)
                        (I.Record (OMap.singleton ("Name", I.String "Joe")))
                    )
                )
                ( compile
                    ( let t = "∀t1::U.∀t2::{{ Name: String }}.(t1 -> t2)"
                       in "let name: "
                            ++ t
                            ++ "= Poly(λx: t2 -> (x : t1).Name) :"
                            ++ t
                            ++ "in ((name String { Name: String })) { Name: \"Joe\" }"
                    )
                )
        ]

tests :: Test
tests =
    TestList
        [testCompilation]

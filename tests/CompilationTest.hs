module CompilationTest where

import Data.Map qualified as Map
import Explicit.Terms qualified as E
import Explicit.Types qualified as T
import Implementation.Compilation
import Implementation.Terms qualified as I
import Implicit.Parser
import Implicit.TypeInference (typeInference)
import Test.HUnit

compile' :: String -> I.Expression
compile' = compile . fst . typeInference

testCompilation :: Test
testCompilation =
    TestList
        [ TestCase $
            assertEqual
                "Field access"
                ( I.IndexExpression
                    (I.Record [I.String "Joe", I.Literal 433])
                    (Left 2)
                )
                (compile' "{ Office = 433, Name = \"Joe\" } . Office")
        , TestCase $
            assertEqual
                "Polymorphic field access"
                ( I.Let
                    "name"
                    ( I.IndexAbstraction
                        "I1"
                        ( I.Abstraction "x" (I.IndexExpression (I.Variable "x") (Right ("I1", 0)))
                        )
                    )
                    ( I.Application
                        (I.IndexApplication (I.Variable "name") (Left 1))
                        (I.Record [I.String "Joe", I.Literal 443])
                    )
                )
                (compile' "let name = λx-> x . Name in name { Name = \"Joe\", Office = 443 }")
        , TestCase $
            assertEqual
                "Contraction"
                ( I.Contraction
                    (I.Record [I.String "Joe", I.Literal 443])
                    (Left 1)
                )
                (compile' "{ Name = \"Joe\", Office = 443 } \\\\ Name")
        , TestCase $
            assertEqual
                "Extend"
                ( I.Extend
                    (I.Record [I.String "Joe"])
                    (Left 2)
                    (I.Literal 443)
                )
                (compile' "extend({ Name = \"Joe\"}, Office, 443)")
        ]

tests :: Test
tests =
    TestList
        [testCompilation]

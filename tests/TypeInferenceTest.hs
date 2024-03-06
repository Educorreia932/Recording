module TypeInferenceTest where

import Test.HUnit

import Explicit.Terms
import Explicit.Types qualified as T
import Implicit.TypeInference

testEvaluate :: Test
testEvaluate =
    TestList
        [ TestCase
            $ assertEqual
                "Literal"
                (Literal 42)
                (infer "42")
        , TestCase
            $ assertEqual
                "Identity"
                ( Abstraction
                    "x"
                    ( T.ForAll
                        ("t", T.Universal)
                        (T.Parameter "t" `T.Arrow` T.Parameter "t")
                    )
                    (Variable "x" [])
                )
                (infer "λx -> x")
        ]

tests :: Test
tests =
    TestList
        [testEvaluate]
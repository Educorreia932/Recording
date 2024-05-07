module TypeInferenceTest where

import Test.HUnit

import Data.Map qualified as Map

import Explicit.Terms
import Explicit.Types qualified as T
import Implicit.TypeInference
import Implicit.Types (Scheme (..))

testEvaluate :: Test
testEvaluate =
    TestList
        [ TestCase $
            assertEqual
                "Constant"
                (Literal 42, T.Int)
                (typeInference "42")
        , TestCase $
            assertEqual
                "Abstraction"
                ( Abstraction "x" (T.Parameter "_s1") (Variable "x" [])
                , T.Parameter "_s1" `T.Arrow` T.Parameter "_s1"
                )
                (typeInference "λx -> x")
        , TestCase $
            assertEqual
                "Application"
                ( Application
                    ( Abstraction
                        "x"
                        T.Int
                        ( Record (Map.singleton "A" (Variable "x" []))
                        )
                    )
                    (Literal 1)
                , T.Record (Map.singleton "A" T.Int)
                )
                (typeInference "(λx -> { A = x }) 1")
        , TestCase $
            assertEqual
                "Let expression"
                ( let t = T.ForAll ("_s2", T.Universal) (T.Parameter "_s2" `T.Arrow` T.Parameter "_s2")
                   in Let
                        "id"
                        t
                        (Poly (Abstraction "x" (T.Parameter "_s2") (Variable "x" [])) t)
                        (Application (Variable "id" [T.Int]) (Literal 42))
                , T.Int
                )
                (typeInference "let id = λx -> x in id 42")
        , TestCase $
            assertEqual
                "Record"
                ( Record $
                    Map.fromList
                        [ ("A", Literal 1)
                        , ("B", Literal 2)
                        ]
                , T.Record $
                    Map.fromList
                        [ ("A", T.Int)
                        , ("B", T.Int)
                        ]
                )
                (typeInference "{ A = 1, B = 2 }")
        , TestCase $
            assertEqual
                "Modify"
                ( let t =
                        T.Record
                            ( Map.fromList
                                [ ("A", T.Int)
                                , ("B", T.Int)
                                ]
                            )
                   in ( Modify
                            ( Record $
                                Map.fromList
                                    [ ("A", Literal 1)
                                    , ("B", Literal 2)
                                    ]
                            )
                            t
                            "A"
                            (Literal 3)
                      , t
                      )
                )
                (typeInference "modify({ A = 1, B = 2 }, A, 3)")
        ]

tests :: Test
tests =
    TestList
        [testEvaluate]
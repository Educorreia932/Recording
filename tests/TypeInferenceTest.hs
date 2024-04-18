module TypeInferenceTest where

import Test.HUnit

import Data.Map qualified as Map

import Data.Map.Ordered qualified as OMap
import Explicit.Terms
import Explicit.Types qualified as T
import Implicit.TypeInference

testEvaluate :: Test
testEvaluate =
    TestList
        [ TestCase $
            assertEqual
                "Constant"
                (Literal 42, T.Int)
                (infer "42")
        , TestCase $
            assertEqual
                "Variable"
                ( Variable "x" [T.Parameter "_s1", T.Parameter "_s2"]
                , T.ForAll
                    ("_s1", T.Universal)
                    ( T.ForAll
                        ( "_s2"
                        , T.RecordKind
                            ( Map.singleton
                                "A"
                                T.Int
                            )
                        )
                        (T.Parameter "_s1" `T.Arrow` T.Parameter "_s2")
                    )
                )
                ( inferWithState
                    "x"
                    ( Map.empty
                    , Map.singleton
                        "x"
                        ( Scheme
                            ["_s1", "_s2"]
                            ( T.ForAll
                                ("_s1", T.Universal)
                                ( T.ForAll
                                    ( "_s2"
                                    , T.RecordKind
                                        ( Map.singleton
                                            "A"
                                            T.Int
                                        )
                                    )
                                    (T.Parameter "_s1" `T.Arrow` T.Parameter "_s2")
                                )
                            )
                        )
                    , 0
                    )
                )
        , TestCase $
            assertEqual
                "Abstraction"
                ( Abstraction "x" (T.Parameter "_s1") (Variable "x" [])
                , T.Parameter "_s1" `T.Arrow` T.Parameter "_s1"
                )
                (infer "λx -> x")
        , TestCase $
            assertEqual
                "Application"
                ( Application
                    ( Abstraction
                        "x"
                        T.Int
                        ( ERecord (OMap.singleton ("A", Variable "x" []))
                        )
                    )
                    (Literal 1)
                , T.Record (Map.singleton "A" T.Int)
                )
                (infer "(λx -> { A: x }) 1")
        , TestCase $
            assertEqual
                "Let expression"
                ( let t = T.ForAll ("_s1", T.Universal) (T.Parameter "_s1" `T.Arrow` T.Parameter "_s1")
                   in Let
                        "id"
                        t
                        (Poly (Abstraction "x" (T.Parameter "_s1") (Variable "x" [])) t)
                        (Application (Variable "id" []) (Literal 42))
                , T.Int
                )
                (infer "let id = λx -> x in (id) 42")
        , TestCase $
            assertEqual
                "Record"
                ( ERecord $
                    OMap.fromList
                        [ ("A", Literal 1)
                        , ("B", Literal 2)
                        ]
                , T.Record $
                    Map.fromList
                        [ ("A", T.Int)
                        , ("B", T.Int)
                        ]
                )
                (infer "{ A: 1, B: 2 }")
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
                            ( ERecord $
                                OMap.fromList
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
                (infer "modify({ A: 1, B: 2 }, A, 3)")
        ]

tests :: Test
tests =
    TestList
        [testEvaluate]
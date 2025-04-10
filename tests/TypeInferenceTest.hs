module TypeInferenceTest where

import Data.Map qualified as Map
import Explicit.Terms
import Explicit.TypeInference
import Explicit.Types qualified as T
import Implicit.Parser (parseExpression)
import Test.HUnit

typeInference' :: String -> (Expression, T.Type)
typeInference' input = case parseExpression input >>= typeInference of
    Left err -> error $ show err
    Right res -> res

testEvaluate :: Test
testEvaluate =
    TestList
        [ TestCase $
            assertEqual
                "Constant"
                (Literal 42, T.Int)
                (typeInference' "42")
        , TestCase $
            assertEqual
                "Abstraction"
                ( Abstraction "x" (T.Parameter "t0") (Variable "x" [])
                , T.Parameter "t0" `T.Arrow` T.Parameter "t0"
                )
                (typeInference' "\\x -> x")
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
                (typeInference' "(\\x -> { A = x }) 1")
        , TestCase $
            assertEqual
                "Let expression"
                ( let t = T.ForAll ("t1", T.Universal) (T.Parameter "t1" `T.Arrow` T.Parameter "t1")
                   in Let
                        "id"
                        t
                        (Poly (Abstraction "x" (T.Parameter "t1") (Variable "x" [])) t)
                        (Application (Variable "id" [T.Int]) (Literal 42))
                , T.Int
                )
                (typeInference' "let id = \\x -> x in id 42")
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
                (typeInference' "{ A = 1, B = 2 }")
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
                (typeInference' "modify({ A = 1, B = 2 }, A, 3)")
        ]

tests :: Test
tests =
    TestList
        [testEvaluate]

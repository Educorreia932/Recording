module UnificationTest where

import Control.Monad
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State
import Data.Map qualified as Map
import Explicit.Types qualified as T
import Explicit.Typing
import Explicit.Unification
import Implicit.Parser
import Implicit.Terms
import Test.HUnit

unify' :: KindAssignment -> [TypePair] -> (KindAssignment, Substitution)
unify' k us = do
    let initialState = (Map.empty, Map.empty)
    let result = evalState (runExceptT (unify k us)) 0
    case result of
        Left err -> error $ show err
        Right (k, s) -> (k, s)

testUnification :: Test
testUnification =
    TestList
        [ TestCase $
            assertEqual
                "Unify two identical types"
                (Map.empty, Map.empty)
                (unify' Map.empty [(T.Int, T.Int)])
        , TestCase $
            assertEqual
                "Unify parameter and final type"
                ( Map.empty
                , Map.fromList [("x", T.Int)]
                )
                (unify' (Map.fromList [("x", T.Universal)]) [(T.Parameter "x", T.Int)])
        , TestCase $
            assertEqual
                "Unify two parameters (w/ universal kind)"
                ( Map.fromList [("y", T.Universal)]
                , Map.fromList [("x", T.Parameter "y")]
                )
                ( unify'
                    ( Map.fromList
                        [ ("x", T.Universal)
                        , ("y", T.Universal)
                        ]
                    )
                    [(T.Parameter "x", T.Parameter "y")]
                )
        , TestCase $
            assertEqual
                "Unify two parameters (w/ record kind)"
                ( Map.fromList [("y", T.RecordKind (Map.fromList [("A", T.Int)]) Map.empty)]
                , Map.fromList
                    [ ("x", T.Parameter "y")
                    , ("z", T.Int)
                    ]
                )
                ( unify'
                    ( Map.fromList
                        [ ("x", T.RecordKind (Map.fromList [("A", T.Int)]) Map.empty)
                        , ("y", T.RecordKind (Map.fromList [("A", T.Parameter "z")]) Map.empty)
                        , ("z", T.Universal)
                        ]
                    )
                    [(T.Parameter "x", T.Parameter "y")]
                )
        , TestCase $
            assertEqual
                "Unify record and parameter"
                ( Map.empty
                , Map.fromList
                    [ ("x", T.Record (Map.fromList [("A", T.Int)]))
                    , ("y", T.Int)
                    ]
                )
                ( unify'
                    ( Map.fromList
                        [ ("x", T.RecordKind (Map.fromList [("A", T.Parameter "y")]) Map.empty)
                        , ("y", T.Universal)
                        ]
                    )
                    [(T.Parameter "x", T.Record (Map.fromList [("A", T.Int)]))]
                )
        , TestCase $
            assertEqual
                "Unify two records"
                ( Map.empty
                , Map.fromList [("x", T.Int)]
                )
                ( unify'
                    (Map.fromList [("x", T.Universal)])
                    [
                        ( T.Record (Map.fromList [("A", T.Parameter "x")])
                        , T.Record (Map.fromList [("A", T.Int)])
                        )
                    ]
                )
        , TestCase $
            assertEqual
                "Unify two arrow types"
                ( Map.empty
                , Map.fromList [("x", T.Int), ("y", T.Int)]
                )
                ( unify'
                    ( Map.fromList
                        [ ("x", T.Universal)
                        , ("y", T.Universal)
                        ]
                    )
                    [
                        ( T.Parameter "x" `T.Arrow` T.Int
                        , T.Int `T.Arrow` T.Parameter "y"
                        )
                    ]
                )
        , TestCase $
            assertEqual
                "Unify paramater and extensible type"
                ( Map.fromList [("y", T.RecordKind (Map.fromList [("A", T.Int), ("C", T.Int)]) Map.empty)]
                , Map.fromList
                    [
                        ( "x"
                        , T.Extension
                            (T.Parameter "y")
                            "B"
                            T.Int
                        )
                    ]
                )
                ( unify'
                    ( Map.fromList
                        [ ("x", T.RecordKind (Map.fromList [("A", T.Int)]) Map.empty)
                        , ("y", T.RecordKind (Map.fromList [("C", T.Int)]) Map.empty)
                        ]
                    )
                    [
                        ( T.Parameter "x"
                        , T.Extension
                            (T.Parameter "y")
                            "B"
                            T.Int
                        )
                    ]
                )
        , TestCase $
            assertEqual
                "Unify two extensibles types (common extension/contraction)"
                ( Map.fromList [("y", T.RecordKind (Map.fromList [("A", T.Int), ("C", T.Int)]) Map.empty)]
                , Map.fromList
                    [
                        ( "x"
                        , T.Parameter "y"
                        )
                    ]
                )
                ( unify'
                    ( Map.fromList
                        [ ("x", T.RecordKind (Map.fromList [("A", T.Int)]) Map.empty)
                        , ("y", T.RecordKind (Map.fromList [("C", T.Int)]) Map.empty)
                        ]
                    )
                    [
                        ( T.Extension
                            (T.Parameter "x")
                            "B"
                            T.Int
                        , T.Extension
                            (T.Parameter "y")
                            "B"
                            T.Int
                        )
                    ]
                )
        , TestCase $
            assertEqual
                "Unify two extensibles types (no common extensions/contractions)"
                ( Map.fromList [("t0", T.RecordKind (Map.fromList [("A", T.Int), ("C", T.Int)]) Map.empty)]
                , Map.fromList
                    [
                        ( "x"
                        , T.Extension
                            (T.Parameter "t0")
                            "B"
                            T.Int
                        )
                    ,
                        ( "y"
                        , T.Extension
                            (T.Parameter "t0")
                            "D"
                            T.Int
                        )
                    ]
                )
                ( unify'
                    ( Map.fromList
                        [ ("x", T.RecordKind (Map.fromList [("A", T.Int)]) Map.empty)
                        , ("y", T.RecordKind (Map.fromList [("C", T.Int)]) Map.empty)
                        ]
                    )
                    [
                        ( T.Extension
                            (T.Parameter "x")
                            "D"
                            T.Int
                        , T.Extension
                            (T.Parameter "y")
                            "B"
                            T.Int
                        )
                    ]
                )
        ]

tests :: Test
tests =
    TestList
        [testUnification]

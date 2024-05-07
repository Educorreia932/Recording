module Implicit.TypeInference (typeInference) where

import Control.Monad (when)
import Control.Monad.State
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Traversable
import Explicit.Terms qualified as E
import Explicit.Types qualified as T
import Implicit.Parser
import Implicit.Terms
import Implicit.Types
import Implicit.Unification

{- | Create a type scheme out of a type.

__Examples:__

@
generalize {} s1 == ([s1], s1)
generalize {x: ([], s1)} (s1 -> s2) == ([s2], s1 -> s2)
@
-}
generalize :: KindAssignment -> TypeAssignment -> T.Type -> (KindAssignment, Scheme)
generalize kindAssign typeAssign t = (kindAssign', Scheme params t')
  where
    params = Set.toList (eftv t Set.\\ eftv typeAssign)
    t' = foldl (\acc v -> T.ForAll (v, kindAssign Map.! v) acc) t (Set.toList $ ftv t)
    kindAssign' = Map.filterWithKey (\k _ -> k `notElem` params) kindAssign

    eftv :: (Types a) => a -> Set.Set String
    eftv x = foldl (\acc x' -> acc `Set.union` ftv (kindAssign Map.! x')) (ftv x) (Set.toList $ ftv t)

{- | Replaces all bound type variables in a type scheme with fresh type variables.

__Examples:__

@
instantiate ([t1], t1) == (s1, {})
instantiate ([t1, t2], t1 -> t2) == (s1 -> s2, {(t1, s1), (t2, s2)})
@
-}
instantiate :: Scheme -> TI (T.Type, Substitution)
instantiate (Scheme params t) = do
    freshTypes <- mapM (const freshType) params
    let substitution = Map.fromList $ zip params freshTypes
        instantiated = apply substitution t
    return (instantiated, substitution)

infer :: TypeEnv -> Expression -> TI (KindAssignment, Substitution, E.Expression, T.Type)
infer (k, t) = infer'
  where
    -- Constants
    infer' (Literal a) = return (k, nullSubstitution, E.Literal a, T.Int)
    infer' (String a) = return (k, nullSubstitution, E.String a, T.String)
    -- Variable
    infer' (Variable x) =
        case Map.lookup x t of
            Nothing -> error $ "Unbound variable: " ++ x
            Just (Scheme _ tau') ->
                do
                    let tauKindedTypes = T.typeParameters tau'
                        tau = T.concreteType tau'
                    freshTypes <- mapM (const freshType) tauKindedTypes
                    let substitution = Map.fromList $ zip (map fst tauKindedTypes) freshTypes
                        k' = k `Map.union` apply substitution (Map.fromList tauKindedTypes)
                    return
                        ( k'
                        , nullSubstitution
                        , E.Variable x freshTypes
                        , T.rewrite $ apply substitution tau
                        )

    -- Abstraction
    infer' (Abstraction x m1) =
        do
            alpha <- freshType
            let (T.Parameter alpha') = alpha
            (k1, s1, m1', tau) <-
                infer
                    ( k `Map.union` Map.singleton alpha' T.Universal
                    , t `Map.union` Map.singleton x (Scheme [] alpha)
                    )
                    m1
            return
                ( k1
                , s1
                , E.Abstraction x (apply s1 alpha) m1'
                , T.rewrite (apply s1 alpha) `T.Arrow` tau
                )

    -- Application
    infer' (Application m1 m2) =
        do
            (k1, s1, m1', tau1) <- infer' m1
            (k2, s2, m2', tau2) <- infer (k1, apply s1 t) m2
            alpha <- freshType
            let (T.Parameter alpha') = alpha
            (k3, s3) <-
                unify
                    (k2 `Map.union` Map.singleton alpha' T.Universal)
                    [(apply s2 tau1, tau2 `T.Arrow` alpha)]
            return
                ( k3
                , s1 `composeSubs` s2 `composeSubs` s3
                , E.Application
                    (apply (s2 `composeSubs` s3) m1')
                    (apply s3 m2')
                , T.rewrite $ apply s3 alpha
                )

    -- Record
    infer' (Record r) =
        do
            let (_, m1) = head (Map.toList r)
            -- Inference for first field
            (k1, s1, m1', tau1) <- infer' m1
            -- Inference for remaining fields
            (_, fieldsInference) <-
                mapAccumM
                    ( \(k', s') (_, mi) ->
                        do
                            (ki, si, mi', taui) <- infer (k', apply s' t) mi
                            return
                                ( (ki, s' `composeSubs` si)
                                , (ki, si, mi', taui)
                                )
                    )
                    (k1, nullSubstitution)
                    (tail $ Map.toList r)
            return
                ( last (k1 : map (\(ki, _, _, _) -> ki) fieldsInference)
                , foldr (composeSubs . (\(_, si, _, _) -> si)) s1 fieldsInference
                , E.Record $
                    Map.fromList $
                        zip
                            (Map.keys r)
                            (m1' : map (\(_, _, mi, _) -> mi) fieldsInference)
                , T.Record $
                    Map.fromList $
                        zip
                            (Map.keys r)
                            (tau1 : map (\(_, _, _, taui) -> T.rewrite taui) fieldsInference)
                )

    -- Dot
    infer' (Dot m1 l) =
        do
            (k1, s1, m1', tau1) <- infer' m1
            alpha1 <- freshType
            alpha2 <- freshType
            let (T.Parameter alpha1') = alpha1
                (T.Parameter alpha2') = alpha2
            (k2, s2) <-
                unify
                    ( k1
                        `Map.union` Map.fromList
                            [ (alpha1', T.Universal)
                            , (alpha2', T.RecordKind (Map.singleton l alpha1) Map.empty)
                            ]
                    )
                    [(alpha2, tau1)]
            return
                ( k2
                , s1 `composeSubs` s2
                , E.Dot (apply s2 m1') (apply s2 alpha2) l
                , apply s2 alpha1
                )

    -- Modify
    infer' (Modify m1 l m2) =
        do
            (k1, s1, m1', tau1) <- infer' m1
            (k2, s2, m2', tau2) <- infer (k1, apply s1 t) m2
            alpha1 <- freshType
            alpha2 <- freshType
            let (T.Parameter alpha1') = alpha1
                (T.Parameter alpha2') = alpha2
            (k3, s3) <-
                unify
                    ( k2
                        `Map.union` Map.fromList
                            [ (alpha1', T.Universal)
                            , (alpha2', T.RecordKind (Map.singleton l alpha1) Map.empty)
                            ]
                    )
                    [ (alpha1, tau2)
                    , (alpha2, apply s2 tau1)
                    ]
            return
                ( k3
                , s1 `composeSubs` s2 `composeSubs` s3
                , E.Modify
                    (apply (s2 `composeSubs` s3) m1')
                    (apply s3 alpha2)
                    l
                    (apply s3 m2')
                , T.rewrite $ apply s3 alpha2
                )

    -- Let expression
    infer' (Let x m1 m2) =
        do
            (k1, s1, m1', tau1) <- infer' m1
            let (k1', sigma) = generalize k1 (apply s1 t) tau1
            (sigma', s3) <- instantiate sigma
            (k2, s2, m2', tau2) <-
                infer
                    ( k1
                    , apply (s1 `composeSubs` s3) t `Map.union` Map.singleton x sigma
                    )
                    m2
            return
                ( k2
                , s1 `composeSubs` s2
                , E.Let
                    x
                    (apply s2 sigma')
                    (E.Poly (apply (s2 `composeSubs` s3) m1') (apply s2 sigma'))
                    m2'
                , tau2
                )

    -- Contract
    infer' (Contract m1 l) = do
        (k1, s1, m1', tau1) <- infer' m1
        alpha1 <- freshType
        alpha2 <- freshType
        let (T.Parameter alpha1') = alpha1
            (T.Parameter alpha2') = alpha2
        (k2, s2) <-
            unify
                ( k1
                    `Map.union` Map.fromList
                        [ (alpha1', T.Universal)
                        , (alpha2', T.RecordKind (Map.singleton l alpha1) Map.empty)
                        ]
                )
                [(alpha2, tau1)]
        return
            ( k2
            , s1 `composeSubs` s2
            , E.Contract
                (apply s2 m1')
                (apply s2 alpha2)
                l
            , T.rewrite $ apply s2 (T.Contraction alpha2 l alpha1)
            )

    -- Extend
    infer' (Extend m1 l m2) = do
        (k1, s1, m1', tau1) <- infer' m1
        (k2, s2, m2', tau2) <- infer (k1, apply s1 t) m2
        let chi = T.root tau1
        case chi of
            T.Parameter chi' -> when (chi' `Set.member` ftv tau2) $ error "Type inference failed"
            _ -> return ()
        alpha1 <- freshType
        alpha2 <- freshType
        let (T.Parameter alpha1') = alpha1
            (T.Parameter alpha2') = alpha2
        (k3, s3) <-
            unify
                ( k2
                    `Map.union` Map.fromList
                        [ (alpha1', T.Universal)
                        , (alpha2', T.RecordKind Map.empty (Map.singleton l alpha1))
                        ]
                )
                [ (alpha1, tau2)
                , (alpha2, apply s2 tau1)
                ]
        return
            ( k3
            , s1 `composeSubs` s2 `composeSubs` s3
            , E.Extend
                (apply (s1 `composeSubs` s2 `composeSubs` s3) m1')
                (apply (s1 `composeSubs` s2 `composeSubs` s3) alpha2)
                l
                (apply s3 m2')
            , T.rewrite $ apply s3 (T.Extension alpha2 l alpha1)
            )

typeInference :: String -> (E.Expression, T.Type)
typeInference s = (m, tau)
  where
    (_, _, m, tau) = evalState (infer (Map.empty, Map.empty) (parseExpression s)) 0

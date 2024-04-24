module Implicit.Unification where

import Data.Bifunctor qualified
import Data.Foldable (asum)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Explicit.Types qualified as T

import Debug.Trace
import Implicit.Types

type UnificationState = (Set.Set TypePair, KindAssignment, Substitution)
type TypePair = (T.Type, T.Type)

unifyStep' :: UnificationState -> (TypePair, (String, T.Kind)) -> Maybe UnificationState
unifyStep' (u, k, s) (ui, ki) =
    case (ui, ki) of
        -- (I)
        ((t1, t2), _)
            | t1 == t2 ->
                let u' = Set.delete ui u
                 in Just
                        ( u'
                        , k
                        , s
                        )
        -- (II)
        ((t, T.Parameter x), (x', T.Universal))
            | x == x' && not (x `Set.member` ftv t) ->
                unifyStep' (u, k, s) ((T.Parameter x, t), (x', T.Universal))
        ((T.Parameter x, t), (x', T.Universal))
            | x == x' && not (x `Set.member` ftv t) ->
                let s' = Map.singleton x t
                    u' = Set.delete ui u
                    k' = Map.delete x k
                 in Just
                        ( apply s' u'
                        , apply s' k'
                        , s `composeSubs` s'
                        )
        -- (III)
        ((T.Parameter x1, T.Parameter x2), (x1', T.RecordKind f1l f1r))
            | x1 == x1' ->
                case Map.lookup x2 k of
                    Just (T.RecordKind f2l f2r)
                        | (Map.keysSet f1l `Set.disjoint` Map.keysSet f2r)
                            && (Map.keysSet f1r `Set.disjoint` Map.keysSet f2l) ->
                            let s' = Map.singleton x1 (T.Parameter x2)
                                u' = Set.delete ui u
                                k' = Map.delete x1 k
                                ul = Set.fromList [(f1l Map.! l, f2l Map.! l) | l <- Map.keys f1l, l `Map.member` f2l]
                                ur = Set.fromList [(f1r Map.! l, f2r Map.! l) | l <- Map.keys f1r, l `Map.member` f2r]
                                ks = Map.singleton x2 $ apply s' (T.RecordKind (f1l `Map.union` f2l) (f1r `Map.union` f2r))
                             in Just
                                    ( apply s' $ u' `Set.union` ul `Set.union` ur
                                    , apply s' k' `Map.union` apply s' ks
                                    , s `composeSubs` s'
                                    )
                    _ -> Nothing
        -- (IV)
        ((T.Record f2, T.Parameter x), (x', T.RecordKind f1l f1r))
            | x == x'
                && (Map.keysSet f1l `Set.isSubsetOf` Map.keysSet f2)
                && (Map.keysSet f1r `Set.disjoint` Map.keysSet f2)
                && not (x `Set.member` ftv (T.Record f2)) ->
                unifyStep' (u, k, s) ((T.Parameter x, T.Record f2), (x', T.RecordKind f1l f1r))
        ((T.Parameter x, T.Record f2), (x', T.RecordKind f1l f1r))
            | x == x'
                && (Map.keysSet f1l `Set.isSubsetOf` Map.keysSet f2)
                && (Map.keysSet f1r `Set.disjoint` Map.keysSet f2)
                && not (x `Set.member` ftv (T.Record f2)) ->
                let s' = Map.singleton x (T.Record f2)
                    u' = Set.delete ui u
                    k' = Map.delete x k
                    us = Set.fromList [(f1l Map.! l, f2 Map.! l) | l <- Map.keys f1l]
                 in Just
                        ( apply s' $ u' `Set.union` us
                        , apply s' k'
                        , s `composeSubs` s'
                        )
        -- (V)
        ((T.Record f1, T.Record f2), _)
            | Map.keysSet f1 == Map.keysSet f2 ->
                let u' = Set.delete ui u
                 in Just
                        ( u' `Set.union` Set.fromList [(f1 Map.! l, f2 Map.! l) | l <- Map.keys f1]
                        , k
                        , s
                        )
        -- (VI)
        ((t1 `T.Arrow` t2, t1' `T.Arrow` t2'), _) ->
            let u' = Set.delete ui u
             in Just
                    ( u' `Set.union` Set.fromList [(t1, t1'), (t2, t2')]
                    , k
                    , s
                    )
        _ -> Nothing

unifyStep :: UnificationState -> UnificationState
unifyStep (u, k, s) =
    case asum [unifyStep' (u, k, s) (ui, ki) | ui <- Set.toList u, ki <- Map.toList k ++ repeat ("__AUX__", T.Universal)] of
        Just result -> result
        Nothing -> error "Types do not unify"

-- | Most general unification for two types.
unify :: KindAssignment -> [TypePair] -> (KindAssignment, Substitution)
unify k us =
    unificationAlgorithm
        ( Set.fromList $ map (Data.Bifunctor.bimap T.concreteType T.concreteType) us
        , k
        , nullSubstitution
        )
  where
    unificationAlgorithm :: UnificationState -> (KindAssignment, Substitution)
    unificationAlgorithm (us', k', s)
        | Set.null us' = (k', s)
        | otherwise = unificationAlgorithm (unifyStep (us', k', s))

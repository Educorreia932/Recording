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
        ((T.Parameter x1, T.Parameter x2), (x1', T.RecordKind f1 _))
            | x1 == x1' ->
                case Map.lookup x2 k of
                    Just (T.RecordKind f2 _) ->
                        let s' = Map.singleton x1 (T.Parameter x2)
                            u' = Set.delete ui u
                            k' = Map.delete x1 k
                         in Just
                                ( apply s' (u' `Set.union` Set.fromList [(f1 Map.! l, f2 Map.! l) | l <- Map.keys f1, l `Map.member` f2])
                                , apply s' (k' `Map.union` Map.singleton x2 (T.RecordKind (apply s' (f1 `Map.union` f2)) Map.empty))
                                , s `composeSubs` s'
                                )
                    _ -> Nothing
        -- (IV)
        ((T.Record f2, T.Parameter x), (x', T.RecordKind f1 _))
            | x == x'
                && (Map.keysSet f1 `Set.intersection` Map.keysSet f2 == Map.keysSet f1)
                && not (x `Set.member` ftv (T.Record f2)) ->
                unifyStep' (u, k, s) ((T.Parameter x, T.Record f2), (x', T.RecordKind f1 Map.empty))
        ((T.Parameter x, T.Record f2), (x', T.RecordKind f1 _))
            | x == x'
                && (Map.keysSet f1 `Set.intersection` Map.keysSet f2 == Map.keysSet f1)
                && not (x `Set.member` ftv (T.Record f2)) ->
                let s' = Map.singleton x (T.Record f2)
                    u' = Set.delete ui u
                    k' = Map.delete x k
                 in Just
                        ( apply s' (u' `Set.union` Set.fromList [(f1 Map.! l, f2 Map.! l) | l <- Map.keys f1])
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

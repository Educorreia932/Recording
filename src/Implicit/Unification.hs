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

unifyStep' :: UnificationState -> (TypePair, (String, T.Kind)) -> TI (Maybe UnificationState)
unifyStep' (u, k, s) (ui, ki) =
    case (ui, ki) of
        -- (I)
        ((t1, t2), _)
            | t1 == t2 ->
                let u' = Set.delete ui u
                 in return $
                        Just
                            ( u'
                            , k
                            , s
                            )
        -- (II)
        ((t, T.Parameter x), (x', T.Universal))
            | x == x' && not (x `Set.member` ftv t) -> unifyStep' (u, k, s) ((T.Parameter x, t), (x', T.Universal))
        ((T.Parameter x, t), (x', T.Universal))
            | x == x' && not (x `Set.member` ftv t) ->
                let s' = Map.singleton x t
                    u' = Set.delete ui u
                    k' = Map.delete x k
                 in return $
                        Just
                            ( apply s' u'
                            , apply s' k'
                            , s `composeSubs` s'
                            )
        -- (III)
        ((T.Parameter x1, T.Parameter x2), (x1', T.RecordKind f1l f1r))
            | x2 == x1' -> unifyStep' (u, k, s) ((T.Parameter x2, T.Parameter x1), (x1', T.RecordKind f1l f1r))
        ((T.Parameter x1, T.Parameter x2), (x1', T.RecordKind f1l f1r))
            | x1 == x1' ->
                case Map.lookup x2 k of
                    Just (T.RecordKind f2l f2r)
                        | (Map.keysSet f1l `Set.disjoint` Map.keysSet f2r)
                            && (Map.keysSet f1r `Set.disjoint` Map.keysSet f2l) ->
                            let s' = Map.singleton x1 (T.Parameter x2)
                                u' = Set.delete ui u
                                k' = Map.delete x2 $ Map.delete x1 k
                                ul = Set.fromList [(f1l Map.! l, f2l Map.! l) | l <- Map.keys f1l, l `Map.member` f2l]
                                ur = Set.fromList [(f1r Map.! l, f2r Map.! l) | l <- Map.keys f1r, l `Map.member` f2r]
                                ks = Map.singleton x2 $ apply s' (T.RecordKind (f1l `Map.union` f2l) (f1r `Map.union` f2r))
                             in return $
                                    Just
                                        ( apply s' $ u' `Set.union` ul `Set.union` ur
                                        , apply s' k' `Map.union` apply s' ks
                                        , s `composeSubs` s'
                                        )
                    _ -> return Nothing
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
                 in return $
                        Just
                            ( apply s' $ u' `Set.union` us
                            , apply s' k'
                            , s `composeSubs` s'
                            )
        -- (V)
        ((T.Record f1, T.Record f2), _)
            | Map.keysSet f1 == Map.keysSet f2 ->
                let u' = Set.delete ui u
                 in return $
                        Just
                            ( u' `Set.union` Set.fromList [(f1 Map.! l, f2 Map.! l) | l <- Map.keys f1]
                            , k
                            , s
                            )
        -- (VI)
        ((t1 `T.Arrow` t2, t1' `T.Arrow` t2'), _) ->
            let u' = Set.delete ui u
             in return $
                    Just
                        ( u' `Set.union` Set.fromList [(t1, t1'), (t2, t2')]
                        , k
                        , s
                        )
        --- (VII)
        ((chi, T.Parameter x), (x', T.RecordKind f1l f1r))
            | x == x' -> unifyStep' (u, k, s) ((T.Parameter x, chi), (x', T.RecordKind f1l f1r))
        ((T.Parameter x, chi), (x', T.RecordKind f1l f1r))
            | x == x' && not (null (T.typeModifications chi)) ->
                case T.root chi of
                    T.Parameter chi' ->
                        case Map.lookup chi' k of
                            Just (T.RecordKind f2l f2r)
                                | Map.keysSet f1l `Set.disjoint` Map.keysSet (T.contractions chi)
                                    && Map.keysSet f1r
                                        `Set.disjoint` Map.keysSet
                                            (f2r `Map.union` (f2l `Map.difference` T.contractions chi))
                                    && not (x `Set.member` ftv chi) ->
                                    let s' = Map.singleton x chi
                                        u' = Set.delete ui u
                                        k' = Map.delete x k
                                     in return $
                                            Just
                                                ( apply s' u'
                                                , apply s' k'
                                                , s `composeSubs` s'
                                                )
                            _ -> return Nothing
                    _ -> return Nothing
        --- (VIII)
        ((t1, t2), _)
            | not $ null tm ->
                return $
                    let u' = Set.delete ui u
                        (xi, li, (taui, tauj)) = head tm
                        ui' = fmap (\t -> T.removeTypeModification t (xi, li)) ui
                     in Just
                            ( u' `Set.union` Set.fromList [(taui, tauj), ui']
                            , k
                            , s
                            )
          where
            tm1 = T.typeModifications t1
            tm2 = T.typeModifications t2
            tm = [(x1, x2, (x3, y3)) | (x1, x2, x3) <- tm1, (y1, y2, y3) <- tm2, x1 == y1 && x2 == y2]
        --- (IX)
        ((t1, t2), (x1, T.RecordKind f1l f1r))
            | T.root (T.Parameter x1) == T.root t2 -> unifyStep' (u, k, s) ((t2, t1), (x1, T.RecordKind f1l f1r))
        ((t1, t2), (x1, T.RecordKind f1l f1r))
            | T.root (T.Parameter x1) == T.root t1 ->
                case Map.lookup alpha2 k of
                    Just (T.RecordKind f2l f2r)
                        | Map.keysSet f1l `Set.disjoint` Map.keysSet f2r
                            && Map.keysSet f1r `Set.disjoint` Map.keysSet f2l
                            && not (alpha1 `Set.member` ftv t2)
                            && not (alpha2 `Set.member` ftv t1)
                            && all (\((_, l1, _), (_, l2, _)) -> l1 /= l2) (zip (T.typeModifications t1) (T.typeModifications t2)) -> do
                            alpha <- freshType
                            let
                                (T.Parameter alpha') = alpha
                                s' =
                                    Map.fromList
                                        [ (alpha1, T.replaceRoot t2 (T.Parameter alpha'))
                                        , (alpha2, T.replaceRoot t1 (T.Parameter alpha'))
                                        ]
                                u' = Set.delete ui u
                                us =
                                    Set.fromList [(f1l Map.! l, f2l Map.! l) | l <- Map.keys f1l, l `Map.member` f2l]
                                        `Set.union` Set.fromList [(f1r Map.! l, f2r Map.! l) | l <- Map.keys f1r, l `Map.member` f2r]
                                k' = Map.delete alpha2 $ Map.delete alpha1 k
                                ks = Map.singleton alpha' $ T.RecordKind (f1l `Map.union` f2l) (f1r `Map.union` f2r)
                            return $
                                Just
                                    ( apply s' $ u' `Set.union` us
                                    , apply s' $ k' `Map.union` ks
                                    , s `composeSubs` s'
                                    )
                    _ -> return Nothing
          where
            T.Parameter alpha1 = T.root t1
            T.Parameter alpha2 = T.root t2
        _ -> return Nothing

firstJustM :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (x : xs) = do
    mx <- x
    case mx of
        Just _ -> return mx
        Nothing -> firstJustM xs

unifyStep :: UnificationState -> TI UnificationState
unifyStep (u, k, s) = do
    let results =
            [ unifyStep' (u, k, s) (ui, ki)
            | ui <- Set.toList u
            , ki <- Map.toList k ++ replicate (length u) ("__AUX__", T.Universal)
            ]
    case results of
        [] -> error "Types do not unify"
        _ -> do
            result <- firstJustM results
            case result of
                Just result' -> return result'
                Nothing -> error "Types do not unify"

-- | Most general unification for two types.
unify :: KindAssignment -> [TypePair] -> TI (KindAssignment, Substitution)
unify k us =
    unificationAlgorithm
        ( Set.fromList $ map (Data.Bifunctor.bimap T.concreteType T.concreteType) us
        , k
        , nullSubstitution
        )
  where
    unificationAlgorithm :: UnificationState -> TI (KindAssignment, Substitution)
    unificationAlgorithm (us', k', s)
        | Set.null us' = return (k', s)
        | otherwise = do
            newState <- unifyStep (us', k', s)
            unificationAlgorithm newState

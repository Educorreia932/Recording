module Implicit.TypeInference where

import Control.Monad.State
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap

import Control.Lens
import Data.Set qualified as Set
import Explicit.Terms qualified as E
import Explicit.Types qualified as T

import Data.Bifunctor qualified
import Data.List (intersect)
import Data.Traversable
import Debug.Trace
import Implicit.Parser
import Implicit.Terms

data Scheme = Scheme [String] T.Type deriving (Show)
type Substitution = Map.Map String T.Type

type KindAssignment = Map.Map String T.Kind
type TypeAssignment = Map.Map String Scheme
type InferenceState = (KindAssignment, TypeAssignment, Int)

freshType :: (State InferenceState) T.Type
freshType = do
  (k, t, i) <- get
  put (k, t, i + 1)
  return $ T.Parameter $ "_s" ++ show (i + 1)

class Types a where
  ftv :: a -> Set.Set String
  apply :: Substitution -> a -> a

instance Types T.Type where
  ftv T.Int = Set.empty
  ftv T.String = Set.empty
  ftv (T.Parameter x) = Set.singleton x
  ftv (T.Arrow t1 t2) = ftv t1 `Set.union` ftv t2
  ftv (T.Record m) = Set.unions $ fmap ftv m
  ftv (T.ForAll (l, _) t') = ftv t' Set.\\ Set.singleton l

  apply substitution = sub
   where
    sub T.Int = T.Int
    sub T.String = T.String
    sub (T.Parameter p)
      | substitution & has (ix p) = substitution Map.! p
      | otherwise = T.Parameter p
    sub (T.Arrow t1 t2) = T.Arrow (sub t1) (sub t2)
    sub (T.Record m) = T.Record $ fmap sub m
    sub (T.ForAll (t, k) t')
      | substitution & has (ix t) =
          let substitution' = Map.delete t substitution
           in T.ForAll (t, k) $ apply substitution' t'
      | otherwise = T.ForAll (t, k) $ sub t'

instance Types T.Kind where
  ftv T.Universal = Set.empty
  ftv (T.RecordKind m) = Set.unions $ fmap ftv m

  apply substitution = sub
   where
    sub T.Universal = T.Universal
    sub (T.RecordKind m) = T.RecordKind $ fmap (apply substitution) m

instance Types Scheme where
  ftv :: Scheme -> Set.Set String
  ftv (Scheme params t) = ftv t Set.\\ Set.fromList params

  apply :: Substitution -> Scheme -> Scheme
  apply substitution (Scheme params t) = Scheme params t'
   where
    t' = apply (foldr Map.delete substitution params) t

instance (Types a) => Types [a] where
  ftv = foldr (Set.union . ftv) Set.empty
  apply s = map (apply s)

instance Types E.Expression where
  ftv (E.Literal _) = Set.empty
  ftv (E.String _) = Set.empty
  ftv (E.Variable _ ts) = ftv ts
  ftv (E.Abstraction _ t e) = ftv t `Set.union` ftv e
  ftv (E.Application e1 e2) = ftv e1 `Set.union` ftv e2
  ftv (E.Let _ t e1 e2) = ftv t `Set.union` ftv e1 `Set.union` ftv e2
  ftv (E.Poly e t) = ftv e `Set.union` ftv t
  ftv (E.ERecord m) = Set.unions $ fmap ftv m
  ftv (E.Dot e t _) = ftv e `Set.union` ftv t
  ftv (E.Modify e1 t _ e2) = ftv e1 `Set.union` ftv t `Set.union` ftv e2

  apply substitution = sub
   where
    sub (E.Literal a) = E.Literal a
    sub (E.String a) = E.String a
    sub (E.Variable x ts) = E.Variable x $ apply substitution ts
    sub (E.Abstraction x t e) = E.Abstraction x (apply substitution t) (sub e)
    sub (E.Application e1 e2) = E.Application (sub e1) (sub e2)
    sub (E.Let x t e1 e2) = E.Let x (apply substitution t) (sub e1) (sub e2)
    sub (E.Poly e t) = E.Poly (sub e) (apply substitution t)
    sub (E.ERecord m) = E.ERecord $ fmap (apply substitution) m
    sub (E.Dot e t l) = E.Dot (sub e) (apply substitution t) l
    sub (E.Modify e1 t l e2) = E.Modify (sub e1) (apply substitution t) l (sub e2)

instance Types (Map.Map String T.Type) where
  ftv = ftv . Map.elems
  apply substitution = Map.map (apply substitution)

instance Types TypeAssignment where
  ftv = ftv . Map.elems
  apply substitution = Map.map (apply substitution)

instance Types T.KindedType where
  ftv (x, _) = Set.singleton x
  apply substitution (x, k) = (x', k)
   where
    x'
      | substitution & has (ix x) = case substitution Map.! x of
          T.Parameter p -> p
          _ -> x
      | otherwise = x

instance (Types a) => Types (a, a) where
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2
  apply substitution (t1, t2) = (apply substitution t1, apply substitution t2)

nullSubstitution :: Substitution
nullSubstitution = Map.empty

composeSubs :: Substitution -> Substitution -> Substitution
composeSubs s1 s2 = Map.map (apply s1) s2 `Map.union` s1

{- | Create a type scheme out of a type.

__Examples:__

@
generalize {} s1 == ([s1], s1)
generalize {x: ([], s1)} (s1 -> s2) == ([s2], s1 -> s2)
@
-}
generalize :: KindAssignment -> TypeAssignment -> T.Type -> Scheme
generalize kindAssign typeAssign t = traceShow kindAssign Scheme params t'
 where
  params = Set.toList (ftv t Set.\\ ftv typeAssign)
  t' = foldl (\acc v -> T.ForAll (v, kindAssign Map.! v) acc) t (Set.toList $ ftv t)

{- | Replaces all bound type variables in a type scheme with fresh type variables.

__Examples:__

@
instantiate ([t1], t1) == (s1, {})
instantiate ([t1, t2], t1 -> t2) == (s1 -> s2, {(t1, s1), (t2, s2)})
@
-}
instantiate :: Scheme -> State InferenceState (T.Type, Substitution)
instantiate (Scheme params t) = do
  freshTypes <- mapM (const freshType) params
  let substitution = Map.fromList $ zip params freshTypes
      instantiated = apply substitution t
  return (instantiated, substitution)

type TypePair = (T.Type, T.Type)
type UnificationState = (Substitution, [T.KindedType], [TypePair])


-- TODO: Can't use lists. Has to match against unordered maps.
unifyStep :: UnificationState -> UnificationState
unifyStep (s, k, []) = (s, k, [])
-- Constant types
unifyStep (s, k, (T.Int, T.Int) : us) = (s, k, us)
unifyStep (s, k, (T.String, T.String) : us) = (s, k, us)
-- Type variables [Universal Kind]
unifyStep (s, (_, T.Universal) : ks, (T.Parameter x1, t2) : us) =
  ( s `composeSubs` s'
  , apply s' ks
  , apply s' us
  )
 where
  s' = varBind x1 t2
unifyStep (s, k@((_, T.Universal) : _), (t1, T.Parameter x2) : us) = (s, k, (T.Parameter x2, t1) : us)
-- Type variables [Record Kinds]
unifyStep (s, (_, T.RecordKind f1) : (_, T.RecordKind f2) : ks, (T.Parameter x1, T.Parameter x2) : us) =
  ( s `composeSubs` s'
  , k' : ks
  , u' ++ us
  )
 where
  s' = Map.singleton x1 (T.Parameter x2)
  k' = (x2, T.RecordKind $ apply s' (f1 `Map.union` f2))
  u' = [(f1 Map.! l, f2 Map.! l) | l <- Map.keys f1, l `elem` Map.keys f2]
-- Type variable <-> Record type
unifyStep (s, (_, T.RecordKind f1) : ks, (T.Parameter x, T.Record f2) : us)
  -- dom(f1) ⊆ dom(f2) and x ∉ ftv(f2)
  | (Map.keysSet f2 `Set.intersection` Map.keysSet f1 == Map.keysSet f1)
      && not (x `Set.member` ftv (T.Record f2)) =
      ( s `composeSubs` s' `composeSubs` Map.singleton x (T.Record f2)
      , apply s' ks
      , apply s' (zip (Map.elems f1) (Map.elems f2)) ++ apply s' us
      )
  | otherwise = error "Types do not unify"
 where
  s' = varBind x (T.Record f2)
-- Record types
unifyStep (s, k, (T.Record f1, T.Record f2) : us)
  -- dom(f1) == dom(f2)
  | Map.keysSet f1 == Map.keysSet f2 =
      (s, k, zip (Map.elems f1) (Map.elems f2) ++ us)
  | otherwise = error "Types do not unify"
-- Function type
unifyStep (s, k, (t1 `T.Arrow` t2, t1' `T.Arrow` t2') : us) = (s, k, (t1, t1') : (t2, t2') : us)
-- General case
unifyStep (s, k, (t1, t2) : us)
  -- Unify same type
  | t1 == t2 = (s, k, us)
  | otherwise = traceShow (t1, t2, k) error "Types do not unify"

-- | Most general unification for two types.
unify :: [T.KindedType] -> [TypePair] -> (KindAssignment, Substitution)
unify k us = unificationAlgorithm (nullSubstitution, k, map (Data.Bifunctor.bimap T.concreteType T.concreteType) us)
 where
  unificationAlgorithm :: UnificationState -> (KindAssignment, Substitution)
  unificationAlgorithm (s, k', []) = (Map.fromList k', s)
  unificationAlgorithm (s, k', us') = unificationAlgorithm (unifyStep (s, k', us'))

{- | Attempts to bind a type variable to a type and returns that binding as a substitution.
Avoids binding a variable to itself and performs the occurs check.
-}
varBind :: String -> T.Type -> Substitution
varBind x t
  | t == T.Parameter x = nullSubstitution
  | x `Set.member` ftv t = error $ "Occurs check failed: " ++ x ++ " vs. " ++ show t
  | otherwise = Map.singleton x t

infer' :: Expression -> State InferenceState (Substitution, E.Expression, T.Type)
-- Constants
infer' (Literal a) = return (nullSubstitution, E.Literal a, T.Int)
infer' (String a) = return (nullSubstitution, E.String a, T.String)
-- Variable
infer' (Variable x) =
  do
    (kindAssign, typeAssign, _) <- get
    let typeScheme = case Map.lookup x typeAssign of
          Nothing -> error $ "Unbound variable: " ++ x
          Just t -> t
    (instantiatedType, substitution) <- instantiate typeScheme
    let newTypes = Map.elems substitution
        kindAssign' = kindAssign `Map.union` Map.fromList (zip newTypes' kinds)
         where
          newTypes' = map fst $ T.typeParameters instantiatedType
          kinds = apply substitution (Map.elems kindAssign)
    (_, _, i') <- get
    put (kindAssign', typeAssign, i')
    return (nullSubstitution, E.Variable x newTypes, instantiatedType)

-- Abstraction
infer' (Abstraction x e) =
  do
    ft <- freshType
    (kindAssign, typeAssign, i) <- get
    let
      (T.Parameter ft') = ft
      kindAssign' = Map.insert ft' T.Universal kindAssign
      typeAssign' = Map.insert x (Scheme [] ft) typeAssign
    put (kindAssign', typeAssign', i)
    (s, m, t) <- infer' e
    return (s, E.Abstraction x (apply s ft) m, apply s ft `T.Arrow` t)

-- Application
infer' (Application e1 e2) =
  do
    (s1, m1, t1) <- infer' e1
    (kindAssign, typeAssign, i) <- get
    let typeAssign' = apply s1 typeAssign
    put (kindAssign, typeAssign', i)
    (s2, m2, t2) <- infer' e2
    t <- freshType
    let
      T.Parameter t' = t
      (k3, s3) = unify [(t', T.Universal)] [(apply s2 t1, t2 `T.Arrow` t)]
    (_, _, i') <- get
    put (k3, typeAssign', i')
    return
      ( s1 `composeSubs` s2 `composeSubs` s3
      , E.Application
          (apply (s2 `composeSubs` s3) m1)
          (apply s3 m2)
      , apply s3 t
      )

-- Record
infer' (Record r) =
  do
    (kindAssign, typeAssign, i) <- get
    let
      (_, e1) = head (Map.toList r)
      n = length r
    (s1, m1, t1) <- infer' e1
    -- Infer for all fields
    (_, fieldsInference) <-
      mapAccumM
        ( \(s, m, t) (_, value) ->
            do
              let typeAssign' = apply s typeAssign
              put (kindAssign, typeAssign', i)
              (s', m', t') <- infer' value
              return
                ( (composeSubs s s', m ++ [m'], t ++ [t'])
                , (s', m', t')
                )
        )
        (nullSubstitution, [], [])
        (tail $ Map.toList r)
    return
      ( foldr (composeSubs . (\(s, _, _) -> s)) s1 fieldsInference
      , E.ERecord $
          OMap.fromList $
            zip
              (Map.keys r)
              (m1 : map (\(_, m, _) -> m) fieldsInference)
      , T.Record $
          Map.fromList $
            zip
              (Map.keys r)
              (t1 : map (\(_, _, t) -> t) fieldsInference)
      )

-- Dot
infer' (Dot e l) =
  do
    (s1, m1, t1) <- infer' e
    (kindAssign, typeAssign, _) <- get
    ft1 <- freshType
    ft2 <- freshType
    let
      (T.Parameter ft1') = ft1
      (T.Parameter ft2') = ft2
      kindAssign' =
        kindAssign
          `Map.union` Map.fromList
            [ (ft1', T.Universal)
            , (ft2', T.RecordKind $ Map.singleton l ft1)
            ]
    (_, _, i') <- get
    _ <- put (kindAssign', typeAssign, i')
    let (_, s2) = unify [(ft1', T.Universal), (ft2', T.RecordKind (Map.singleton l ft1))] [(ft2, t1)]
    return
      ( s2 `composeSubs` s1
      , E.Dot (apply s2 m1) (apply s2 ft2) l
      , apply s2 ft1
      )

-- Modify
infer' (Modify e1 l e2) =
  do
    (s1, m1, t1) <- infer' e1
    (k1, typeAssign, i) <- get
    let typeAssign' = apply s1 typeAssign
    put (k1, typeAssign', i)
    (s2, m2, t2) <- infer' e2
    (k2, _, _) <- get
    ft1 <- freshType
    ft2 <- freshType
    let
      (T.Parameter ft1') = ft1
      (T.Parameter ft2') = ft2
      (k3', s3) =
        unify
          ([(ft1', T.Universal), (ft2', T.RecordKind (Map.singleton l ft1))])
          [(ft1, t2), (ft2, apply s2 t1)]
      k3 = k2 `Map.union` k3'
    (_, _, i') <- get
    put (k3, typeAssign', i')
    return $
      traceShow
        k3'
        ( s1 `composeSubs` s2 `composeSubs` s3
        , E.Modify (apply (s2 `composeSubs` s3) m1) (apply s3 ft2) l (apply s3 m2)
        , apply s3 ft2
        )

-- Let expression
infer' (Let x e1 e2) =
  do
    (s1, m1, t1) <- infer' e1
    (kindAssign, typeAssign, i) <- get
    let t' = generalize kindAssign (apply s1 typeAssign) t1
        typeAssign' = Map.insert x t' (apply s1 typeAssign)
    put (kindAssign, typeAssign', i)
    (s2, m2, t2) <- infer' e2
    (t'', _) <- instantiate t'
    return
      ( s2 `composeSubs` s1
      , E.Let x (apply s2 t'') (E.Poly m1 (apply s2 t'')) m2
      , t2
      )

-- | Infers the type of an expression.
infer :: String -> (E.Expression, T.Type)
infer s = (expression', t)
 where
  startState = (Map.empty, Map.empty, 0)
  expression = parseExpression s
  (_, expression', t) = evalState (infer' expression) startState

inferWithState :: String -> InferenceState -> (E.Expression, T.Type)
inferWithState s startState = (expression', t)
 where
  expression = parseExpression s
  (_, expression', t) = evalState (infer' expression) startState

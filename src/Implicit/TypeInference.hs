module Implicit.TypeInference where

import Control.Monad.State
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap

import Control.Lens
import Data.Set qualified as Set
import Explicit.Terms qualified as E
import Explicit.Types qualified as T

import Data.Traversable
import Debug.Trace
import Implicit.Parser
import Implicit.Terms

data Scheme = Scheme [String] T.Type
type Substitution = Map.Map String T.Type

type KindAssignment = Map.Map String T.Kind
type TypeAssignment = Map.Map String Scheme
type InferenceState = (KindAssignment, TypeAssignment)

class Types a where
  ftv :: a -> Set.Set String
  apply :: Substitution -> a -> a

instance Types T.Type where
  ftv T.Int = Set.empty
  ftv T.String = Set.empty
  ftv (T.Parameter x) = Set.singleton x
  ftv (T.Arrow t1 t2) = ftv t1 `Set.union` ftv t2
  ftv (T.Record m) = Set.unions $ fmap ftv m
  ftv (T.ForAll (l, k) t') = ftv t' Set.\\ Set.singleton l

  apply substitution = sub
   where
    sub T.Int = T.Int
    sub T.String = T.String
    sub (T.Parameter p)
      | substitution & has (ix p) = substitution Map.! p
      | otherwise = T.Parameter p
    sub (T.Arrow t1 t2) = T.Arrow (sub t1) (sub t2)
    sub (T.Record m) = T.Record $ fmap sub m
    sub (T.ForAll (l, k) t')
      | substitution & has (ix l) =
          case substitution Map.! l of
            T.Parameter x -> T.ForAll (x, k) $ sub t'
            _ -> error "Invalid substitution"
      | otherwise = T.ForAll (l, k) $ sub t'

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

instance Types TypeAssignment where
  ftv :: TypeAssignment -> Set.Set String
  ftv = ftv . Map.elems
  apply substitution = Map.map (apply substitution)

instance (Types a) => Types (a, a) where
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2
  apply substitution (t1, t2) = (apply substitution t1, apply substitution t2)

nullSubstitution :: Substitution
nullSubstitution = Map.empty

composeSubs :: Substitution -> Substitution -> Substitution
composeSubs s1 s2 = Map.map (apply s1) s2 `Map.union` s1

freshType :: TypeAssignment -> T.Type
freshType typeAssign = T.Parameter $ "s" ++ show (length typeAssign + 1)

-- | Abstracts a type over all type variables which are free in the type but not in the given type assignment.
generalize :: TypeAssignment -> T.Type -> Scheme
generalize typeAssign t = Scheme params t
 where
  params = Set.toList (ftv t Set.\\ ftv typeAssign)

-- | Replaces all bound type variables in a type scheme with fresh type variables.
instantiate :: Scheme -> (T.Type, Substitution)
instantiate (Scheme params t) =
  let freshTypes = map (\x -> T.Parameter $ "s" ++ show x) [1 .. length params]
      substitution = Map.fromList $ zip params freshTypes
      instantiated = apply substitution t
   in (instantiated, substitution)

type TypePair = (T.Type, T.Type)
type UnificationState = (Substitution, [T.KindedType], [TypePair])

unifyStep :: UnificationState -> UnificationState
unifyStep (s, k, []) = (s, k, [])
-- Constant types
unifyStep (s, k, (T.Int, T.Int) : us) = (s, k, us)
unifyStep (s, k, (T.String, T.String) : us) = (s, k, us)
unifyStep (s, (_, T.RecordKind f1) : (_, T.RecordKind f2) : ks, (T.Parameter x1, T.Parameter x2) : us) = (s, ks, us)
-- Type variable
unifyStep (s, (_, T.Universal) : ks, (T.Parameter x1, t2) : us) = (s `composeSubs` s', ks, apply s' us)
 where
  s' = varBind x1 t2
unifyStep (s, k, (t1, T.Parameter x2) : us) = (s `composeSubs` s', k, apply s' us)
 where
  s' = varBind x2 t1
-- Record type
unifyStep (s, (_, T.RecordKind f1) : ks, (T.Parameter x, T.Record f2) : us)
  | (Map.keysSet f2 `Set.intersection` Map.keysSet f1 == Map.keysSet f1)
      && not (x `Set.member` ftv (T.Record f2)) =
      (s `composeSubs` s', ks, apply s' us)
  | otherwise = error "Types do not unify"
 where
  s' = varBind x (T.Record f2)
unifyStep (s, k, (T.Record f1, T.Record f2) : us)
  | Map.keysSet f1 == Map.keysSet f2 =
      (s, k, zip (Map.elems f1) (Map.elems f2) ++ us)
  | otherwise = error "Types do not unify"
-- Function type
unifyStep (s, k, (t1 `T.Arrow` t2, t1' `T.Arrow` t2') : us) = (s, k, (t1, t1') : (t2, t2') : us)
-- General case
unifyStep (s, k, (t1, t2) : us)
  -- Unify same type
  | t1 == t2 = (s, k, us)
  | otherwise = error "Types do not unify"

-- | Most general unification for two types.
unify :: [T.KindedType] -> [TypePair] -> Substitution
unify k us = unificationAlgorithm (nullSubstitution, k, us)
 where
  unificationAlgorithm :: UnificationState -> Substitution
  unificationAlgorithm (s, _, []) = s
  unificationAlgorithm (s, k, us) = unificationAlgorithm (unifyStep (s, k, us))

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
infer' (Literal a) = return (Map.empty, E.Literal a, T.Int)
infer' (String a) = return (Map.empty, E.String a, T.String)
-- Variable
infer' (Variable x) =
  do
    (kindAssign, typeAssign) <- get
    let typeScheme = case Map.lookup x typeAssign of
          Nothing -> error $ "Unbound variable: " ++ x
          Just t -> t
        (instantiatedType, substitution) = instantiate typeScheme
        newTypes = Map.elems substitution
    return (Map.empty, E.Variable x newTypes, instantiatedType)

-- Abstraction
infer' (Abstraction x e) =
  do
    (kindAssign, typeAssign) <- get
    let
      ft = freshType typeAssign
      kindAssign' = Map.insert x T.Universal kindAssign
      typeAssign' = Map.insert x (Scheme [] ft) typeAssign
    _ <- put (kindAssign', typeAssign')
    (s, m, t) <- infer' e
    return (s, E.Abstraction x ft m, apply s ft `T.Arrow` t)

-- Application
infer' (Application e1 e2) =
  do
    (kindAssign, typeAssign) <- get
    (s1, m1, t1) <- infer' e1
    let typeAssign' = apply s1 typeAssign
    _ <- put (kindAssign, typeAssign')
    (s2, m2, t2) <- infer' e2
    let t@(T.Parameter x) = freshType typeAssign
        s3 = unify [(x, T.Universal)] [(apply s2 t1, t2 `T.Arrow` t)]
    return (s1 `composeSubs` s2 `composeSubs` s3, E.Application m1 m2, apply s3 t)

-- Record
infer' (Record r) =
  do
    (kindAssign, typeAssign) <- get
    let
      (_, e1) = head (Map.toList r)
      n = length r
    (s1, m1, t1) <- infer' e1
    -- Infer for all fields
    (_, fieldsInference) <-
      mapAccumM
        ( \(s, m, t) (key, value) ->
            do
              let typeAssign' = apply s typeAssign
              _ <- put (kindAssign, typeAssign')
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
    (kindAssign, typeAssign) <- get
    let
      ft1@(T.Parameter ft1') = freshType typeAssign
      ft2@(T.Parameter ft2') = freshType typeAssign -- TODO: This will not work
      kindAssign' =
        kindAssign
          `Map.union` Map.fromList
            [ (ft1', T.Universal)
            , (ft2', T.RecordKind $ Map.singleton l ft1)
            ]
    _ <- put (kindAssign', typeAssign)
    let s2 = unify [] [(ft2, t1)]
    return
      ( s2 `composeSubs` s1
      , E.Dot (apply s2 m1) (apply s2 ft2) l
      , apply s2 ft1
      )

-- Modify
infer' (Modify e1 l e2) =
  do
    (s1, m1, t1) <- infer' e1
    (kindAssign, typeAssign) <- get
    let typeAssign' = apply s1 typeAssign
    _ <- put (kindAssign, typeAssign')
    (s2, m2, t2) <- infer' e2
    let
      ft1@(T.Parameter ft1') = freshType typeAssign'
      ft2@(T.Parameter ft2') = T.Parameter (ft1' ++ "'") -- TODO: This will not work
      s3 =
        unify
          [("t1", T.Universal), ("t2", T.RecordKind (Map.singleton l t1))]
          [(ft1, t2), (ft2, apply s2 t1)]
    return
      ( s1 `composeSubs` s2 `composeSubs` s3
      , E.Modify (apply (s2 `composeSubs` s3) m1) (apply s3 ft2) l (apply s3 m2)
      , apply s3 ft2
      )

-- Let expression
infer' (Let x e1 e2) =
  do
    (s1, m1, t1) <- infer' e1
    (kindAssign, typeAssign) <- get
    let typeAssign' = apply s1 typeAssign
        t' = generalize typeAssign' t1
        typeAssign'' = Map.insert x t' typeAssign'
    _ <- put (kindAssign, typeAssign'')
    (s2, m2, t2) <- infer' e2
    let (t'', _) = instantiate t'
    return
      ( s2 `composeSubs` s1
      , E.Let x (apply s2 t'') (E.Poly m1 (apply s2 t'')) m2
      , t2
      )

infer :: String -> (E.Expression, T.Type)
infer s = (expression', t)
 where
  startState = (Map.empty, Map.empty)
  expression = parseExpression s
  (_, expression', t) = evalState (infer' expression) startState

inferWithState :: String -> InferenceState -> (E.Expression, T.Type)
inferWithState s startState = (expression', t)
 where
  expression = parseExpression s
  (_, expression', t) = evalState (infer' expression) startState

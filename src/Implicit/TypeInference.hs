module Implicit.TypeInference where

import Control.Monad.State
import Data.Map qualified as Map

import Control.Lens
import Data.Set qualified as Set
import Explicit.Terms qualified as E
import Explicit.Types qualified as T
import Implicit.Parser
import Implicit.Terms

data Scheme = Scheme [String] T.Type
type Substitution = Map.Map String T.Type

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

nullSubstitution :: Substitution
nullSubstitution = Map.empty

composeSubs :: Substitution -> Substitution -> Substitution
composeSubs s1 s2 = Map.map (apply s1) s2 `Map.union` s1

freshType :: TypeAssignment -> T.Type
freshType typeAssign = T.Parameter $ "s" ++ show (length typeAssign + 1)

type KindAssignment = Map.Map String T.Kind
type TypeAssignment = Map.Map String Scheme
type InferenceState = (KindAssignment, TypeAssignment)

instance Types TypeAssignment where
  ftv :: TypeAssignment -> Set.Set String
  ftv = ftv . Map.elems
  apply substitution = Map.map (apply substitution)

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

-- | Most general unification for two types.
unify :: T.Type -> T.Type -> Substitution
unify T.Int T.Int = nullSubstitution
unify T.String T.String = nullSubstitution
unify (T.Parameter x) t = varBind x t
unify t (T.Parameter x) = varBind x t
unify (T.Arrow t1 t2) (T.Arrow t1' t2') = s1 `composeSubs` s2
 where
  s1 = unify t1 t1'
  s2 = unify (apply s1 t2) (apply s1 t2')
unify t1 t2 = error $ "Types do not unify: " ++ show t1 ++ " vs. " ++ show t2

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
    let t = freshType typeAssign
        s3 = unify (apply s2 t1) (t2 `T.Arrow` t)
    return (s1 `composeSubs` s2 `composeSubs` s3, E.Application m1 m2, apply s3 t)

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

-- Record
-- infer' (Record r) =
--   do
--     (kindAssign, typeAssign) <- get
--     let
--       (s, m, t) =
--         foldr
--           ( \(k, v) (s', m', t') ->
--               do
--                 (s'', m'', t'') <- infer' v
--                 return (s' `composeSubs` s'', (k, m'') : m', (k, t'') : t')
--           )
--           (Map.empty, [], [])
--           r
--     return (s, E.Record m, T.Record t)

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
    let s2 = unify ft2 t1
    return
      ( s2 `composeSubs` s1
      , E.Dot m1 (apply s2 ft2) l -- TODO: Apply s2 to m1
      , apply s2 ft1
      )

-- Modify

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

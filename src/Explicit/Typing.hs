module Explicit.Typing where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Errors (RecordingException)
import Explicit.Terms qualified as E
import Explicit.Types qualified as T

data Scheme = Scheme [String] T.Type deriving (Show)

type KindAssignment = Map.Map String T.Kind

type TypeAssignment = Map.Map String Scheme

type Substitution = Map.Map String T.Type

nullSubstitution :: Substitution
nullSubstitution = Map.empty

composeSubs :: Substitution -> Substitution -> Substitution
composeSubs s1 s2 = Map.map (apply s2) s1 `Map.union` s2

class Types a where
    ftv :: a -> Set.Set String
    apply :: Substitution -> a -> a

instance Types T.Type where
    ftv T.Int = Set.empty
    ftv T.String = Set.empty
    ftv (T.Parameter x) = Set.singleton x
    ftv (T.Arrow t1 t2) = ftv t1 `Set.union` ftv t2
    ftv (T.Record m) = ftv m
    ftv (T.ForAll (l, _) t') = ftv t' Set.\\ Set.singleton l
    ftv (T.Contraction t1 _ t2) = ftv t1 Set.\\ ftv t2
    ftv (T.Extension t1 _ t2) = ftv t1 `Set.union` ftv t2

    apply substitution = sub
      where
        sub T.Int = T.Int
        sub T.String = T.String
        sub (T.Parameter p)
            | substitution & has (ix p) = substitution Map.! p
            | otherwise = T.Parameter p
        sub (T.Arrow t1 t2) = T.Arrow (sub t1) (sub t2)
        sub (T.Record m) = T.Record $ fmap sub m
        sub (T.ForAll (x, k) t)
            | substitution & has (ix x) =
                case substitution Map.! x of
                    T.Parameter x' -> T.ForAll (x', k') $ apply substitution t
                    _ ->
                        let substitution' = Map.delete x substitution
                         in T.ForAll (x, k') $ apply substitution' t
            | otherwise = T.ForAll (x, k') $ sub t
          where
            k' = apply substitution k
        sub (T.Contraction t1 l t2) = T.Contraction (sub t1) l (sub t2)
        sub (T.Extension t1 l t2) = T.Extension (sub t1) l (sub t2)

instance Types T.Kind where
    ftv T.Universal = Set.empty
    ftv (T.RecordKind l r) = ftv l `Set.union` ftv r

    apply substitution = sub
      where
        sub T.Universal = T.Universal
        sub (T.RecordKind l r) = T.RecordKind (apply substitution l) (apply substitution r)

instance Types Scheme where
    ftv (Scheme params t) = ftv t Set.\\ Set.fromList params

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
    ftv (E.Record m) = Set.unions $ fmap ftv m
    ftv (E.Dot e t _) = ftv e `Set.union` ftv t
    ftv (E.Modify e1 t _ e2) = ftv e1 `Set.union` ftv t `Set.union` ftv e2
    ftv (E.Contract e t _) = ftv e `Set.union` ftv t
    ftv (E.Extend e1 t _ e2) = ftv e1 `Set.union` ftv t `Set.union` ftv e2

    apply substitution = sub
      where
        sub (E.Literal a) = E.Literal a
        sub (E.String a) = E.String a
        sub (E.Variable x ts) = E.Variable x $ apply substitution ts
        sub (E.Abstraction x t e) = E.Abstraction x (apply substitution t) (sub e)
        sub (E.Application e1 e2) = E.Application (sub e1) (sub e2)
        sub (E.Let x t e1 e2) = E.Let x (apply substitution t) (sub e1) (sub e2)
        sub (E.Poly e t) = E.Poly (sub e) (apply substitution t)
        sub (E.Record m) = E.Record $ fmap (apply substitution) m
        sub (E.Dot e t l) = E.Dot (sub e) (apply substitution t) l
        sub (E.Modify e1 t l e2) = E.Modify (sub e1) (apply substitution t) l (sub e2)
        sub (E.Contract e t l) = E.Contract (sub e) (apply substitution t) l
        sub (E.Extend e1 t l e2) = E.Extend (sub e1) (apply substitution t) l (sub e2)

instance (Types a) => Types (Map.Map String a) where
    ftv = ftv . Map.elems

    apply substitution xs = Map.map (apply substitution) xs'
      where
        xs' =
            Map.mapKeys
                ( \k ->
                    fromMaybe
                        k
                        ( case Map.lookup k substitution of
                            Just (T.Parameter x) -> Just x
                            _ -> Nothing
                        )
                )
                xs

instance (Types a, Ord a) => Types (Set.Set a) where
    ftv = foldr (Set.union . ftv) Set.empty
    apply substitution = Set.map (apply substitution)

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

{--
Type inference state containing a counter for generating fresh type variables.
--}
type TIState = Int

type TI a = ExceptT RecordingException (State TIState) a

type TypeEnv = (KindAssignment, TypeAssignment)

freshType :: TI (T.Type, String)
freshType = do
    i <- get
    put $ i + 1
    let x = "t" ++ show i
    return (T.Parameter x, x)

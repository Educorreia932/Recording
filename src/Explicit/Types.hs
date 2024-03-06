module Explicit.Types where

import Data.List (intercalate)
import Data.Map qualified as Map

type Fields = Map.Map String Type

data Kind
    = Universal
    | RecordKind Fields Fields
    deriving (Eq, Ord)

instance Show Kind where
    show Universal = "U"
    show (RecordKind m1 m2) = "{{ " ++ showFields m1 ++ " || " ++ showFields m2 ++ " }}"
      where
        showFields x = intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) $ Map.toAscList x)

type KindedType = (String, Kind)

data Type
    = Int
    | String
    | Parameter String
    | Arrow Type Type
    | Record Fields
    | ForAll KindedType Type
    | Extension Type String Type
    | Contraction Type String Type
    deriving (Eq, Ord)

instance Show Type where
    show Int = "Int"
    show String = "String"
    show (Parameter p) = p
    show (Arrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show (ForAll (t, k) t') = "∀" ++ t ++ "::" ++ show k ++ "." ++ show t'
    show (Record m) = "{ " ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) $ Map.toAscList m) ++ " }"
    show (Extension t1 l t2) = show t1 ++ " + { " ++ l ++ ": " ++ show t2 ++ "}"
    show (Contraction t1 l t2) = show t1 ++ " - { " ++ l ++ ": " ++ show t2 ++ "}"

substituteType :: String -> Type -> Type -> Type
substituteType var t = sub
  where
    sub Int = Int
    sub String = String
    sub (Parameter p)
        | var == p = t
        | otherwise = Parameter p
    sub (Arrow t1 t2) = Arrow (sub t1) (sub t2)
    sub (Record m) = Record $ fmap sub m
    sub (ForAll (l, k) t')
        | var == l = ForAll (l, k) t'
        | otherwise = ForAll (l, k) $ sub t'
    sub (Extension t1 l t2) = Extension (sub t1) l (sub t2)
    sub (Contraction t1 l t2) = Contraction (sub t1) l (sub t2)

typeParameters :: Type -> [String]
typeParameters Int = []
typeParameters String = []
typeParameters (Parameter p) = [p]
typeParameters (Arrow t1 t2) = typeParameters t1 ++ typeParameters t2
typeParameters (Record m) = concatMap typeParameters (Map.elems m)
typeParameters (ForAll (l, _) t') = l : typeParameters t'
typeParameters (Extension t1 _ t2) = typeParameters t1 ++ typeParameters t2
typeParameters (Contraction t1 _ t2) = typeParameters t1 ++ typeParameters t2

typeKinds :: Type -> [Kind]
typeKinds Int = []
typeKinds String = []
typeKinds (Parameter _) = [Universal]
typeKinds (Arrow t1 t2) = typeKinds t1 ++ typeKinds t2
typeKinds (Record m) = concatMap typeKinds (Map.elems m)
typeKinds (ForAll (_, k) t') = k : typeKinds t'
typeKinds (Extension t1 _ t2) = typeKinds t1 ++ typeKinds t2
typeKinds (Contraction t1 _ t2) = typeKinds t1 ++ typeKinds t2

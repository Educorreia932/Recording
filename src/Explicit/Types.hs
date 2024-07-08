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
    show (RecordKind m1 m2) = "{{ " ++ showFields m1 ++ "|| " ++ showFields m2 ++ "}}"
      where
        showFields x =
            intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) $ Map.toAscList x)
                ++ if not (null x) then " " else ""

type KindedType = (String, Kind)

data TypeModification = TypeContraction | TypeExtension deriving (Eq)

data Type
    = Int
    | String
    | Parameter String
    | Arrow Type Type
    | Record Fields
    | ForAll KindedType Type
    | Contraction Type String Type
    | Extension Type String Type
    deriving (Eq, Ord)

instance Show Type where
    show Int = "Int"
    show String = "String"
    show (Parameter p) = p
    show (Arrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show (ForAll (t, k) t') = "∀" ++ t ++ "::" ++ show k ++ "." ++ show t'
    show (Record m) = "{ " ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) $ Map.toAscList m) ++ " }"
    show (Contraction t1 l t2) = show t1 ++ " - { " ++ l ++ ": " ++ show t2 ++ "}"
    show (Extension t1 l t2) = show t1 ++ " + { " ++ l ++ ": " ++ show t2 ++ "}"

-- Retrieves all (type, kind) pairs from a polymorphic type
typeParameters :: Type -> [KindedType]
typeParameters (ForAll (l, k) t') = (l, k) : typeParameters t'
typeParameters _ = []

-- Retrieves the last monomorphic type from a polymorphic type
concreteType :: Type -> Type
concreteType (ForAll _ t) = concreteType t
concreteType t = t

typeKinds :: Type -> [Kind]
typeKinds Int = []
typeKinds String = []
typeKinds (Parameter _) = [Universal]
typeKinds (Arrow t1 t2) = typeKinds t1 ++ typeKinds t2
typeKinds (Record m) = concatMap typeKinds (Map.elems m)
typeKinds (ForAll (_, k) t') = k : typeKinds t'
typeKinds (Extension t1 _ t2) = typeKinds t1 ++ typeKinds t2
typeKinds (Contraction t1 _ t2) = typeKinds t1 ++ typeKinds t2

root :: Type -> Type
root (Extension t _ _) = root t
root (Contraction t _ _) = root t
root t = t

contractions :: Type -> Map.Map String Type
contractions (Contraction t l t') = Map.insert l t' $ contractions t
contractions _ = Map.empty

extensions :: Type -> Map.Map String Type
extensions (Extension t l t') = Map.insert l t' $ extensions t
extensions _ = Map.empty

typeModifications :: Type -> [(TypeModification, String, Type)]
typeModifications (Contraction t l t') = (TypeContraction, l, t') : typeModifications t
typeModifications (Extension t l t') = (TypeExtension, l, t') : typeModifications t
typeModifications _ = []

replaceRoot :: Type -> Type -> Type
replaceRoot (Contraction x l t') x' = Contraction (replaceRoot x x') l t'
replaceRoot (Extension x l t') x' = Extension (replaceRoot x x') l t'
replaceRoot _ t = t

removeTypeModification :: Type -> (TypeModification, String) -> Type
removeTypeModification (Contraction t l t') (TypeContraction, l')
    | l == l' = t
    | otherwise = Contraction (removeTypeModification t (TypeContraction, l')) l t'
removeTypeModification (Extension t l t') (TypeExtension, l')
    | l == l' = t
    | otherwise = Extension (removeTypeModification t (TypeExtension, l')) l t'
removeTypeModification t _ = t

-- | Normalizes a type by removing all contractions and extensions
rewrite :: Type -> Type
rewrite (Contraction t1@(Record r) l t2) = case Map.lookup l r of
    Just _ -> Record $ Map.delete l r
    Nothing -> Contraction t1 l t2
rewrite (Extension t1@(Record r) l t2)
    | null r = Record $ Map.singleton l t2
    | otherwise = case Map.lookup l r of
        Just _ -> Extension t1 l t2
        Nothing -> Record $ Map.insert l t2 r
rewrite (Contraction t1 l t2) = case rewrite t1 of
    r@(Record _) -> rewrite $ Contraction r l t2
    t -> Contraction t l t2
rewrite (Extension t1 l t2) = case rewrite t1 of
    r@(Record _) -> rewrite $ Extension r l t2
    t -> Extension t l t2
rewrite t = t

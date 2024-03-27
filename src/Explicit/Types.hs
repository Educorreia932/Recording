module Explicit.Types where

import Control.Lens
import Control.Lens.Fold
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Set qualified as Set

data Kind
    = Universal
    | RecordKind (Map.Map String Type)
    deriving (Eq, Ord)

instance Show Kind where
    show Universal = "U"
    show (RecordKind m) = "{{ " ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) $ Map.toAscList m) ++ " }}"

type KindedType = (String, Kind)

data Type
    = Int
    | String
    | Parameter String
    | Arrow Type Type
    | Record (Map.Map String Type)
    | ForAll KindedType Type
    deriving (Eq, Ord)

instance Show Type where
    show Int = "Int"
    show String = "String"
    show (Parameter p) = p
    show (Arrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show (ForAll (t, k) t') = "∀" ++ t ++ "::" ++ show k ++ "." ++ show t'
    show (Record m) = "{ " ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) $ Map.toAscList m) ++ " }"

-- Retrieves all (type, kind) pairs from a polymorphic type
typeParameters :: Type -> [KindedType]
typeParameters (ForAll (l, k) t') = (l, k) : typeParameters t'
typeParameters _ = []

-- Retrieves the last monomorphic type from a polymorphic type
concreteType :: Type -> Type
concreteType (ForAll _ t) = concreteType t
concreteType t = t



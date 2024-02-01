module Explicit.Types where

import Data.List (intercalate)
import Data.Map qualified as Map

data Kind
    = Universal
    | RecordKind (Map.Map String Type)
    deriving (Eq)

instance Show Kind where
    show Universal = "U"
    show (RecordKind m) = "{{ " ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) $ Map.toAscList m) ++ " }}"

data Type
    = Int
    | Parameter String
    | Arrow Type Type
    | Record (Map.Map String Type)
    | ForAll String Kind Type

instance Show Type where
    show Int = "Int"
    show (Parameter p) = p
    show (Arrow t1 t2) = show t1 ++ " -> " ++ show t2
    show (ForAll p k t) = "∀" ++ p ++ "::" ++ show k ++ "." ++ show t
    show (Record m) = "{ " ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) $ Map.toAscList m) ++ " }"

instance Eq Type where
    Int == Int = True
    (Parameter a) == (Parameter b) = a == b
    (ForAll p1 k1 t1) == (ForAll p2 k2 t2) = k1 == k2 && t1 == substituteType p2 (Parameter p1) t2
    _ == _ = False

substituteType :: String -> Type -> Type -> Type
substituteType var t = sub
  where
    sub Int = Int
    sub (Parameter p)
        | var == p = t
        | otherwise = Parameter p
    sub (Arrow t1 t2) = Arrow (sub t1) (sub t2)
    sub (Record m) = Record $ fmap sub m
    sub (ForAll p k t')
        | var == p = ForAll p k t'
        | otherwise = ForAll p k $ sub t'

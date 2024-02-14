module Explicit.Terms where

import Data.List (intercalate)
import Data.Map.Ordered qualified as Map
import Explicit.Types qualified as T

data Expression
    = Literal Int
    | String String
    | Variable String [T.Type]
    | Abstraction String T.Type Expression
    | Application Expression Expression
    | Poly Expression T.Type
    | Let String T.Type Expression Expression
    | ERecord (Map.OMap String Expression)
    | Dot Expression T.Type String
    | Modify Expression Int Expression
    deriving (Eq)

instance Show Expression where
    show (Literal a) = show a
    show (Variable x t) = x ++ concatMap (\v -> " " ++ show v) t
    show (String s) = show s
    show (Abstraction x t e2) = "λ" ++ x ++ ":" ++ show t ++ " -> " ++ show e2
    show (Application e1 e2) = "(" ++ show e1 ++ ") " ++ show e2
    show (Poly e t) = "Poly(" ++ show e ++ "): " ++ show t
    show (Let x t e1 e2) = "let " ++ x ++ " : " ++ show t ++ " = " ++ show e1 ++ " in " ++ show e2
    show (ERecord m) = "{ " ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) $ Map.toAscList m) ++ " }"
    show (Dot e t x) = show e ++ "." ++ x ++ ":" ++ show t
    show (Modify e1 i e2) = "modify(" ++ show e1 ++ ", " ++ show i ++ ", " ++ show e2 ++ ")"

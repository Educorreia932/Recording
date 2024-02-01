module Explicit.Terms where

import Data.List (intercalate)
import Data.Map.Ordered qualified as Map
import Explicit.Types

data Expression
    = Literal Int
    | Variable String
    | Abstraction String Type Expression
    | Application Expression Expression
    | Poly Expression Type
    | Let String Type Expression Expression
    | ERecord (Map.OMap String Expression)
    | Dot Expression Type String
    | Modify Expression Int Expression
    deriving (Eq)

instance Show Expression where
    show (Literal a) = show a
    show (Variable x) = x
    show (Abstraction x t e2) = "λ" ++ x ++ ":" ++ show t ++ "." ++ show e2
    show (Application e1 e2) = "(" ++ show e1 ++ ") " ++ show e2
    show (Poly e t) = "Poly(" ++ show e ++ "): " ++ show t 
    show (Let x t e1 e2) = "let " ++ x ++ " : " ++ show t ++ " = " ++ show e1 ++ " in " ++ show e2
    show (ERecord m) = "{ " ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) $ Map.toAscList m) ++ " }"
    show (Dot e t x) = show e ++ ":" ++ show t ++ "." ++ x
    show (Modify e1 i e2) = "modify(" ++ show e1 ++ ", " ++ show i ++ ", " ++ show e2 ++ ")"

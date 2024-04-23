module Implicit.Terms where

import Data.List (intercalate)

import Data.Map qualified as Map

type Label = String

data Expression
    = Literal Int
    | String String
    | Variable String
    | Abstraction String Expression
    | Application Expression Expression
    | Let String Expression Expression
    | Record (Map.Map Label Expression)
    | Dot Expression Label
    | Modify Expression Label Expression
    deriving (Eq)

instance Show Expression where
    show (Literal a) = show a
    show (Variable x) = x
    show (String s) = show s
    show (Abstraction x e2) = "λ" ++ x ++ " -> " ++ show e2
    show (Application e1 e2) = "(" ++ show e1 ++ ") " ++ show e2
    show (Let x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2
    show (Record m) = "{ " ++ intercalate ", " (map (\(k, v) -> k ++ " = " ++ show v) $ Map.toAscList m) ++ " }"
    show (Dot e x) = "(" ++ show e ++ ")." ++ x
    show (Modify e1 l e2) = "modify(" ++ show e1 ++ ", " ++ l ++ ", " ++ show e2 ++ ")"
module Implementation.Terms where

import Data.List (intercalate)
import Data.Map.Ordered qualified as Map

type Index = Either Int String

data Expression
    = Literal Int
    | String String
    | Variable String
    | Abstraction String Expression
    | Application Expression Expression
    | Let String Expression Expression
    | Record (Map.OMap String Expression)
    | IndexExpression Expression Index
    | IndexAbstraction String Expression
    | IndexApplication Expression Index
    | Modify Expression Index Expression
    deriving (Eq)

instance Show Expression where
    show :: Expression -> String
    show (Literal a) = show a
    show (String s) = show s
    show (Variable x) = x
    show (Abstraction x e2) = "λ" ++ x ++ " -> " ++ show e2
    show (Application e1 e2) = "(" ++ show e1 ++ ") " ++ show e2
    show (Let x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2
    show (Record m) = "{ " ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) $ Map.toAscList m) ++ " }"
    show (IndexExpression e i) = show e ++ "[" ++ either show id i ++ "]"
    show (IndexAbstraction i e) = "λ" ++ i ++ " -> " ++ show e
    show (IndexApplication e i) = "(" ++ show e ++ ") " ++ either show show i
    show (Modify e1 i e2) = "modify(" ++ show e1 ++ ", " ++ show i ++ ", " ++ show e2 ++ ")"

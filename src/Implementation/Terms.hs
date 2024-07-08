module Implementation.Terms where

import Data.List (intercalate)

type Label = String

type Index = Either Int (String, Int)

data Expression
    = Literal Int
    | String String
    | Variable String
    | Abstraction String Expression
    | Application Expression Expression
    | Let String Expression Expression
    | Record [Expression]
    | IndexExpression Expression Index
    | IndexAbstraction Label Expression
    | IndexApplication Expression Index
    | Modify Expression Index Expression
    | Contraction Expression Index
    | Extend Expression Index Expression
    deriving (Eq)

showIndex :: Index -> String
showIndex (Left i) = show i
showIndex (Right (s, i))
    | i == 0 = s
    | i > 0 = "(" ++ s ++ " + " ++ show i ++ ")"
    | otherwise = "(" ++ s ++ " - " ++ show (abs i) ++ ")"

instance Show Expression where
    show :: Expression -> String
    show (Literal a) = show a
    show (String s) = show s
    show (Variable x) = x
    show (Abstraction x e2) = "λ" ++ x ++ " -> " ++ show e2
    show (Application e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ") "
    show (Let x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2
    show (Record m) = "{ " ++ intercalate ", " (map show m) ++ " }"
    show (IndexExpression e i) = show e ++ "[" ++ showIndex i ++ "]"
    show (IndexAbstraction i e) = "λ" ++ i ++ " -> " ++ show e
    show (IndexApplication e i) = "(" ++ show e ++ " " ++ showIndex i ++ ") "
    show (Modify e1 i e2) = "modify(" ++ show e1 ++ ", " ++ showIndex i ++ ", " ++ show e2 ++ ")"
    show (Contraction e i) = show e ++ " \\\\ " ++ showIndex i
    show (Extend e1 i e2) = "extend(" ++ show e1 ++ ", " ++ showIndex i ++ ", " ++ show e2 ++ ")"

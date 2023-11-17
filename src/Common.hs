module Common where

type VariableIdentifier = String

data Token = LeftParentheses | RightParenteses | Lambda | Dot | Word String | Number Int deriving (Eq, Show, Read)

data Expression
    = Application Expression Expression
    | Abstraction VariableIdentifier Expression
    | Variable VariableIdentifier
    | Literal Int
    deriving (Eq)

instance Show Expression where
    show :: Expression -> String
    show (Application e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abstraction x e2) = "λ" ++ x ++ "." ++ show e2
    show (Variable x) = x
    show (Literal a) = show a

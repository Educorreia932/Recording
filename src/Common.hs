module Common where

type VariableIdentifier = String

data Expression
    = Application Expression Expression
    | Abstraction VariableIdentifier Expression -- TODO: Is it possible to replace Char with Variable?
    | Variable VariableIdentifier
    | Literal Int       
    deriving (Eq)

instance Show Expression where    
    show :: Expression -> String
    show (Application e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abstraction x e2) = "λ" ++ x ++ "." ++ show e2
    show (Variable x) = x
    show (Literal a) = show a 
    
module Implicit.Terms where

import Data.List (intercalate)
import Data.Map qualified as Map
import Prettyprinter
import Prelude hiding ((<>))
import Pretty 

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
    | Contract Expression Label
    | Extend Expression Label Expression
    deriving (Eq)

instance Show Expression where
    show (Literal a) = show a
    show (Variable x) = x
    show (String s) = show s
    show (Abstraction x e2) = "λ" ++ x ++ " -> " ++ show e2
    show (Application e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ") "
    show (Let x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2
    show (Record m) = "{ " ++ intercalate ", " (map (\(k, v) -> k ++ " = " ++ show v) $ Map.toAscList m) ++ " }"
    show (Dot e x) = show e ++ " . " ++ x
    show (Modify e1 l e2) = "modify(" ++ show e1 ++ ", " ++ l ++ ", " ++ show e2 ++ ")"
    show (Contract e l) = show e ++ " \\\\ " ++ l
    show (Extend e1 l e2) = "extend(" ++ show e1 ++ ", " ++ l ++ ", " ++ show e2 ++ ")"

instance Pretty Expression where
    pretty (Literal a) = pretty a
    pretty (Variable x) = pretty x
    pretty (String s) = pretty "\"" <> pretty s <> pretty "\""
    pretty (Abstraction x e2) = pretty "λ" <> pretty x <> pretty " → " <> pretty e2
    pretty (Application e1 e2) = parens (pretty e1 <> pretty " " <> pretty e2)
    pretty (Let x e1 e2) = pretty "let " <> pretty x <> pretty " = " <> pretty e1 <> pretty " in " <> pretty e2
    pretty (Record m) =
        pretty "{ "
            <> hcat
                ( punctuate
                    (pretty ", ")
                    (map (\(k, v) -> pretty k <> pretty " = " <> pretty v) $ Map.toAscList m)
                )
            <> pretty " }"
    pretty (Dot e x) = pretty e <> pretty " . " <> pretty x
    pretty (Modify e1 l e2) = pretty "modify" <> parens (pretty e1 <> pretty ", " <> pretty l <> pretty ", " <> pretty e2)
    pretty (Contract e l) = pretty e <> pretty " ⑊ " <> pretty l
    pretty (Extend e1 l e2) = pretty "extend" <> parens (pretty e1 <> pretty ", " <> pretty l <> pretty ", " <> pretty e2)

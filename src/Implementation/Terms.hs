module Implementation.Terms where

import Data.List (intercalate)
import Pretty
import Prettyprinter
import Prelude hiding ((<>))

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

instance Pretty Expression where
    pretty (Literal a) = pretty a
    pretty (String s) = pretty s
    pretty (Variable x) = pretty x
    pretty (Abstraction x e2) = pretty "λ" <> pretty x <> pretty " → " <> pretty e2
    pretty (Application e1 e2) = parens (pretty e1 <> pretty " " <> pretty e2)
    pretty (Let x e1 e2) = pretty "let " <> pretty x <> pretty " = " <> pretty e1 <> pretty " in " <> pretty e2
    pretty (Record m) =
        pretty "{ "
            <> hcat
                ( punctuate
                    (pretty ", ")
                    (map pretty m)
                )
            <> pretty " }"
    pretty (IndexExpression e i) = pretty e <> pretty "[" <> pretty (showIndex i) <> pretty "]"
    pretty (IndexAbstraction i e) = pretty "λ" <> pretty i <> pretty " → " <> pretty e
    pretty (IndexApplication e i) = parens (pretty e <> pretty " " <> pretty (showIndex i))
    pretty (Modify e1 i e2) = pretty "modify" <> parens (pretty e1 <> pretty ", " <> pretty (showIndex i) <> pretty ", " <> pretty e2)
    pretty (Contraction e i) = pretty e <> pretty " \\\\ " <> pretty (showIndex i)
    pretty (Extend e1 i e2) = pretty "extend" <> parens (pretty e1 <> pretty ", " <> pretty (showIndex i) <> pretty ", " <> pretty e2)

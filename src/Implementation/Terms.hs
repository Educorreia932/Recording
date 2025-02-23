{-# OPTIONS_GHC -Wno-orphans #-}

module Implementation.Terms where

import Pretty (dbackslash, lambda, rarrow)
import Prettyprinter
import Prelude hiding ((<>))

type Label = String

type Index = Either Int (String, Int)

data Expression
    = Literal Int
    | String String
    | Variable String
    | List [Expression]
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
    deriving (Eq, Show)

instance Pretty Index where
    pretty (Left i) = pretty i
    pretty (Right (s, i))
        | i == 0 = pretty s
        | i > 0 = parens $ pretty s <+> pretty "+" <+> pretty i
        | otherwise = parens $ pretty s <+> pretty "-" <+> pretty (abs i)

instance Pretty Expression where
    pretty (Literal a) = pretty a
    pretty (String s) = dquotes $ pretty s
    pretty (Variable x) = pretty x
    pretty (List l) = list $ map pretty l
    pretty (Abstraction x e2) = lambda <> pretty x <+> pretty e2
    pretty (Application e1 e2) =
        pretty e1 <+> case e2 of
            Application _ _ -> parens $ pretty e2
            _ -> pretty e2
    pretty (Let x e1 e2) =
        align $
            pretty "let"
                <+> pretty x
                <+> equals
                <+> pretty e1
                <> group (flatAlt (hardline <> space) (pretty " "))
                <> pretty "in"
                <+> pretty e2
    pretty (Record m) =
        pretty "{ "
            <> hcat
                ( punctuate
                    (pretty ", ")
                    (map pretty m)
                )
            <> pretty " }"
    pretty (IndexExpression e i) = pretty e <> brackets (pretty i)
    pretty (IndexAbstraction i e) = lambda <> pretty i <+> rarrow <+> pretty e
    pretty (IndexApplication e i) = pretty e <+> pretty i
    pretty (Modify e1 i e2) = prettyFunction "modify" e1 i e2
    pretty (Contraction e i) = pretty e <+> dbackslash <+> pretty i
    pretty (Extend e1 i e2) = prettyFunction "extend" e1 i e2

prettyFunction :: (Pretty a2, Pretty a3, Pretty a4) => String -> a2 -> a3 -> a4 -> Doc ann
prettyFunction name e1 i e2 = pretty name <> parens (pretty e1 <> comma <+> pretty i <> comma <+> pretty e2)
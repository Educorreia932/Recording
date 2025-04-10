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

collectLets :: Expression -> ([(String, Expression)], Expression)
collectLets (Let x e1 (Let y e2 e3)) =
    let (bindings, body) = collectLets (Let y e2 e3)
     in ((x, e1) : bindings, body)
collectLets (Let x e1 e2) = ([(x, e1)], e2)
collectLets expr = ([], expr)

instance Pretty Expression where
    pretty (Literal a) = pretty a
    pretty (String s) = dquotes $ pretty s
    pretty (Variable x) = pretty x
    pretty (List l) = list $ map pretty l
    pretty (Abstraction x e2) = lambda <> pretty x <+> rarrow <+> pretty e2
    pretty (Application e1 e2) =
        pretty e1 <+> case e2 of
            Application _ _ -> parens $ pretty e2
            _ -> pretty e2
    pretty (Let x e1 e2) =
        group $
            vsep
                [ pretty "let"
                    <> hardline
                    <> indent 2 (vsep (map prettyAssign ((x, e1) : bindings)))
                , pretty "in"
                    <> hardline
                    <> indent 2 (pretty body)
                ]
      where
        prettyAssign (x', e) =
            pretty x'
                <+> equals
                <+> pretty e
        (bindings, body) = collectLets e2
    pretty (Record m) =
        encloseSep
            (lbrace <> space)
            (space <> rbrace)
            (comma <> space)
            (map pretty m)
    pretty (IndexExpression e i) = pretty e <> brackets (pretty i)
    pretty (IndexAbstraction i e) = lambda <> pretty i <+> rarrow <+> pretty e
    pretty (IndexApplication e i) = pretty e <+> pretty i
    pretty (Modify e1 i e2) = prettyFunction "modify" e1 i e2
    pretty (Contraction e i) = pretty e <+> dbackslash <+> pretty i
    pretty (Extend e1 i e2) = prettyFunction "extend" e1 i e2

prettyFunction :: (Pretty a2, Pretty a3, Pretty a4) => String -> a2 -> a3 -> a4 -> Doc ann
prettyFunction name e1 i e2 = pretty name <> parens (pretty e1 <> comma <+> pretty i <> comma <+> pretty e2)

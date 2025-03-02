module Explicit.Terms where

import Data.Map qualified as Map
import Explicit.Types qualified as T
import Pretty
import Prettyprinter
import Prelude hiding ((<>))

type Label = String

data Expression
    = Literal Int
    | String String
    | Variable String [T.Type]
    | List [Expression]
    | Abstraction String T.Type Expression
    | Application Expression Expression
    | Poly Expression T.Type
    | Let String T.Type Expression Expression
    | Record (Map.Map Label Expression)
    | Dot Expression T.Type Label
    | Modify Expression T.Type Label Expression
    | Contract Expression T.Type Label
    | Extend Expression T.Type Label Expression
    deriving (Eq, Show)

collectLets :: Expression -> ([(String, T.Type, Expression)], Expression)
collectLets (Let x t e1 (Let y t' e2 e3)) =
    let (bindings, body) = collectLets (Let y t' e2 e3)
     in ((x, t, e1) : bindings, body)
collectLets (Let x t e1 e2) = ([(x, t, e1)], e2)
collectLets expr = ([], expr)

instance Pretty Expression where
    pretty (Literal a) = pretty a
    pretty (String s) = dquotes $ pretty s
    pretty (Variable x t) = pretty x <> (if null t then mempty else space) <> hsep (map pretty t)
    pretty (List l) = list $ map pretty l
    pretty (Abstraction x t e2) = lambda <> pretty x <> colon <+> pretty t <+> rarrow <+> pretty e2
    pretty (Application e1 e2) =
        pretty e1 <+> case e2 of
            Application _ _ -> parens $ pretty e2
            _ -> pretty e2
    pretty (Poly e _) = pretty "Poly" <> parens (pretty e)
    pretty (Let x t e1 e2) =
        group $
            vsep
                [ pretty "let"
                    <> hardline
                    <> indent 2 (vsep (map prettyAssign ((x, t, e1) : bindings)))
                , pretty "in"
                    <> hardline
                    <> indent 2 (pretty body)
                ]
      where
        prettyAssign (x', t', e) =
            pretty x'
                <> colon
                <+> pretty t'
                <+> equals
                <+> pretty e
        (bindings, body) = collectLets e2
    pretty (Record m) =
        braces $
            hcat (punctuate comma (map prettyField (Map.toAscList m)))
      where
        prettyField (k, v) = pretty k <+> equals <+> pretty v
    pretty (Dot e t x) = parens (pretty e <> colon <+> pretty t) <> dot <> pretty x
    pretty (Modify e1 t l e2) = prettyFunction "modify" e1 t l e2
    pretty (Contract e t l) = pretty e <> colon <+> pretty t <+> dbackslash <+> pretty l
    pretty (Extend e1 t l e2) = prettyFunction "extend" e1 t l e2

prettyFunction :: (Pretty a2, Pretty a3, Pretty a4, Pretty a5) => String -> a2 -> a3 -> a4 -> a5 -> Doc ann
prettyFunction name e1 t l e2 = pretty name <> parens (pretty e1 <> colon <+> pretty t <> comma <+> pretty l <> comma <+> pretty e2)

module Explicit.Terms where

import Data.Map qualified as Map
import Explicit.Types qualified as T
import Prettyprinter
import Prelude hiding ((<>))
import Pretty (rarrow, dbackslash)

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

instance Pretty Expression where
    pretty (Literal a) = pretty a
    pretty (Variable x t) = pretty x <+> hsep (map pretty t)
    pretty (String s) = dquotes $ pretty s
    pretty (List l) = list $ map pretty l
    pretty (Abstraction x t e2) = pretty "λ" <> pretty x <> comma <+> pretty t <+> rarrow <+> pretty e2
    pretty (Application e1 e2) =
        pretty e1 <+> case e2 of
            Application _ _ -> parens $ pretty e2
            _ -> pretty e2
    pretty (Poly e _) = pretty "Poly" <> parens (pretty e)
    pretty (Let x t e1 e2) = align $ pretty "let" <+> pretty x <> colon <+> pretty t <+> equals <+> pretty e1 <> softline <> pretty "in" <+> pretty e2
    pretty (Record m) =
        braces $
            space
                <> hcat
                    ( punctuate
                        comma
                        (map (\(k, v) -> pretty k <+> equals <+> pretty v) $ Map.toAscList m)
                    )
                <> space
    pretty (Dot e t x) = parens (pretty e <> colon <+> pretty t) <> dot <> pretty x
    pretty (Modify e1 t l e2) = prettyFunction "modify" e1 t l e2
    pretty (Contract e t l) = pretty e <> colon <+> pretty t <+> dbackslash <+> pretty l
    pretty (Extend e1 t l e2) = prettyFunction "extend" e1 t l e2

prettyFunction :: (Pretty a2, Pretty a3, Pretty a4, Pretty a5) => String -> a2 -> a3 -> a4 -> a5 -> Doc ann
prettyFunction name e1 t l e2 = pretty name <> parens (pretty e1 <+> pretty t <> comma <+> pretty l <+> pretty e2)  

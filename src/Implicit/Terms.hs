module Implicit.Terms where

import Data.Map qualified as Map
import Pretty (lambda, rarrow, dbackslash)
import Prettyprinter
import Prelude hiding ((<>))

type Label = String

data Expression
    = Literal Int
    | String String
    | Variable String
    | List [Expression]
    | Abstraction String Expression
    | Application Expression Expression
    | Let String Expression Expression
    | Record (Map.Map Label Expression)
    | Dot Expression Label
    | Modify Expression Label Expression
    | Contract Expression Label
    | Extend Expression Label Expression
    deriving (Eq, Show)

instance Pretty Expression where
    pretty (Literal a) = pretty a
    pretty (Variable x) = pretty x
    pretty (String s) = dquotes $ pretty s
    pretty (List es) = list $ map pretty es
    pretty (Abstraction x e2) = lambda <> pretty x <+> rarrow <+> pretty e2
    pretty (Application e1 e2) = parens (pretty e1 <> pretty " " <> pretty e2)
    pretty (Let x e1 e2) = pretty "let " <> pretty x <> pretty " = " <> pretty e1 <> softline <> pretty "in" <+> pretty e2
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
    pretty (Contract e l) = pretty e <+> dbackslash <+> pretty l
    pretty (Extend e1 l e2) = pretty "extend" <> parens (pretty e1 <> pretty ", " <> pretty l <> pretty ", " <> pretty e2)

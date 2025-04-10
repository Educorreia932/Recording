module Implicit.Terms where

import Data.Map qualified as Map
import Pretty (dbackslash, lambda, rarrow)
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

collectLets :: Expression -> ([(String, Expression)], Expression)
collectLets (Let x e1 (Let y e2 e3)) =
    let (bindings, body) = collectLets (Let y e2 e3)
     in ((x, e1) : bindings, body)
collectLets (Let x e1 e2) = ([(x, e1)], e2)
collectLets expr = ([], expr)

instance Pretty Expression where
    pretty (Literal a) = pretty a
    pretty (Variable x) = pretty x
    pretty (String s) = dquotes $ pretty s
    pretty (List es) = list $ map pretty es
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
            (map (\(k, v) -> pretty k <+> equals <+> pretty v) $ Map.toAscList m)
    pretty (Dot e x) = pretty e <> pretty " . " <> pretty x
    pretty (Modify e1 l e2) = pretty "modify" <> parens (pretty e1 <> pretty ", " <> pretty l <> pretty ", " <> pretty e2)
    pretty (Contract e l) = pretty e <+> dbackslash <+> pretty l
    pretty (Extend e1 l e2) = pretty "extend" <> parens (pretty e1 <> pretty ", " <> pretty l <> pretty ", " <> pretty e2)

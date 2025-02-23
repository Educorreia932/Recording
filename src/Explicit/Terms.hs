module Explicit.Terms where

import Data.List (intercalate)
import Data.Map qualified as Map
import Explicit.Types qualified as T
import Prettyprinter
import Prelude hiding ((<>))

type Label = String

data Expression
    = Literal Int
    | String String
    | Variable String [T.Type]
    | Abstraction String T.Type Expression
    | Application Expression Expression
    | Poly Expression T.Type
    | Let String T.Type Expression Expression
    | Record (Map.Map Label Expression)
    | Dot Expression T.Type Label
    | Modify Expression T.Type Label Expression
    | Contract Expression T.Type Label
    | Extend Expression T.Type Label Expression
    deriving (Eq)

instance Show Expression where
    show (Literal a) = show a
    show (Variable x t) = x ++ concatMap (\v -> " " ++ show v) t
    show (String s) = show s
    show (Abstraction x t e2) = "λ" ++ x ++ ": " ++ show t ++ " -> " ++ show e2
    show (Application e1 e2) = "(" ++ show e1 ++ ") " ++ show e2
    show (Poly e _) = "Poly(" ++ show e ++ ")"
    show (Let x t e1 e2) = "let " ++ x ++ ": " ++ show t ++ " = " ++ show e1 ++ " in " ++ show e2
    show (Record m) = "{ " ++ intercalate ", " (map (\(k, v) -> k ++ " = " ++ show v) $ Map.toAscList m) ++ " }"
    show (Dot e t x) = "(" ++ show e ++ " : " ++ show t ++ ")." ++ x
    show (Modify e1 t l e2) = "modify(" ++ show e1 ++ ": " ++ show t ++ ", " ++ l ++ ", " ++ show e2 ++ ")"
    show (Contract e t l) = show e ++ ": " ++ show t ++ " \\\\ " ++ l
    show (Extend e1 t l e2) = "extend(" ++ show e1 ++ ": " ++ show t ++ ", " ++ l ++ ", " ++ show e2 ++ ")"

instance Pretty Expression where
    pretty (Literal a) = pretty a
    pretty (Variable x t) = pretty x <> space <> hsep (map pretty t)
    pretty (String s) = dquotes $ pretty s
    pretty (Abstraction x t e2) = pretty "λ" <> pretty x <> pretty ": " <> pretty t <> pretty " -> " <> pretty e2
    pretty (Application e1 e2) =
        pretty e1 <> space <> case e2 of
            Application _ _ -> parens $ pretty e2
            _ -> pretty e2
    pretty (Poly e _) = pretty "Poly" <> parens (pretty e)
    pretty (Let x t e1 e2) = align $ pretty "let" <> space <> pretty x <> pretty ": " <> pretty t <> pretty " = " <> pretty e1 <> softline <> pretty " in " <> pretty e2
    pretty (Record m) =
        braces $
            space
                <> hcat
                    ( punctuate
                        comma
                        (map (\(k, v) -> pretty k <> space <> equals <> space <> pretty v) $ Map.toAscList m)
                    )
                <> space
    pretty (Dot e t x) = parens (pretty e <> pretty ": " <> pretty t) <> dot <> pretty x
    pretty (Modify e1 t l e2) = pretty "modify" <> parens (pretty e1 <> pretty ": " <> pretty t <> pretty ", " <> pretty l <> pretty ", " <> pretty e2)
    pretty (Contract e t l) = pretty e <> pretty ": " <> pretty t <> pretty " \\\\ " <> pretty l
    pretty (Extend e1 t l e2) = pretty "extend" <> parens (pretty e1 <> pretty ": " <> pretty t <> pretty ", " <> pretty l <> pretty ", " <> pretty e2)

module Pretty where

import Prettyprinter
import Prettyprinter.Render.String

dbraces :: Doc ann -> Doc ann
dbraces = braces . braces

lambda :: Doc ann
lambda = pretty "λ"

rarrow :: Doc ann
rarrow = pretty "->"

dbackslash :: Doc ann
dbackslash = pretty "\\\\"

forAll :: Doc ann
forAll = pretty "∀"

render :: Pretty a => a -> String
render s = renderString $ layoutSmart defaultLayoutOptions $ pretty s
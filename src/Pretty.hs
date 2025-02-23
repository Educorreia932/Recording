module Pretty where

import Prettyprinter

dbraces :: Doc ann -> Doc ann
dbraces = braces . braces

lambda :: Doc ann
lambda = pretty "λ"

rarrow :: Doc ann
rarrow = pretty "->"

equals :: Doc ann
equals = pretty "="

dbackslash :: Doc ann
dbackslash = pretty "\\\\"
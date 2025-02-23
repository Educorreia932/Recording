module Pretty where

import Prettyprinter (Doc, braces)

doubleBraces :: Doc ann -> Doc ann
doubleBraces = braces . braces
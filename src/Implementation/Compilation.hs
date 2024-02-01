module Implementation.Compilation where

import Explicit.Terms qualified as E
import Explicit.Types
import Implementation.Terms qualified as I

type IndexAssignment = [(String, Int)]
type TypeAssignment = [(String, Type)]

compile :: IndexAssignment -> TypeAssignment -> E.Expression -> I.Expression
compile l t = comp
 where
  comp (E.Variable x) =
    let
     in I.Variable x
  comp (E.Literal i) = I.Literal i
  comp (E.Abstraction x t e) = I.Abstraction x (comp e)
  comp (E.Application e1 e2) = I.Application (comp e1) (comp e2)
  comp (E.Dot e _ x) =
    let c = comp e
        index = Right "I"
     in I.IndexExpression c index
  comp (E.Poly e _) =
    let c = comp e
     in I.Abstraction "I" c
  comp _ = I.Literal (-1)

module Evaluator (evaluate) where

import Common (Expression (..), VariableIdentifier)

substitute :: VariableIdentifier -> Expression -> Expression -> Expression
substitute v e = sub
  where
    sub (Literal l) = Literal l
    sub (Variable v')
        | v == v' = e
        | otherwise = Variable v'
    sub (Abstraction v' e1)
        | v == v' = Abstraction v' e1
        | otherwise = Abstraction v' $ sub e1
    sub (Application e1 e2) = Application (sub e1) (sub e2)

evaluate :: Expression -> Expression
evaluate (Application fun arg) = case evaluate fun of
    -- Substitute the argument for the variable in the body of the abstraction
    Abstraction var body -> evaluate $ substitute var arg body
    other -> Application other arg
evaluate e = e
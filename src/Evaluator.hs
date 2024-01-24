module Evaluator (evaluate) where

import Common (Expression (..), VariableIdentifier)

freeVariables :: Expression -> [VariableIdentifier]
freeVariables (Literal _) = []
freeVariables (Variable v) = [v]
freeVariables (Abstraction v e) = filter (/= v) $ freeVariables e
freeVariables (Application e1 e2) = freeVariables e1 ++ freeVariables e2

disambiguate :: VariableIdentifier -> VariableIdentifier
disambiguate x = x ++ "'"

substitute :: VariableIdentifier -> Expression -> Expression -> Expression
substitute var e = sub
  where
    sub (Literal l) = Literal l
    sub (Variable v)
        | var == v = e
        | otherwise = Variable v
    sub (Abstraction v body)
        | var == v = Abstraction v body
        -- α-conversion
        | v `elem` freeVariables e =
            let v' = disambiguate v
                body' = substitute v (Variable v') body
             in Abstraction v' $ sub body'
        | otherwise = Abstraction v $ sub body
    sub (Application e1 e2) = Application (sub e1) (sub e2)

evaluate :: Expression -> Expression
evaluate (Application fun arg) = case evaluate fun of
    -- Substitute the argument for the variable in the body of the abstraction
    Abstraction var body -> evaluate $ substitute var arg body
    other -> Application other arg
evaluate e = e
